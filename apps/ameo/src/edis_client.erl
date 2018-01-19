%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @author Chad DePue <chad@inakanetworks.com>
%%% @copyright (C) 2011 InakaLabs SRL
%%% @doc edis client FSM
%%% @end
%%%-------------------------------------------------------------------
-module(edis_client).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').
-author('Chad DePue <chad@inakanetworks.com>').

-behaviour(gen_fsm).

-export([start_link/1, set_socket/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([socket/2, command_start/2, arg_size/2, command_name/2, argument/2]).
-export([disconnect/1]).

-include("edis.hrl").

-define(FSM_TIMEOUT, 60000).

-record(state, {socket            :: undefined | port(),
                peerport          :: undefined | pos_integer(),
                missing_args = 0  :: non_neg_integer(),
                next_arg_size     :: undefined | integer(),
                command_name      :: undefined | binary(),
                args = []         :: [binary()],
                buffer = <<>>     :: binary(),
                command_runner    :: undefined | atom()
               }).
-type state() :: #state{}.

%% ====================================================================
%% External functions
%% ====================================================================
%% -- General ---------------------------------------------------------
%% @doc Start the client
-spec start_link(map()) -> {ok, pid()}.
start_link(Opts) ->
  gen_fsm:start_link(?MODULE, Opts, []).

%% @doc Associates the client with the socket
-spec set_socket(pid(), port()) -> ok.
set_socket(Client, Socket) ->
  gen_fsm:send_event(Client, {socket_ready, Socket}).

%% @doc Stop the client
-spec disconnect(pid()) -> ok.
disconnect(Client) ->
  gen_fsm:send_event(Client, disconnect).

%% ====================================================================
%% Server functions
%% ====================================================================
%% @hidden
-spec init(map()) -> {ok, socket, state(), ?FSM_TIMEOUT}.
init(Opts=#{command_runner_mod := Mod}) ->
  lager:info("start edis_client ~p", [Opts]),
  {ok, socket, #state{command_runner=Mod}, ?FSM_TIMEOUT}.

%% ASYNC EVENTS -------------------------------------------------------
%% @hidden
-spec socket({socket_ready, port()} | timeout | term(), state()) -> {next_state, command_start, state(), hibernate} | {stop, timeout | {unexpected_event, term()}, state()}.
socket({socket_ready, Socket}, State) ->
  % Now we own the socket
  PeerPort =
    case inet:peername(Socket) of
      {ok, {_Ip, Port}} -> Port;
      Error -> Error
    end,
  lager:info("New Client: ~p ~n", [PeerPort]),
  ok = inet:setopts(Socket, [{active, once}, {packet, line}, binary]),
  _ = erlang:process_flag(trap_exit, true), %% We want to know even if it stops normally
  {next_state, command_start, State#state{socket   = Socket,
                                          peerport = PeerPort}, hibernate};
socket(timeout, State) ->
  lager:alert("Timeout~n", []),
  {stop, timeout, State};
socket(Other, State) ->
  lager:alert("Unexpected message: ~p\n", [Other]),
  {stop, {unexpected_event, Other}, State}.

%% @hidden
-spec command_start(term(), state()) -> {next_state, command_start, state(), hibernate} | {next_state, arg_size | argument, state()} | {stop, {unexpected_event, term()}, state()}.
command_start({data, <<"\r\n">>}, State) ->
  {next_state, command_start, State};
command_start({data, <<"\n">>}, State) ->
  {next_state, command_start, State};
command_start({data, <<"*", N/binary>>}, State) -> %% Unified Request Protocol
  {NArgs, "\r\n"} = string:to_integer(binary_to_list(N)),
  lager:debug("URP command - ~p args~n",[NArgs]),
  {next_state, arg_size, State#state{missing_args = NArgs, command_name = undefined, args = []}};
command_start({data, OldCmd}, State) ->
  [Command|Args] = binary:split(OldCmd, [<<" ">>, <<"\r\n">>], [global,trim]),
  lager:debug("Old protocol command ~p (~p args)~n",[Command, length(Args)]),
  case last_arg(edis_util:upper(Command)) of
    inlined ->
      lager:info("Run Command: ~p ~p", [edis_util:upper(Command), Args]),
      {next_state, command_start, State, hibernate};
    safe ->
      case lists:reverse(Args) of
        [LastArg | FirstArgs] ->
          case string:to_integer(binary_to_list(LastArg)) of
            {error, no_integer} ->
              ok = err(State, io_lib:format("lenght of last param expected for '~s'. ~s received instead", [Command, LastArg])),
              {next_state, command_start, State, hibernate};
            {ArgSize, _Rest} ->
              {next_state, argument, State#state{command_name   = Command,
                                                 args           = lists:reverse(FirstArgs),
                                                 missing_args   = 1,
                                                 buffer         = <<>>,
                                                 next_arg_size  = ArgSize}}
          end;
        [] ->
          run_command(edis_util:upper(Command), [], State),
          {next_state, command_start, State, hibernate}
      end
  end;
command_start(Event, State) ->
  lager:alert("Unexpected Event: ~p~n", [Event]),
  {stop, {unexpected_event, Event}, State}.

%% @hidden
-spec arg_size(term(), state()) -> {next_state, command_start, state(), hibernate} | {next_state, command_name | argument, state()} | {stop, {unexpected_event, term()}, state()}.
arg_size({data, <<"$", N/binary>>}, State) ->
  case string:to_integer(binary_to_list(N)) of
    {error, no_integer} ->
      ok = err(State, io_lib:format("lenght of next arg expected. ~s received instead", [N])),
      {next_state, command_start, State, hibernate};
    {ArgSize, _Rest} ->
      lager:debug("Arg Size: ~p ~n", [ArgSize]),
      {next_state, case State#state.command_name of
                     undefined -> command_name;
                     _Command -> argument
                   end, State#state{next_arg_size = ArgSize, buffer = <<>>}}
  end;
arg_size(Event, State) ->
    lager:alert("Unexpected Event: ~p~n", [Event]),
    {stop, {unexpected_event, Event}, State}.

%% @hidden
-spec command_name(term(), state()) -> {next_state, command_start, state(), hibernate} | {next_state, arg_size, state()} | {stop, {unexpected_event, term()}, state()}.
command_name({data, Data}, State = #state{next_arg_size = Size, missing_args = 1}) ->
  <<Command:Size/binary, _Rest/binary>> = Data,
  lager:debug("Command: ~p ~n", [Command]),
  run_command(edis_util:upper(Command), [], State),
  {next_state, command_start, State, hibernate};
command_name({data, Data}, State = #state{next_arg_size = Size, missing_args = MissingArgs}) ->
  <<Command:Size/binary, _Rest/binary>> = Data,
  lager:debug("Command: ~p ~n", [Command]),
  {next_state, arg_size, State#state{command_name = Command, missing_args = MissingArgs - 1}};
command_name(Event, State) ->
  lager:alert("Unexpected Event: ~p~n", [Event]),
  {stop, {unexpected_event, Event}, State}.

%% @hidden
-spec argument(term(), state()) -> {next_state, command_start, state(), hibernate} | {next_state, argument | arg_size, state()} | {stop, {unexpected_event, term()}, state()}.
argument({data, Data}, State = #state{buffer        = Buffer,
                                      next_arg_size = Size}) ->
  case <<Buffer/binary, Data/binary>> of
    <<Argument:Size/binary, _Rest/binary>> ->
      case State#state.missing_args of
        1 ->
          run_command(edis_util:upper(State#state.command_name),
                      lists:reverse([Argument|State#state.args]),
                      State),
          {next_state, command_start, State, hibernate};
        MissingArgs ->
          {next_state, arg_size, State#state{missing_args = MissingArgs - 1,
                                             args = [Argument | State#state.args]}}
      end;
    NewBuffer -> %% Not the whole argument yet, just an \r\n in the middle of it
      {next_state, argument, State#state{buffer = NewBuffer}}
  end;
argument(Event, State) ->
    lager:alert("Unexpected Event: ~p~n", [Event]),
    {stop, {unexpected_event, Event}, State}.

%% OTHER EVENTS -------------------------------------------------------
%% @hidden
-spec handle_event(X, atom(), state()) -> {stop, {atom(), unexpected_event, X}, state()}.
handle_event(Event, StateName, StateData) ->
  {stop, {StateName, unexpected_event, Event}, StateData}.

%% @hidden
-spec handle_sync_event(X, reference(), atom(), state()) -> {stop, {atom(), unexpected_event, X}, state()}.
handle_sync_event(Event, _From, StateName, StateData) ->
  {stop, {StateName, unexpected_event, Event}, StateData}.

%% @hidden
-spec handle_info(term(), atom(), state()) -> term().
handle_info({'EXIT', CmdRunner, Reason}, _StateName, State = #state{command_runner = CmdRunner}) ->
  lager:debug("Command runner stopped: ~p~n", [Reason]),
  {stop, Reason, State};
handle_info({tcp, Socket, Bin}, StateName, #state{socket = Socket,
                                                  peerport = PeerPort} = StateData) ->
  % Flow control: enable forwarding of next TCP message
  ok = inet:setopts(Socket, [{active, false}]),
  lager:debug("~p >> ~s", [PeerPort, Bin]),
  Result = ?MODULE:StateName({data, Bin}, StateData),
  ok = inet:setopts(Socket, [{active, once}]),
  Result;
handle_info({tcp_closed, Socket}, _StateName, #state{socket = Socket,
                                                     peerport = PeerPort} = StateData) ->
  lager:debug("Disconnected ~p.~n", [PeerPort]),
  {stop, normal, StateData};
handle_info({pubsub_msg, Msg}, StateName, StateData) ->
  send_value(Msg, StateData),
  {next_state, StateName, StateData};
handle_info(_Info, StateName, StateData) ->
  {next_state, StateName, StateData}.

%% @hidden
-spec terminate(term(), atom(), state()) -> ok.
terminate(normal, _StateName, #state{socket = Socket}) ->
  (catch gen_tcp:close(Socket)),
  ok;
terminate(Reason, _StateName, #state{socket = Socket}) ->
  lager:warning("Terminating client: ~p~n", [Reason]),
  (catch gen_tcp:close(Socket)),
  ok.

%% @hidden
-spec code_change(term(), atom(), state(), any()) -> {ok, atom(), state()}.
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%% private

run_command(<<"COMMAND">>, [], State) ->
    send("*0\r\n", State);
run_command(Command, Args, State=#state{command_runner=Mod}) ->
    Reply = Mod:run_command(Command, Args),
    SendRes = case Reply of
        {ok, _Partition} ->
                      send_ok(State);
        {ok, _Partition, nil} ->
                      send_nil(State);
        {ok, _Partition, Value} ->
                      send_value(Value, State);
        no_reply -> ok;
        {error, _Partition, {_Code, Reason, _Details}} ->
                      send_error(Reason, State)
    end,
    lager:info("~p ~p", [Reply, SendRes]),
    Reply.

send_ok(State) ->
    send_string("OK", State).

send_value(V, State) when is_list(V); is_binary(V) ->
    send(["+", V], State);
send_value(V, State) when is_integer(V) ->
    send([":", integer_to_list(V)], State).

send_string(V, State) ->
    send(["+", V], State).

send_nil(State) ->
    send("$-1", State).

send_error(V, State) ->
    send(["-ERR ", V], State).

send(Message, State) ->
    gen_tcp:send(State#state.socket, [Message, "\r\n"]).

err(State, Message) ->
    send_error(Message, State),
    ok.

last_arg(_) -> inlined.
