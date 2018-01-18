-module(ameo_vnode).
-behaviour(riak_core_vnode).

-export([start_vnode/1,
         init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         encode_handoff_item/2,
         handle_overload_command/3,
         handle_overload_info/2,
         handle_coverage/4,
         handle_exit/3]).

-ignore_xref([
             start_vnode/1
             ]).

-record(state, {partition, table_id, table_name}).

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
    TableName = list_to_atom("ameo_" ++ integer_to_list(Partition)),
    TableId = ets:new(TableName, [set, {write_concurrency, false},
                                  {read_concurrency, false}]),

    {ok, #state{partition=Partition, table_id=TableId,
                table_name=TableName}}.

%% Sample command: respond to a ping
handle_command(ping, _Sender, State) ->
    {reply, {pong, State#state.partition}, State};

handle_command({cmd, Command, Args}, _Sender, State) ->
    Result = run_cmd(Command, Args, State),
    lager:info("~p ~p -> ~p", [Command, Args, Result]),
    {reply, Result, State};

handle_command(Message, _Sender, State) ->
    lager:warning("unhandled_command ~p", [Message]),
    {noreply, State}.

handle_handoff_command(_Message, _Sender, State) ->
    {noreply, State}.

handoff_starting(_TargetNode, State) ->
    {true, State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.

handle_handoff_data(_Data, State) ->
    {reply, ok, State}.

encode_handoff_item(_ObjectName, _ObjectValue) ->
    <<>>.

handle_overload_command(_, _, _) ->
    ok.

handle_overload_info(_, _Idx) ->
    ok.

is_empty(State) ->
    {true, State}.

delete(State) ->
    {ok, State}.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
    {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% private
run_cmd(<<"SET">>, [Key, Value], #state{table_id=TableId, partition=Partition}) ->
    ets:insert(TableId, {Key, Value}),
    {ok, Partition};
run_cmd(<<"GET">>, [Key], #state{table_id=TableId, partition=Partition}) ->
    case ets:lookup(TableId, Key) of
        [] ->
            {ok, Partition, nil};
        [{_, Value}] ->
            {ok, Partition, Value}
    end;
run_cmd(<<"DEL">>, [Key], #state{table_id=TableId, partition=Partition}) ->
    case ets:lookup(TableId, Key) of
        [] ->
            {ok, Partition, 0};
        [_Value] ->
            true = ets:delete(TableId, Key),
            {ok, Partition, 1}
    end.
