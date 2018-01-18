-module(ameo_channel).
-behaviour(gen_event).

-export([start_link/0, subscribe/2, unsubscribe/2, send/2, subscriber_count/1,
         stop/1]).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3,
         terminate/2]).
%% API

start_link() ->
    gen_event:start_link().

subscribe(Channel, Pid) ->
    gen_event:add_sup_handler(Channel, {ameo_channel, Pid}, [Pid]).

unsubscribe(Channel, Pid) ->
    gen_event:delete_handler(Channel, {ameo_channel, Pid}, [Pid]).

send(Channel, Event) ->
    gen_event:notify(Channel, Event).

subscriber_count(Channel) ->
    length(gen_event:which_handlers(Channel)).

stop(Channel) ->
    gen_event:stop(Channel).

-record(state, {pid}).

%% callbacks

init([Pid]) -> {ok, #state{pid=Pid}}.

handle_event(Msg, State=#state{pid=Pid}) ->
    Pid ! Msg,
    {ok, State}.

handle_call(Reason, State) ->
    lager:warning("ameo_channel: Unknown handle_call: ~p", [Reason]),
    {ok, ok, State}.

handle_info(Reason, State) ->
    lager:warning("ameo_channel: Unknown handle_info: ~p", [Reason]),
    {ok, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(Reason, #state{pid=Pid}) ->
    Pid ! {smc, {terminate, [{reason, Reason}]}},
    lager:debug("terminating channel ~p", [Reason]),
    ok.

