-module(ameo_h_pubsub).
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).

init(ReqIn, State) ->
  IdleTimeout = maps:get(idle_timeout, State, 60000),
  Opts = #{idle_timeout => IdleTimeout},
  {cowboy_websocket, ReqIn, State, Opts}.

websocket_init(State=#{close := true, close_msg := Msg}) ->
  {reply, [{text, Msg}, close], State};
websocket_init(State) ->
  {reply, {text, <<"{\"t\":\"init\"}">>}, State}.

websocket_handle({text, Text}, State) ->
    case jsx:decode(Text, [return_maps]) of
        Cmd=#{<<"t">> := <<"subunsub">>} ->
            Subs = maps:get(<<"subs">>, Cmd, []),
            Unsubs = maps:get(<<"unsubs">>, Cmd, []),

            UnsubRes = [unsubscribe(Unsub) || Unsub <- Unsubs],
            SubRes = [subscribe(Sub) || Sub <- Subs],

            Resp = jsx:encode(#{t => <<"subunsub">>, subs => SubRes, unsub => UnsubRes}),
            {reply, {text, Resp}, State};
        #{<<"t">> := <<"ping">>, <<"sn">> := Sn} ->
            {reply, {text, jsx:encode(#{t => <<"pong">>, sn => Sn})}, State};
        Cmd ->
            lager:warning("Unknown command in pubsub: ~p", [Cmd]),
            {stop, State}
    end;

websocket_handle({ping, 'PING'}, State) ->
    {ok, State};

websocket_handle(Frame, State) ->
    lager:warning("Unknown frame in pubsub: ~p", [Frame]),
    {stop, State}.

websocket_info({'EXIT', _Pid, _Reason}, State) ->
    {ok, State};
websocket_info({pubsub_msg, Msg}, State) ->
    Resp = jsx:encode(#{t => 'msg', data => Msg}),
    {reply, {text, Resp}, State}.

%% private
subscribe(Topic) when is_binary(Topic) ->
    cmd_reply_to_ws_reply(ameo:run_command(<<"SUBSCRIBE">>, [Topic])).

unsubscribe(Topic) when is_binary(Topic) ->
    cmd_reply_to_ws_reply(ameo:run_command(<<"UNSUBSCRIBE">>, [Topic])).

cmd_reply_to_ws_reply(Reply) ->
    case Reply of
        ok ->
            #{ok => true};
        {ok, _Partition} ->
            #{ok => true};
        {ok, _Partition, nil} ->
            #{ok => true, data => null};
        {ok, _Partition, Value} ->
            #{ok => true, data => Value};
        no_reply ->
            #{ok => true};
        {error, _Partition, {Code, Reason, _Details}} ->
            #{ok => false, error => Code, reason => Reason}
    end.
