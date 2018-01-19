-module(ameo_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case ameo_sup:start_link() of
        {ok, Pid} ->
            ok = riak_core:register([{vnode_module, ameo_vnode}]),
            ok = riak_core_node_watcher:service_up(ameo, self()),
            setup_webserver(),

            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.

%% private
setup_webserver() ->
  Dispatch = cowboy_router:compile([
    {'_', [
        {"/pubsub", ameo_h_pubsub, #{}},
        {"/", cowboy_static, {priv_file, ameo, "ui/index.html"}},
        {"/ui/[...]", cowboy_static, {priv_dir, ameo, "ui"}}
      ]}
  ]),

  HttpPort = env(http_port, 8080),
  HttpAcceptors = env(http_acceptors, 100),
  HttpMaxConnections = env(http_max_connections, infinity),

  lager:info("Starting Web Server at ~p", [HttpPort]),

  {ok, _} = cowboy:start_clear(ameo_http_listener,
    [{port, HttpPort},
     {num_acceptors, HttpAcceptors},
     {max_connections, HttpMaxConnections}],
    #{env => #{dispatch => Dispatch}}).

env(Key, Default) -> application:get_env(ameo, Key, Default).
