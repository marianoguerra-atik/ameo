-module(ameo_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
    VMaster = {ameo_vnode_master,
                  {riak_core_vnode_master, start_link, [ameo_vnode]},
                  permanent, 5000, worker, [riak_core_vnode_master]},

  MinPort = application:get_env(ameo, port_from, 6379),
  MaxPort = application:get_env(ameo, port_to, 6379),
  ListenerOpts = #{min_port => MinPort, max_port => MaxPort},
  ListenerSup = {edis_listener_sup, {edis_listener_sup, start_link, [ListenerOpts]},
                 permanent, 1000, supervisor, [edis_listener_sup]},
  ClientOpts = #{command_runner_mod => ameo},
  ClientSup = {edis_client_sup, {edis_client_sup, start_link, [ClientOpts]},
               permanent, 1000, supervisor, [edis_client_sup]},

    { ok, {{one_for_one, 5, 10}, [VMaster, ListenerSup, ClientSup]}}.
