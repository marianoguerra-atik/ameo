%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @author Chad DePue <chad@inakanetworks.com>
%%% @copyright (C) 2011 InakaLabs SRL
%%% @doc Edis TCP listener supervisor
%%% @end
%%%-------------------------------------------------------------------
-module(edis_listener_sup).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').
-author('Chad DePue <chad@inakanetworks.com>').

-include("edis.hrl").

-behaviour(supervisor).

-export([start_link/1, init/1, reload/0]).

%% ====================================================================
%% External functions
%% ====================================================================
%% @doc  Starts the supervisor process
-spec start_link(map()) -> ignore | {error, term()} | {ok, pid()}.
start_link(Opts) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Opts).

%% @doc  Reloads configuration. Restarts the listeners
-spec reload() -> ok.
reload() ->
  true = exit(erlang:whereis(?MODULE), kill),
  ok.

%% ====================================================================
%% Server functions
%% ====================================================================
%% @hidden
-spec init(map()) -> {ok, {{one_for_one, 5, 10}, [supervisor:child_spec()]}}.
init(Opts) ->
  lager:info("Listener supervisor initialized"),
  MinPort = maps:get(port_from, Opts, 6379),
  MaxPort = maps:get(port_to, Opts, 6379),
  Listeners =
    [{list_to_atom("edis-listener-" ++ integer_to_list(I)),
      {edis_listener, start_link, [I]}, permanent, brutal_kill, worker, [edis_listener]}
     || I <- lists:seq(MinPort, MaxPort)],
  {ok, {{one_for_one, 5, 10}, Listeners}}.
