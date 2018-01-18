-module(ameo).

-export([ping/0, run_command/2]).

-ignore_xref([ping/0, run_command/2]).

%% Public API

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(os:timestamp())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, ameo),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, ameo_vnode_master).

run_command(Command, Args) ->
    Key = key_for_command(Command, Args),
    Bucket = <<"default">>,
    Pid = self(),
    send_to_one({Bucket, Key}, {cmd, Command, Args, Pid}).

% private functions

key_for_command(<<"SET">>, [Key, _]) ->
    Key;
key_for_command(<<"GET">>, [Key]) ->
    Key;
key_for_command(<<"SUBSCRIBE">>, [Topic]) ->
    Topic;
key_for_command(<<"PUBLISH">>, [Topic, _]) ->
    Topic;
key_for_command(<<"DEL">>, [Key]) ->
    Key.

send_to_one(Key, Cmd) ->
    DocIdx = riak_core_util:chash_key(Key),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, ameo),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, Cmd,
         ameo_vnode_master).
