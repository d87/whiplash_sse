%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc whiplash_sse.

-module(whiplash_sse).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the whiplash_sse server.
start() ->
    whiplash_sse_deps:ensure(),
    ensure_started(crypto),
    application:start(whiplash_sse).


%% @spec stop() -> ok
%% @doc Stop the whiplash_sse server.
stop() ->
    application:stop(whiplash_sse).

stop(RemoteNode) ->
    rpc:call(RemoteNode, whiplash_sse, stop, []),
    rpc:call(RemoteNode, init, stop, []).