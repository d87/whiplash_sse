%% @author Mochi Media <dev@mochimedia.com>
%% @copyright whiplash_sse Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the whiplash_sse application.

-module(whiplash_sse_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for whiplash_sse.
start(_Type, _StartArgs) ->
    whiplash_sse_deps:ensure(),
    whiplash_sse_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for whiplash_sse.
stop(_State) ->
    ok.
