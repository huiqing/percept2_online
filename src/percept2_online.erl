%% Feel free to use, reuse and abuse the code in this file.

-module(percept2_online).

%% API.
-export([start/0, stop/0]).

start() ->
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    ok = application:start(percept2_online),
    io:format("Point your browser at http://localhost:8080/ to use percept2_online.\n").


stop() ->
    error_logger:tty(false),
    ok = application:stop(crypto),
    ok = application:stop(ranch),
    ok = application:stop(cowboy),
    ok = application:stop(percept2_online),
    error_logger:tty(true).
