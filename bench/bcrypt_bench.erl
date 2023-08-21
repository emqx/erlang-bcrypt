%% Copyright (c) 2023 EMQX
%% Distributed under the MIT license; see LICENSE for details.

%% @doc Very simple benchmark for bcrypt
-module(bcrypt_bench).

-define(PASSWORD, "password").

-export([measure/3, run/3]).

measure(Rounds, NProcs, NHashes) ->
    {ok, _} = application:ensure_all_started(bcrypt),
    {ok, Salt} = bcrypt:gen_salt(Rounds),
    {TotalTimeMcs, _} = timer:tc(?MODULE, run, [Salt, NProcs, NHashes]),
    TimeMcs = TotalTimeMcs / NProcs / NHashes,
    TimeMcs.

run(Salt, NProcs, NHashes) ->
    start_n(Salt, NProcs, NHashes),
    wait_n(NProcs).

start_n(Salt, NProcs, NHashes) when NProcs > 0 ->
    _ = start_link(Salt, NHashes),
    start_n(Salt, NProcs - 1, NHashes);
start_n(_, 0, _NHashes) ->
    ok.

wait_n(0) ->
    ok;
wait_n(N) ->
    receive
        done -> wait_n(N-1)
    end.

start_link(Salt, N) ->
    OwnerPid = self(),
    spawn_link(fun() -> run_single(Salt, OwnerPid, N) end).

run_single(_Salt, Pid, 0) ->
    Pid ! done;
run_single(Salt, Pid, N) when N > 0 ->
    _ = bcrypt:hashpw(?PASSWORD, Salt),
    run_single(Salt, Pid, N-1).
