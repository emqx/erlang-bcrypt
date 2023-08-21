%% Copyright (c) 2011 Hunter Morris
%% Distributed under the MIT license; see LICENSE for details.
-module(bcrypt_nif_worker).
-author('Hunter Morris <huntermorris@gmail.com>').

-behaviour(gen_server).

-export([start_link/0]).
-export([gen_salt/0, gen_salt/1]).
-export([hashpw/2]).

%% gen_server
-export([init/1, code_change/3, terminate/2,
         handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {
          current_ctx_index :: pos_integer(),
          contexts :: tuple()
         }).

-define(DEFAULT_LOG_ROUNDS, 12).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

start_link() ->
    DefultBCryptQueueNum = erlang:system_info(schedulers),
    start_link(DefultBCryptQueueNum).

start_link(BCryptQueueNum) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [BCryptQueueNum], []).

gen_salt() ->
    Rounds = application:get_env(bcrypt, default_log_rounds, ?DEFAULT_LOG_ROUNDS),
    gen_salt(Rounds).
gen_salt(Rounds) ->
    {ok, bcrypt_nif:gen_salt(Rounds)}.

hashpw(Password, Salt) ->
    case whereis(?MODULE) of
        undefined ->
            {error, not_started};
        Pid ->
            hashpw(Pid, Password, Salt)
    end.

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([CtxN]) when CtxN > 0 ->
    Ctxs = list_to_tuple([bcrypt_nif:create_ctx() || _ <- lists:seq(1, CtxN)]),
    {ok, #state{
        current_ctx_index = 0,
        contexts = Ctxs
    }}.

terminate(_Reason, _State) ->
    ok.

handle_cast({hashpw, Ref, Pid, Password, Salt},
            #state{contexts = Ctxs, current_ctx_index = Index} = State) ->
    Ctx = element(Index + 1, Ctxs),
    ok = bcrypt_nif:hashpw(Ctx, Ref, Pid, Password, Salt),
    NewIndex = (Index + 1) rem tuple_size(Ctxs),
    {noreply, State#state{current_ctx_index=NewIndex}};
handle_cast(Msg, _) ->
    exit({unknown_cast, Msg}).

handle_call(Msg, _, _) ->
    exit({unknown_call, Msg}).

handle_info(Msg, _) ->
    exit({unknown_info, Msg}).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

hashpw(Pid, Password, Salt) ->
    MRef = monitor(process, Pid),
    ok = gen_server:cast(Pid, {hashpw, MRef, self(), Password, Salt}),
    receive
        {ok, MRef, Result} ->
            _ = erlang:demonitor(MRef, [flush]),
            {ok, Result};
        {error, MRef, Result} ->
            _ = erlang:demonitor(MRef, [flush]),
            {error, Result};
        {'DOWN', MRef, process, Pid, Reason} ->
            {error, Reason}
    end.


