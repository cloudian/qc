%%%-------------------------------------------------------------------
%%% Copyright (c) 2010-2012 Gemini Mobile Technologies, Inc.  All rights reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% File    : qc_statem.erl
%%% Purpose : Wrapper for statem
%%%-------------------------------------------------------------------

-module(qc_statem).

-ifdef(QC).

-eqc_group_commands(false).

-include("qc_statem.hrl").

%% API
-export([qc_run/3]).
-export([qc_sample/2]).
-export([qc_prop/2]).
-export([qc_counterexample/3]).
-export([qc_counterexample_read/3]).
-export([qc_counterexample_write/2]).
-export([qc_recheck/2]).
-export([qc_pretty_print/0, qc_pretty_print/1]).
-export([rfun/1]).

%% Interface Functions
-ifndef(old_callbacks).

-type call() :: {call, Mod::atom(), Fun::atom(), Args::list(term())}.
-type var() :: {var, integer()}.

-callback scenario_gen() -> Gen::term().
-callback command_gen(SymState::term()) -> Gen::term().
-callback scenario() -> Gen::term().
-callback command(SymState::term()) -> Gen::term().
-callback initial_state(Scenario::term()) -> SymState::term().
-callback state_is_sane(DynState::term()) -> boolean().
-callback next_state(SymState::term(), R::var(), C::call()) -> SymState::term().
-callback precondition(SymState::term(), C::call()) -> boolean().
-callback postcondition(DynState::term(), C::call(), R::term()) -> boolean().
-callback setup() -> ok.
-callback setup(Scenario::term()) -> {ok, Ref::term()}.
-callback teardown(Ref::term(), DynState::term() | undefined) -> ok.
-callback aggregate([{N::integer(), Call::term(), R::term(), DynState::term()}]) -> [term()].

-else. % -ifndef(old_callbacks).

-export([behaviour_info/1]).

%% Define the behaviour's required mods.
behaviour_info(callbacks) ->
    [{scenario_gen,0}
     , {command_gen,1}
     , {scenario,0} %% Bug 36899
     , {command,1}  %% Bug 36899
     , {initial_state,1}
     , {state_is_sane,1}
     , {next_state,3}
     , {precondition,2}
     , {postcondition,3}
     , {setup,0}
     , {setup,1}
     , {teardown,2}
     , {aggregate,1}
    ];
behaviour_info(_Other) ->
	undefined.

-endif. % -ifndef(old_callbacks).

-define(IMPL, qc_statem_impl).

%%%----------------------------------------------------------------------
%%% types and records
%%%----------------------------------------------------------------------

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
qc_run(Mod, NumTests, Options) ->
    ?IMPL:qc_run(Mod, NumTests, Options).

qc_sample(Mod, Options) ->
    ?IMPL:qc_sample(Mod, Options).

qc_prop(Mod, Options) ->
    ?IMPL:qc_prop(Mod, Options).

qc_counterexample(Mod, Options, CounterExample) ->
    ?QC:check(qc_prop(Mod, Options), CounterExample).

qc_counterexample_read(Mod, Options, FileName) ->
    {ok, [CounterExample]} = file:consult(FileName),
    qc_counterexample(Mod, Options, CounterExample).

qc_counterexample_write(FileName, CounterExample) ->
    file:write_file(FileName, io_lib:format("~p.~n", CounterExample)).

qc_recheck(Mod, Options) ->
    ?QC:recheck(qc_prop(Mod, Options)).

qc_pretty_print() ->
    qc_pretty_print([]).
qc_pretty_print(Env) ->
    case ?QC:counterexample() of
        [_, {Cmds0, CmdsL}]  ->
            pprint0( "pre-cmds:", Env, Cmds0),
            [pprint0("parallel-cmds:", Env, Cmds) || Cmds <- CmdsL];
        [_, Cmds]  ->
            pprint0("cmds:", Env, Cmds);
        _ ->
            no_counterexample
    end.

pprint0(Label, Env, Cmds) ->
    io:format("%% ~s~n", [Label]),
    [io:format("~s.~n",[eqc_symbolic:pretty_print(Env, C)]) ||
        {set, _, C} <- Cmds].

%%%----------------------------------------------------------------------
%%% Utility
%%%----------------------------------------------------------------------
rfun(Fac) -> %% example resize_fun
    fun(X) ->
            N0 = get(numttt),
            if N0==undefined ->
                    N=0;
               true ->
                    N=N0
            end,
            put(numttt, N+1), 
            if (N rem 100)=:=0 -> io:format("~p~n", [N]);
               true -> pass
            end,
            X*Fac
    end.


-endif. %% -ifdef(QC).
