-module(qc_testm).

-ifdef(QC).

-export([t_test/1, t_not/1, t_and/2, t_andl/1, t_or/2, t_then/2, t_all/1, t_all/2, t_run/1]).
-export([test_all/1, fix_valid/1]).

-export_type([test_fun/1]).

-ifdef(lazy).
-type t_v(A) :: A.
-type t_m(A) :: fun(() -> {true, t_v(A)}|{false, any()}).
-else. %% -- strict --
-type t_v(A) :: A.
-type t_m(A) :: {true, t_v(A)}|{false, any()}.
-endif.

-type t_a(A,B) :: fun((A) -> t_m(B)).
-spec t_bind(t_m(A), t_a(A,B)) -> t_m(B).
-spec t_return(t_v(A)) ->t_m(A).
-spec t_run(t_m(A)) -> {true, t_v(A)}|{false, any()}.

-spec t_test({true, t_v(A)}|{false, any()}) ->t_m(A).
-spec t_not(t_m(A)) -> t_m(A).
-spec t_and(t_m(A), t_m(B)) -> t_m(A|B).
-spec t_andl([t_m(_)]) -> t_m(_).
-spec t_or(t_m(A), t_m(B)) -> t_m(A|B).

-define(T_LET(Param,M,Body),
        t_bind(M,fun(Param) -> Body end)).

t_bind(M, F) ->
    case t_run(M) of %% -- strict --
        {true, V} -> F(V);
        {false, V} -> t_fail(V)
    end.


-ifdef(lazy).
t_return(X) ->
    fun() -> {true, X} end.
t_fail(X) ->
    fun() -> {false, X} end.
t_run(M) ->
    M().
-else. %% -- strict
t_return(X) ->
    {true, X}.
t_fail(X) ->
    {false, X}.
t_run(M) ->
    M.
-endif.

t_test(M) ->
    case t_run(M) of
        {true, V} ->
            t_return(V);
        {false, V} ->
            t_fail(V)
    end.
t_not(M) ->
    case t_run(M) of
        {true, V} ->
            t_fail(V);
        {false, V} ->
            t_return(V)
    end.
t_and(M1, M2) ->
    t_then(M1, M2).
t_andl(Ms) ->
    t_all(Ms).
t_or(M1, M2) ->
    t_not(t_then(t_not(M1), t_not(M2))).


%% -----
-spec t_then(t_m(_), t_m(B)) -> t_m(B).
t_then(M1, M2) ->
    t_bind(M1, fun(_) -> M2 end).

-spec t_all([t_m(A)]) -> t_m(A).
t_all(Ms) ->
    lists:foldl(
      fun(M,Acc) -> t_then(Acc, M) end,
      hd(Ms), tl(Ms)).


t_all(M0, As) ->    
    lists:foldl(fun(A, Acc) ->
                        t_bind(Acc, A)
                end, M0, As).


%%% --- test predicates
-spec fix_valid(t_m(_)) -> boolean().
fix_valid(F) ->
    {B,_} = X = t_run(F),
    trace(fix_valid, X),
    B.

-type test_fun(A) :: t_m(A).
-spec test_all([test_fun(A)]) -> true | {test_failed, A}.
test_all(Fs) ->
    case t_run(t_all(Fs)) of
        {true, _} ->
            true;
        {false, Err} ->
            {test_failed, Err}
    end.

%% -- debug trace ---
trace(_,_) ->
    ok.

-endif. %% -ifdef(QC).
