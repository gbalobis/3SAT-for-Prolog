
:- module( solver,
           [
            solve/2
           ]
         )
.

:- use_module('formula.pl')
.

solve([], []).
solve(Formula, Assignment) :-
    hasVariables(Formula, [[Var|_]|_], [[Val|_]|_]),
    (
        checkAssign(Var, Val, Formula, Assignment) ->
        true;
        negateValue(Val, NotVal),
        checkAssign(Var, NotVal, Formula, Assignment)
    )
    .

negateValue(Val, NotVal) :-
    Val = true ->
    NotVal = false;
    NotVal = true
    .

checkAssign(Var, Val, Formula, Assign) :-
    assign(Var, Val, Formula, NewFormula),
    solve(NewFormula, SatList),
    append([Var=Val], SatList, Assign)
    .

