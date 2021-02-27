

:- module( formula,
           [
               nonEmptyFormula/1,
               assign/4,
               hasVariables/3
           ]
         )
.

/*
assign
Produces a new formula ReducedFormula, based on an original Formula,
but simplified according to the given choice of variable value. Var is an atom
specifying the variable name. Val is either the true atom or the false atom.
*/
assign(Var, Val, Formula, ReducedFormula) :-
    getVal(Var, Val, Neg, Pos),
    reduceConjuncts(Neg, Pos, Formula, ReducedFormula)
    . 

reduceConjuncts(NegVal, PosVal, Formula, NewFormula) :-
    maplist(exclude(=(NegVal)), Formula, NoLit),
    nonEmptyFormula(NoLit),
    removeConjuncts(PosVal, NoLit, NewFormula)
    .



/*
getAtom
Helper function for hasVariables
*/
getAtom([], [], [])
.
getAtom([H1|T1], Vars, Truths) :-
    getAtom(T1, Vs, Ts),
    (
        compound_name_arguments(H1, T, V),
        (
            T = + -> true,
            append([true], Ts, Truths);
            append([false], Ts, Truths)
        ),
        append(V, Vs, Vars)
    )
. 
/*
getVal
Sets the positive or negative value of the atom
*/
getVal(Var, Val, NegVal, PosVal) :-
    Val = true ->
    NegVal = -Var,
    PosVal = +Var;
    NegVal = +Var,
    PosVal = -Var
. 
/*
hasVariables
Creates 2 lists, Var_List with the atoms and Truth_List with the
truth assignments
*/

hasVariables([], [], [])
. 

hasVariables([C|Cs], [V|Vs], [T|Ts]) :-

    getAtom(C, VarList, Truths),
    V = VarList,
    T = Truths,
    hasVariables(Cs, Vs, Ts)
. 


/*
nonEmptyFormula
checks to see if the list is empty, returns false if so
otherwise returns true
*/
nonEmptyFormula(List):-
    maplist(dif([]), List).

/*
removeConjunct
removes the head of the list
*/
removeConjuncts(_, [], [])
.
removeConjuncts(Var, [H|T], L) :-
    removeConjuncts(Var, T, Nl),
    (
        memberchk(Var, H) -> true,
        L = Nl;
        L = [H|Nl]
    ).



