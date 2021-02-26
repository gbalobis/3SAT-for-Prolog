

:- module( formula,
           [
               empty_formula/1,
               assign/4,
               has_variables/3
           ]
         )
.

empty_formula(List):-
maplist(dif([]), List).

assign(Var, Val, Formula, Reduced_formula) :-
    get_val(Var, Val, Neg, Pos),
    reduce_conjuncts(Neg, Pos, Formula, Reduced_formula)
    .
reduce_conjuncts(Neg_val, Pos_val, Formula, New_formula) :-
    maplist(exclude(=(Neg_val)), Formula, No_Lit),
    empty_formula(No_Lit),
    remove_conjuncts(Pos_val, No_Lit, New_formula)
    .
get_val(Var, Val, Neg_val, Pos_val) :-
    Val = true ->
    Neg_val = -Var,
    Pos_val = +Var;
    Neg_val = +Var,
    Pos_val = -Var
    .

remove_conjuncts(_, [], [])
.
remove_conjuncts(Var, [H|T], L) :-
    remove_conjuncts(Var, T, Nl),
    (
        memberchk(Var, H) -> true,
        L = Nl;
        L = [H|Nl]
    ).

get_conjuncts([], [], [])
.
get_conjuncts([C|Cs], [V|Vs], [T|Ts]) :-
    get_atom(C, Var_List, Truths),
    V = Var_List,
    T = Truths,
    get_conjuncts(Cs, Vs, Ts)
    . 

get_atom([], [], [])
.
get_atom([H1|T1], Vars, Truths) :-
    get_atom(T1, Vs, Ts),
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