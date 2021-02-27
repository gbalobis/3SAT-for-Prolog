:- module( testformulas,
           [
               formula/1,
               formula2/1
           ]
         ).
:- use_module('solver.pl')
.

formula0(+a)
.
formula0(+b)
.
formula0(+c)
.

formula0(-a)
.
formula0(-b)
.
formula0(-c)
.

formula2(A) :-
    formula0(X),
    formula0(Y),
    formula0(Z),
    not(X=Y),
    append([X], [Y], W),
    append(W, [Z], A)
    .

formula(L) :-
    formula2(Y),
    formula2(X),
    formula2(Z),
    not(Y=Z),
    append([X], [Y], W),
    append(W, [Z], L)
    .