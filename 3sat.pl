:- use_module('Solver.pl'),
    use_module('Parser.pl')
    .
sat(File_Name) :-
    read_in(File_Name, Formula),
    solve(Formula, Assignment) ->
    write("satisfiable by "),
    writeln(Assignment);
    writeln("unsatisfiable")
    .