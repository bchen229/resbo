add_rule(Predicate, X) :-
    Fact =.. [Predicate, X],
    assertz(Fact).

remove_rule(Predicate, X) :-
    Fact=.. [Predicate, X],
    retract(Fact).