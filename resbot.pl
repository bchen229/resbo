% noun phrase
noun_phrase(T0,T4,Obj,C0,C4) :-
    det(T0,T1,Obj,C0,C1),
    adjectives(T1,T2,Obj,C1,C2),
    noun(T2,T3,Obj,C2,C3),
    pp(T3,T4,Obj,C3,C4).

% negative noun phrase
noun_phrase_neg(T0,T4,Obj,C0,C4) :-
    det(T0,T1,Obj,C0,C1),
    adjectives_neg(T1,T2,Obj,C1,C2),
    noun_neg(T2,T3,Obj,C2,C3),
    pp_neg(T3,T4,Obj,C3,C4).

% verb phrase
verb_phrase(T0,T2,Obj,C0,C2) :-
    verb(T0,T1,Obj,C0,C1),
    noun_phrase(T1,T2,Obj,C1,C2).

% negative verb phrase
verb_phrase_neg(T0,T2,Obj,C0,C2) :-
    verb(T0,T1,Obj,C0,C1),
    noun_phrase_neg(T1,T2,Obj,C1,C2).

% determinants
det([the|T],T,_,C,C).
det([a|T],T,_,C,C).
det(T,T,_,C,C).

% adjectives used to define "cheap", "moderate",
% and "expensive" price relation
adjectives(T,T,_,C,C).
adjectives(T0,T2,Obj,C0,C2) :-
    adj(T0,T1,Obj,C0,C1),
    adjectives(T1,T2,Obj,C1,C2).

adj([cheap|T],T,Res,C,[prop(Res,name),
                       prop(Res,serves)|C]) :-
    prop(Res,price,cheap).
adj([moderate|T],T,Res,C,[prop(Res,name),
                          prop(Res,serves)|C]) :-
    prop(Res,price,moderate).
adj([expensive|T],T,Res,C,[prop(Res,name),
                           prop(Res,serves)|C]) :-
    prop(Res,price,expensive).
% adjectives used to define the type relation
adj([fast|T],T,Res,C,[prop(Res,name),
                      prop(Res,serves)|C]) :-
    prop(Res,style,fast).
adj([cafe|T],T,Res,C,[prop(Res,name),
                      prop(Res,serves)|C]) :-
    prop(Res,style,cafe).
adj([western|T],T,Res,C,[prop(Res,name),
                         prop(Res,serves)|C]) :-
    prop(Res,style,western).
adj([japanese|T],T,Res,C,[prop(Res,name),
                          prop(Res,serves)|C]) :-
    prop(Res,style,japanese).

% adjectives_neg used to define the negation of the adjectives
% adjectives used to define "cheap", "moderate",
% and "expensive" price relation
adjectives_neg(T,T,_,C,C).
adjectives_neg(T0,T2,Obj,C0,C2) :-
    adj_neg(T0,T1,Obj,C0,C1),
    adjectives_neg(T1,T2,Obj,C1,C2).

adj_neg([cheap|T],T,Res,C,[prop(Res,name),
                       prop(Res,serves)|C]) :-
    prop(Res,price,_),
    \+prop(Res,price,cheap).
adj_neg([moderate|T],T,Res,C,[prop(Res,name),
                          prop(Res,serves)|C]) :-
    prop(Res,price,_),
    \+prop(Res,price,moderate).
adj_neg([expensive|T],T,Res,C,[prop(Res,name),
                           prop(Res,serves)|C]) :-
    prop(Res,price,_),
    \+prop(Res,price,expensive).
% adjectives used to define the style relation
adj_neg([fast|T],T,Res,C,[prop(Res,name),
                      prop(Res,serves)|C]) :-
    prop(Res,style,_),
    \+prop(Res,style,fast).
adj_neg([cafe|T],T,Res,C,[prop(Res,name),
                      prop(Res,serves)|C]) :-
    prop(Res,style,_),
    \+prop(Res,style,cafe).
adj_neg([western|T],T,Res,C,[prop(Res,name),
                         prop(Res,serves)|C]) :-
    prop(Res,style,_),
    \+prop(Res,style,western).
adj_neg([japanese|T],T,Res,C,[prop(Res,name),
                          prop(Res,serves)|C]) :-
    prop(Res,style,_),
    \+prop(Res,style,japanese).

% noun food is a general word used to refer to all types of food
noun([food|T],T,_,C,C).
% noun queries for the restauarant that have the Food
noun([Food|T],T,Res,C,[prop(Res,name),
                       prop(Res,serves)|C]) :-
    prop(Res,serves,Food),
    \+member(prop(Res,name),C).

% noun_neg matches restaurants that do not have the food
noun_neg([food|T],T,_,C,C).
noun_neg([Food|T],T,Res,C,[prop(Res,name),
                           prop(Res,serves)|C]) :-
    prop(Res,type,restaurant),
    \+prop(Res,serves,Food),
    \+member(prop(Res,name),C).

% optional prepositional phrase
pp(T,T,_,C,C).
pp(T0,T2,Res,C0,C2) :-
     preposition(T0,T1,Res,C0,C1),
     prop_add(T1,T2,_,C1,C2).

preposition([and|T],T,_,C,C).
preposition([or|T],T,_,C,C).

% optional negative prepositional phrase
pp_neg(T,T,_,C,C).
pp_neg(T0,T2,Res,C0,C2) :-
    preposition(T0,T1,Res,C0,C1),
    prop_remove(T1,T2,Res,C1,C2).
    
preposition_neg([and|T],T,_,C,C).
preposition_neg([or|T],T,_,C,C).

prop_add([Food|T],T,Res,C,[prop(Res,name),
                           prop(Res,serves)|C]) :-
    prop(Res,serves,Food),
    \+member(prop(Res,name),C).

prop_remove([Food|T],T,_,C,C) :-
    prop(Res,serves,Food),
    \+member(prop(Res,name),C).
% C2 is C0 without instances of Res
prop_remove([Food|T],T,_,C0,C2) :-
    prop(Res,serves,Food),
    member(prop(Res,name),C0),
    delete(C0,prop(Res,name),C1),
    delete(C1,prop(Res,serves),C2).

% verb
verb([to,eat|T],T,_,C,C).
% no verbs like "to eat" just assumes eating
verb(T,T,_,C,C).

% user_input 
% for the affirmative phrase, we can simply query the knowledge
% base with the object that matches the food that is passed in.
% Obj is the restaurant that serves the food.
user_input([i,like|T0],T1,Obj,C0,C1) :-
    verb_phrase(T0,T1,Obj,C0,C1).
user_input([i,want|T0],T1,Obj,C0,C1) :-
    verb_phrase(T0,T1,Obj,C0,C1).
% for the negative phrase, we need to take the Obj returned from
% the user and query for all restaurants not matching
user_input([i,do,not,like|T0],T1,Obj,C0,C1) :-
    verb_phrase_neg(T0,T1,Obj,C0,C1).
user_input([i,do,not,want|T0],T1,Obj,C0,C1) :-
    verb_phrase_neg(T0,T1,Obj,C0,C1).

% restauarant_id_from_query is true if list A is the name of restauarant with id RestauarantId
restauarant_id_from_query(S, A, RestauarantId, S) :-
    atomic_list_concat(A, ' ', Name),
    prop(RestauarantId, name, Name).

restauarant_id_from_query([H|T], A, RestauarantId, RestS) :- 
    append(A, [H], NewA),
    restauarant_id_from_query(T, NewA, RestauarantId, RestS).

% Restauarant serves Food
state(S) :- 
    restauarant_id_from_query(S, [], RestauarantId, [serves|[V|_]]),
    add_rule(prop, RestauarantId, serves, V).

% Restauarant no longer serves Food
state(S) :- 
    restauarant_id_from_query(S, [], RestauarantId, [no, longer, serves|[V|_]]),
    remove_rule(prop, RestauarantId, serves, V).

% Restauarant has closed down
state(S) :- 
    restauarant_id_from_query(S, [], RestauarantId, [has, closed, down]),
    remove_rule(prop, RestauarantId, _, _).

% state(["Pizza", "Hut", serves, burger]).
% state(["Pizza", "Hut", no, longer, serves, burger]).
% state(["Pizza", "Hut", has, closed, down]).


% ask
% Since C is a list of relations on individuals, 
% we use prove_all to execute the queries
% example: ?- ask([i,like,to,eat,seafood],Res,Food).
ask(Q,Alias,Food) :-
    user_input(Q,[],_,[],C),
    prove_all(C,[],Alias,[],Food).

% prove all
% iterates through the list of queries and variables
% and calls the queries with the corresponding variables
prove_all([],_,[],_,[]).
prove_all([Q|QT],A0,[A|A1],F0,F1) :-
    Q = prop(_,name),
    call(Q,A),
    prove_all(QT,A0,A1,F0,F1).
    
prove_all([Q|QT],A0,A1,F0,[F|F1]) :-
    Q = prop(_,serves),
    call(Q,F),
    prove_all(QT,A0,A1,F0,F1).

add_rule(Predicate, X, Y, Z) :-
    Fact =.. [Predicate, X, Y, Z],
    assertz(Fact).

remove_rule(Predicate, X, Y, Z) :-
    Fact=.. [Predicate, X, Y, Z],
    retract(Fact).

% Knowledge base
prop(mcdonald, type, restaurant).
prop(mcdonald, style, fast).
prop(mcdonald, name, 'McDonalds').
prop(mcdonald, serves, burgers).
prop(mcdonald, serves, fries).
prop(mcdonald, serves, icecream).
prop(mcdonald, price, cheap).

prop(starbucks, type, restaurant).
prop(starbucks, style, cafe).
prop(starbucks, name, 'Starbucks').
prop(starbucks, serves, coffee).
prop(starbucks, serves, cookie).
prop(starbucks, price, moderate).

prop(pizzahut, type, restaurant).
prop(pizzahut, style, fast).
prop(pizzahut, name, 'Pizza Hut').
prop(pizzahut, serves, pizza).
prop(pizzahut, price, moderate).

prop(sage, type, restaurant).
prop(sage, style, western).
prop(sage, name, 'Sage Bistro').
prop(sage, serves, seafood).
prop(sage, price, expensive).

prop(bento_sushi, type, restaurant).
prop(bento_sushi, style, japanese).
prop(bento_sushi, name, 'Bento Sushi').
prop(bento_sushi, serves, sushi).
prop(bento_sushi, serves, ramen).
prop(bento_sushi, price, moderate).

prop(burgers, type, food).
prop(fries, type, food).
prop(icecream, type, food).
prop(coffee, type, food).
prop(cookie, type, food).
prop(pizza, type, food).
prop(seafood, type, food).
prop(sushi, type, food).
prop(ramen, type, food).