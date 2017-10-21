% noun phrase
noun_phrase(T0,T4,Obj,C0,C4) :-
    det(T0,T1,Obj,C0,C1),
    adjectives(T1,T2,Obj,C1,C2),
    noun(T2,T3,Obj,C2,C3),
    mp(T3,T4,Obj,C3,C4).

% verb phrase
verb_phrase(T0,T2,Obj,C0,C2) :-
    verb(T0,T1,Obj,C0,C1),
    opt_noun_phrase(T1,T2,Obj,C1,C2).

% optional noun phrase can exist or not
opt_noun_phrase(T,T,_,C,C).
opt_noun_phrase(T0,T1,Obj,C0,C1) :-
    noun_phrase(T0,T1,Obj,C0,C1).

% determinants
det([the|T],T,_,C,C).
det([a|T],T,_,C,C).
det(T,T,_,C,C).

% adjectives
adjectives(T,T,_,C,C).
adjectives(T0,T2,Obj,C0,C2) :-
    adj(T0,T1,Obj,C0,C1),
    adjectives(T1,T2,Obj,C1,C2).

% no adjectives yet
adj([good|T],T,Obj,C,[good(Obj)|C]).
adj([tasty|T],T,Obj,C,[tasty(Obj)|C]).

% modifying phrase
mp(T,T,_,C,C).
mp(T0,T2,O1,C0,C2) :-
    reln(T0,T1,O1,O2,C0,C1),
    noun_phrase(T1,T2,O2,C1,C2).
mp([that|T0],T2,O1,C0,C2) :-
    reln(T0,T1,O1,O2,C0,C1),
    noun_phrase(T1,T2,O2,C1,C2).

% relation
reln([better,than|T],T,O1,O2,C,[better(O1,O2)|C]).

% noun
noun([food|T],T,Obj,C,[food(Obj)|C]).
noun([X|T],T,X,C,C) :- food(X).

% verb
verb([to,eat|T],T,_,C,C).

% user_input 
user_input([i,like|T0],T1,Obj,C0,C1) :-
    mp(T0,T1,Obj,C0,C1).
user_input([i,like|T0],T1,Obj,C0,C1) :-
    verb_phrase(T0,T1,Obj,C0,C1).

% ask
% Since C is a list of relations on individuals, 
% we use prove_all to execute the queries
ask(Q,A) :-
    user_input(Q,[],A,[],C),
    prove_all(C).

% prove all
prove_all([]).
prove_all([H|T]) :-
    call(H),
    prove_all(T).

% Knowledge base
food(pizza).
food(kiwi).
food([fried,chicken]).

good(pizza).

tasty(pizza).

better(pizza,kiwi).

% 

prop(mcdonald, type, restaurant).
prop(mcdonald, name, "McDonalds")
prop(mcdonald, serves, burger).
prop(mcdonald, serves, fries).
prop(mcdonald, serves, icecream).

prop(starbucks, type, cafe).
prop(starbucks, name, "Starbucks")
prop(starbucks, serves, coffee).
prop(starbucks, serves, cookie).

prop(pizzahut, type, restaurant).
prop(pizzahut, name, "Pizza Hut")
prop(pizzahut, serves, pizza).

prop(sage, type, restaurant).
prop(sage, name, "Sage Bistro")
prop(sage, serves, seafood).
