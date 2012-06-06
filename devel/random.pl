%% random pl


randFull :-
    randHistory(0),
    randHistory(1),
    randHistory(2),
    randHistory(3),
    randHistory(4),
    randHistory(5),
    randHistory(6),
    !, fail.

getRand(R) :-
    repeat,
    random(0,7,Z),
    \+ randHistory(Z),
    R is Z,
    asserta(randHistory(Z)).


clearRand :-
    retractall(randHistory),
    asserta(randHistory(-1)).
