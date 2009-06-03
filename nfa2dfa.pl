%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Convert a NFA to a DFA
% Christian Theil Have, April 2009.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- use_module(library(chr)).

:- chr_constraint dfa_start/1, epsilon_closure/2, transition/3, dfa_transition/3.


dfa_start(NS1) <=>
   new_dfa_state(DS1)
   |
   epsilon_closure(DS1,NS1).

:- chr_constraint report/0.

% e-closure part

epsilon_closure(A,B) \ epsilon_closure(A,B) <=> true.

epsilon_closure(DfaState, From) \ transition(epsilon, From, To) <=>
    epsilon_closure(DfaState,To).

% Move part:
epsilon_closure(DState1, NFrom), dfa_transition(Symbol, DState1, DState2) \ transition(Symbol, NFrom, NTo) <=>
   Symbol \= epsilon
   |
   epsilon_closure(DState2, NTo).

epsilon_closure(DState, NFrom), epsilon_closure(DState, NTo) \ transition(Symbol, NFrom, NTo) <=>
    Symbol \= epsilon
    |
    dfa_transition(Symbol, DState, DState).

epsilon_closure(DState1, NFrom) \ transition(Symbol, NFrom, NTo) <=>
    Symbol \= epsilon,
    new_dfa_state(DNewState) % Create new DFA state
    |
    epsilon_closure(DNewState, NTo),
    dfa_transition(Symbol, DState1, DNewState).
    
report, epsilon_closure(X,Y) <=>
    write(' :'),
    write(epsilon_closure(X,Y)), nl,
    report.

report, dfa_transition(X,Y,Z) <=>
    write(' :'),
    write(dfa_transition(X,Y,Z)), nl,
    report.

init1 :-
	assert(prevstate(0)),
	transition(epsilon, 3, end),
	transition(b, 4, 3),
	transition(epsilon, 3, 4),
	transition(a, 2, 3),
	transition(epsilon, 1, 2),
	dfa_start(1),
	retractall(prevstate(_)).

init2 :-
	assert(prevstate(0)),
	transition(b, 8, end),
	transition(a, 7, 8),
	transition(epsilon, 2, 7),
	transition(epsilon, 4, 2),
	transition(b, 6, 4),
	transition(epsilon, 3, 6),
	transition(a, 5, 4),
	transition(epsilon, 3, 5),
	transition(epsilon, 2, 3),
	transition(epsilon, 1, 2),
	dfa_start(1),
	retractall(prevstate(_)).	

new_dfa_state(State) :-
	catch(prevstate(Y),_,(Y is 0, assert(prevstate(Y)))),
	X is Y + 1,
	retract(prevstate(Y)),
	assert(prevstate(X)),
        atom_concat(dfa, X, State).