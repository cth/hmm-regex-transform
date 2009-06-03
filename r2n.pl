:- use_module(library(chr)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Conversion of a regular expression in s-expr format
% to a NFA.
% Christian Theil Have, April 2009.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- chr_constraint report_nfa/0, regex/2, transition/3, nest_stack/1.

% Simplify nested expressions
list_simplify @
regex(PreviousState, [[Expr|Rest]]) <=>
    write('list_simplify '), write(regex(PreviousState, [[Expr|Rest]])), nl
    |
    regex(PreviousState, [Expr|Rest]).

% Produce an incomplete transition constraint from the start state
start @
regex(start,X) <=>
    newstate(S),
    write(start(S)),nl,
    write(add(transition(start,S,incomplete))),nl,
    write(add(regex(S,X))),nl
    |
    transition(start-epsilon,S,incomplete),
    nest_stack([]),
    regex(S,X).

% Repetion
repetion @
regex(PreviousState, [star,Expr]),
transition(Symbol, PreviousState, incomplete),
nest_stack(Stack) <=>
    newstate(RecursionState), % Newstate is a dummy state for a recursion point
    write('repeat rule'),nl,
    write(add(transition(Symbol,PreviousState,RecursionState))),nl,
    write(add(transition(epsilon,RecursionState,incomplete))),nl,
    write(add(transition(epsilon,incomplete,RecursionState))),nl,
    write(add(regex(RecursionState,Expr))),nl
    |
    transition(Symbol, PreviousState, RecursionState), % This one replaces previous incomplete
    transition(epsilon, RecursionState, incomplete),
    regex(RecursionState,Expr),
    nest_stack([RecursionState|Stack]).

alternation @
regex(PreviousState, [or, Expr1, Expr2]),
transition(Symbol,PreviousState,incomplete),
nest_stack(Stack) <=>
    newstate(Branch),newstate(Junction),
    write('select rule'),nl
    |
    transition(Symbol,PreviousState,Branch), % Complete incoming transition
    regex(Branch,Expr1),
    transition(epsilon,Branch,incomplete),
    regex(Branch,Expr2),
    transition(epsilon,Branch,incomplete),
    % Make it possible to merge branches
    nest_stack([Junction,Junction|Stack]),
    % Finally, add an imcomplete transition from the junction:
    transition(epsilon,Junction,incomplete).

regex(_, []) <=> true.

symbol @
regex(PreviousState, Symbol),
transition(
<=>
        atom(Symbol)

concatenation @
regex(PreviousState, [concat, Expr1, Expr2]),
transition(PreviousSymbol, PreviousState, incomplete) <=>
	newstate(State),
	write(add(regex(State,Rest))),nl,
	write(add(transition(Symbol,State,incomplete))),nl
	|
	transition(PreviousSymbol,PreviousState,State),
	regex(PreviousState, Expr1),
	
	transition(Symbol,State,incomplete),
	regex(State,Rest).
	
merging @
transition(Symbol,From, incomplete), nest_stack([LastBranch|RestBranch]) <=>
	write(merge_transitions), nl
	|
	transition(Symbol,From,LastBranch),
	nest_stack(RestBranch).

report @
report_nfa, transition(Symbol, From, To) <=>
        write('Final transition: '),
	write(transition(Symbol,From,To)),nl,
	assert(nfa_transition(Symbol,From,To))
	|
        report_nfa.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Prolog Helper rules for CHR program
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
newstate(X) :-
	catch(prevstate(Y),_,(Y is 0, assert(prevstate(Y)))),
	X is Y + 1,
	retract(prevstate(Y)),
	assert(prevstate(X)).

regex_to_nfa(Regex, NFA) :-
	assert(prevstate(0)),
	regex(start, Regex),
	report_nfa,
	findall([Symbol,To,From],nfa_transition(Symbol,To,From),NFA),
	retractall(prevstate(_)),
	retractall(nfa_transition(_,_,_)).

regexnfa(Regex) :-
	regex_to_nfa(Regex,NFA),
	nfa_graphviz(NFA,'nfa.dot'),
	viewdot('nfa.dot').

test1 :-
	regex_to_nfa(['a','b'],NFA),
	write('---------'),nl,
	write(NFA),nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Convert NFA to graphviz dot file

write_gv_edges([]).
write_gv_edges([[Symbol,To,From]|Rest]) :-
	write('\t"'),
	write(To),
	write('" -> "'),
	write(From),
	write('" [label="'),
	write(Symbol),
	write('"]'),
	nl,
	write_gv_edges(Rest).

nfa_graphviz(NFA,OutputFile) :-
	tell(OutputFile),
	write('digraph NFA {'),nl,
	write('\trankdir=LR'),nl,
	write_gv_edges(NFA),
	write('}'),nl,
	told.

viewdot(DotFile) :-
	atom_concat('cat ', DotFile, DotCmd1),
	atom_concat(DotCmd1, ' | dot -Tpng -oviewdot.png',DotCmd),
	shell(DotCmd),
	shell('open viewdot.png').

