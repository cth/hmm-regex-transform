:- use_module(library(chr)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This program performs conversions from
% regex -> NFA -> DFA
%
% It consists of three parts:
% 1. A regular expression parser (DCG)
% 2. A CHR program for converting DCG parse tree to an NFA
% 3. A CHR program for converting the NFA to a DFA
%
% Additionally, the program contains some utilities for generating
% and viewing graphviz output.
%
% Before usage, the symbols allowed in regular expressions can
% be initialized, but are per default initialized to the letters
% the English alphabet plus numbers 0-9.
%
% The program has tested with SWI prolog version 5.2.62
%
% To try it out, you can try one of these:
% viewnfa(['(',a,or,b,')',*,a,b]).
% viewdfa(['(',a,or,b,')',*,a,b]).
%
% Depending on you system you might need to install graphviz (dot)
% and edit the viewdot rule in the bottom of the program to
% adapt it to your system (it works on my mac).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 1:
% A simple DCG for parsing regular expressions:
% A parameter is used to build the parsetree
% of the regular expression as an s-expression
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_regexp_symbol(Symbols) :-
	retractall(symbol(_)),
	add_regexp_symbols(Symbols).

add_regexp_symbols([]).
add_regexp_symbols([Symbol|Rest]) :-
	assert(symbol(Symbol)),
	add_regexp_symbols(Rest).

% Default symbols:
:- add_regexp_symbols([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,w,v,x,y,z,1,2,3,4,5,6,7,8,9,0]).

regexp(R) --> alternation(R).
regexp(R) --> repetition(R).
regexp(R) --> concatenation(R).

alternation([or,R1,R2]) --> alternation_primitive(R1), or, alternation(R2).
alternation([or,R1,R2]) --> alternation_primitive(R1), or, alternation_primitive(R2).

alternation_primitive(R) --> repetition(R).
alternation_primitive(R) --> concatenation(R).

repetition([star, R]) --> repetition_primitive(R), star.

repetition_primitive([concat, R, []]) --> symbol(R). % Note, single symbols are concatenated with empty list
repetition_primitive(R) --> lparen, concatenation(R), rparen.
repetition_primitive(R) --> lparen, alternation(R), rparen.

concatenation([concat,R1,R2]) --> concatenation_primitive(R1), concatenation(R2).
concatenation([concat,R,[]]) --> concatenation_primitive(R).

concatenation_primitive(R) --> symbol(R).
concatenation_primitive(R) --> repetition(R).
concatenation_primitive(R) --> lparen, alternation(R), rparen.

or --> [ '|' ].
or --> [ or ].
star --> [ '*' ].
star --> [ star ].
lparen --> [ '(' ].
rparen --> [ ')' ].

symbol(S) --> [S], { symbol(S) }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 2: Regular expression to NFA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- chr_constraint nfa_report/0,
                  regex/2,
		  nest_stack/1,
		  transition/3,
		  join/1,
		  nfa_finalize/0,
		  nfa_accept_state/1.

% Produce an incomplete transition constraint from the start state
start @
regex(start,X) <=>
    new_nfa_state(S)
    |
    nest_stack([[epsilon,S,incomplete]]),
    regex(S,X).

alternation @
regex(PreviousState, [or, Expr1, Expr2]), nest_stack([[Symbol,PreviousState,incomplete]|Rest]) <=>
    write('alternation'),nl,
    new_nfa_state(Branch),new_nfa_state(Junction)
    |
    transition(Symbol,PreviousState,Branch),
    nest_stack([[epsilon,Branch,incomplete]|Rest]),	
    regex(Branch,Expr1),
    join(Junction),
    nest_stack([[epsilon,Branch,incomplete]|Rest]),	
    regex(Branch,Expr2),
    join(Junction),
    nest_stack([[epsilon,Junction, incomplete]|Rest]).

repetion @
regex(PreviousState, [star,Expr]), nest_stack([[Symbol, PreviousState, incomplete]|Rest]) <=>
    write('repetition: '), write(regex(PreviousState, [star,Expr])),nl,
    new_nfa_state(RecursionState) % Newstate is a dummy state for a recursion point
    |
    transition(Symbol,PreviousState,RecursionState),
    nest_stack([[epsilon,RecursionState,incomplete]|Rest]),
    regex(RecursionState,Expr),
    join(RecursionState),	% removes nest_stack
    nest_stack([[epsilon,RecursionState, incomplete]|Rest]).

join1 @
join(JoinState), nest_stack([[Symbol,PreviousState,incomplete]|_]), regex(PreviousState,Expr) <=>
    write('join1'),nl
    |
    transition(Symbol, PreviousState, JoinState),
    regex(JoinState,Expr).	% Continue from there

join2 @
join(JoinState), nest_stack([[Symbol,PreviousState,incomplete]|_]) <=>
    write('join2'),nl
    |
    transition(Symbol, PreviousState, JoinState).

prune_empty @ regex(_, []) <=> write('prune_empty'), nl | true.

connect_parts @
nest_stack([[PreviousSymbol,PreviousState,incomplete]|Rest]),
regex(incomplete, [concat, Symbol, Expr]) <=>
    write('connect parts'),nl
    |
    nest_stack([[PreviousSymbol,PreviousState,incomplete]|Rest]),
    regex(PreviousState, [concat, Symbol, Expr]).

concatenation1 @
nest_stack([[PreviousSymbol,PreviousState,incomplete]|Rest]),
regex(PreviousState, [concat, Symbol, Expr]) <=>
    atom(Symbol),
    new_nfa_state(State),
    write('concatenation1: '), write(regex(PreviousState, [concat,Symbol,Expr])),nl
    |
    transition(PreviousSymbol,PreviousState,State), % Add a completed transition
    nest_stack([[Symbol,State,incomplete]|Rest]),
    regex(State,Expr).

concatenation2 @
nest_stack([[_,PreviousState,_]|_]) \
regex(PreviousState, [concat, Expr1, Expr2]) <=>
    write('concatenation2: '), write(regex(PreviousState, [concat,Expr1,Expr2])),nl
    |
    regex(PreviousState,Expr1), % Will push a new incomplete transition to stack
    regex(incomplete, Expr2). % We dont know in what state previous regex ended

finalize_nfa @
nfa_finalize, nest_stack([[Symbol,From,incomplete]|Rest]) <=>
    new_nfa_state(To)
    |
    transition(Symbol,From,To),
    nfa_accept_state(To),
    nest_stack(Rest),
    nfa_finalize.
	
report_accepting @
nfa_report, transition(Symbol, From, To), nfa_accept_state(To) <=>
    write('Final transition: '),
    write(transition(Symbol,From,To)),write(' (accepting)'), nl,
    assert(nfa(transition(Symbol,From,To))),
    assert(nfa(accept_state(To)))
    |
    nfa_report.
	
report_transitions @
nfa_report, transition(Symbol, From, To) <=>
    write('Final transition: '),
    write(transition(Symbol,From,To)),nl,
    assert(nfa(transition(Symbol,From,To)))
    |
    nfa_report.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 3:
% Convert a NFA to a DFA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- chr_constraint dfa_start/1,
	          dfa_start_state/1,
		  dfa_accept_state/1,
		  epsilon_closure/2,
		  dfa_transition/3,
		  dfa_report/0,
		  open_transition/4,
		  open_state/1, 
		  marked_state/1,
		  unmarked_state/1,
		  replace_state/2.

start_dfa_generation @
dfa_start(NS1), transition(epsilon,NS1,To) <=>
    new_dfa_state(DS1),
    write('-------------------- START DFA GENERATION -----------------------'),nl,
    write('adding closure(start): '),nl,
    write(epsilon_closure(DS1,[To])),nl
    |
    dfa_start_state(DS1),
    epsilon_closure(DS1,[To]),
    open_state(DS1).

%%% The epsilon-closure part
remove_dups @
epsilon_closure(A,B) \ epsilon_closure(A,B) <=> true.

add_eclosure_transition @
transition(epsilon, From, To) \ epsilon_closure(DfaState, FromStates) <=>
    member(From, FromStates),
    not(member(To,FromStates)),
    write('- adding closure: '),nl,
    write(epsilon_closure(DfaState,[To|FromStates])), nl
    |
    epsilon_closure(DfaState,[To|FromStates]).

merge_epsilon_closures @
epsilon_closure(State, NS1), epsilon_closure(State, NS2) <=>
    union(NS1, NS2, NSAll),
    write('merging closures: '),nl,
    write(epsilon_closure(State,NS1)), write(','),
    write(epsilon_closure(State,NS2)), write(' <=> '),
    write(epsilon_closure(State,NSAll)), nl
    |
    epsilon_closure(State,NSAll).

%%% The move part

merge_open_transitions @
open_transition(State,Symbol,From1,To1), open_transition(State,Symbol,From2,To2) <=>
    union(From1, From2, From3), % Not really necesary
    union(To1, To2, To3),
    write('merging open transitions'),nl
    |
    open_transition(State,Symbol,From3,To3).

collapse_dfa_states1 @
epsilon_closure(OtherState, ReachableSet) \
unmarked_state(SomeState), open_state(NewState), dfa_transition(Symbol,FromState,NewState), epsilon_closure(NewState, ReachableSet) <=>
    OtherState \= NewState, % Obviously, or we'll have endless recursion
    write('!! merging transition to existing state (and opening new state)'),nl
    |
    dfa_transition(Symbol,FromState, OtherState),
    replace_state(NewState,OtherState),	
    open_state(SomeState).

collapse_dfa_states2 @
epsilon_closure(OtherState, ReachableSet) \
open_state(NewState), dfa_transition(Symbol,FromState,NewState), epsilon_closure(NewState, ReachableSet) <=>
    OtherState \= NewState, % Obviously, or we'll have endless recursion
    write('!! transition to existing state'),nl,
    write(dfa_transition(Symbol,FromState,NewState)),write(' <=> '),
    write(dfa_transition(Symbol,FromState,OtherState)),nl
    |
    dfa_transition(Symbol,FromState, OtherState),
    replace_state(NewState,OtherState).

replace_dangling_transitions @
replace_state(OldState,NewState) \ dfa_transition(Symbol,SomeState,OldState) <=>
    write('replacing transition from replaced state:'),nl,
    write(dfa_transition(Symbol,SomeState,OldState)),write(' <=> '),
    write(dfa_transition(Symbol,SomeState,NewState)),nl
    |
    dfa_transition(Symbol,SomeState,NewState).

propagate_open_transitions @
open_state(State), epsilon_closure(State,NFromStates), transition(Symbol,NFrom,NTo) ==>
    Symbol \= epsilon,
    member(NFrom,NFromStates),
    write('propagating open_transition:'),
    write(open_transition(State,Symbol,[NFrom],[NTo])),nl
    |
    open_transition(State,Symbol,[NFrom],[NTo]).

open_dfa_state @ % When we have generated all open_transitions
open_state(State) <=> write('mark open state: '), write(State), nl | marked_state(State).

create_dfa_state @
marked_state(State), open_transition(State,Symbol,_,To) <=>
    new_dfa_state(NewState),
    write('new DFA state: '), write(NewState),nl,
    write(dfa_transition(Symbol, State, NewState)), nl
    |
    epsilon_closure(NewState, To),
    dfa_transition(Symbol, State, NewState),
    marked_state(State), 
    unmarked_state(NewState).

open_next_state @
dfa_transition(_,State,NewState), marked_state(State) \ unmarked_state(NewState) <=>
    write('opening state: '), write(NewState), nl
    |
    open_state(NewState).

dfa_accept_states @
dfa_transition(_,_,DState), nfa_accept_state(NState), epsilon_closure(DState,NStates) ==>
    member(NState, NStates)
    |
    dfa_accept_state(DState).
	
%%% DFA reporting - not part of core algorithm %%%

dfa_report, epsilon_closure(X,Y) <=>
    write(' > '),
    write(epsilon_closure(X,Y)), nl,
    dfa_report.

dfa_report, dfa_transition(X,Y,Z) <=>
    write(' > '),
    write(dfa_transition(X,Y,Z)), nl,
    assert(dfa(transition(X,Y,Z))),
    dfa_report.

dfa_report, dfa_start_state(State) <=>
    assert(dfa(start_state(State)))
    |
    dfa_report.

dfa_report, dfa_accept_state(State) <=>
    write(dfa_accept_state(State)),nl,
    assert(dfa(accept_state(State)))
    |
    dfa_report.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% regular expression to markov chain like format
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Pushes transition symbols "forward" to state names in order to
% look like a markov chain.

mm_state_name(Symbol, OrigStateName, NewName) :-
	atom_concat(Symbol, '(', S1),
	atom_concat(S1, OrigStateName, S2),
	atom_concat(S2, ')', NewName).

push_transitions(StartState, [[Symbol,From,To]|T1Rest], [trans(A,B)|T2Rest]) :-
	mm_state_name(Symbol,To, 
	
dfa_to_mm([[StartState], AcceptingStates, Transitions], MM) :-
	

dfa_transition(Symbol, S1, S2), dfa_start_state(S1) ==>
    mm_state_name(Symbol,S2,Name)
    |
    mm_transition(start, Name).

dfa_transition(Symbol, S1, S2)  ==>
    mm_state_name(Symbol,S2,Name)
    |
    mm_transition(S2,Name).

mm_report, mm_transition(A,B) <=>
    write(mm_transition(A,B))
    |
    mm_report.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Prolog Helper rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
new_nfa_state(State) :-
    catch(prev_nfa_state(Y),_,(Y is 0, assert(prev_nfa_state(Y)))),
    X is Y + 1,
    retract(prev_nfa_state(Y)),
    assert(prev_nfa_state(X)),
    atom_concat(nfa_s, X, State).

new_dfa_state(State) :-
    catch(prev_dfa_state(Y),_,(Y is 0, assert(prev_dfa_state(Y)))),
    X is Y + 1,
    retract(prev_dfa_state(Y)),
    assert(prev_dfa_state(X)),
    atom_concat(dfa_s, X, State).

re2nfa(Regex, NFA) :-
    retractall(nfa(_)),
    retractall(prev_nfa_state(_)),	
    assert(prev_nfa_state(0)),
    regexp(Sexp, Regex, []),
    write('Producing NFA for regex s-exp: '), write(Sexp), nl,
    regex(start, Sexp),
    nfa_finalize,
    nfa_report,
    findall([Symbol,To,From],nfa(transition(Symbol,To,From)),Transitions),
    findall(State,nfa(accept_state(State)),AcceptStates),
    NFA = [[nfa_s1],AcceptStates,Transitions].

re2dfa(Regex,DFA) :-
    retractall(nfa(_)),
    retractall(prev_nfa_state(_)),
    assert(prev_nfa_state(0)),
    regexp(Sexp, Regex, []),
    write('Producing NFA for regex s-exp: '), write(Sexp), nl,
    regex(start, Sexp),
    nfa_finalize,
    retractall(prev_dfa_state(_)),
    assert(prev_dfa_state(0)),
    retractall(dfa(_)),
    dfa_start(nfa_s1),
    dfa_report,
    dfa(start_state(StartState)),
    findall(State,dfa(accept_state(State)),AcceptStates),
    findall([Symbol,To,From],dfa(transition(Symbol,To,From)),Transitions),
    DFA = [ [StartState], AcceptStates, Transitions ].

% FIXME:
re2markov_chain(Regex, MarkovChain) :-
    re2dfa(Regex, DFA),
    MarkovChain = DFA.

viewnfa(Regex) :-
    re2nfa(Regex,NFA),
    fa_graphviz(NFA,'nfa.dot'),
    viewdot('nfa.dot').

viewdfa(Regex) :-
    re2dfa(Regex,DFA),
    fa_graphviz(DFA,'dfa.dot'),
    viewdot('dfa.dot').
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Convert FA to graphviz dot file

write_gv_edges([]).

write_gv_edges([[Symbol,From,To]|Rest]) :-
    write_gv_edge(Symbol,From,To),
    write_gv_edges(Rest).

write_gv_edge(Symbol,To,From) :-
    write('\t"'),
    write(To),
    write('" -> "'),
    write(From),
    write('" [label="'),
    write(Symbol),
    write('"]'),
    nl.

write_space_separated([]).
write_space_separated([Elem|Rest]) :-
    write(Elem),
    write(' '),
    write_space_separated(Rest).

fa_graphviz([StartStates,AcceptStates,Edges],OutputFile) :-
    tell(OutputFile),
    write('digraph FA {'),nl,
    write('\trankdir=LR'),nl,
    write('node [shape = doublecircle]; '),
    write_space_separated(StartStates),
    write_space_separated(AcceptStates),
    write(';'),
    nl,
    write('node [shape = circle];'),
    nl,
    write_gv_edges(Edges),
    write('}'),
    nl,
    told.

viewdot(DotFile) :-
    atom_concat('cat ', DotFile, DotCmd1),
    atom_concat(DotCmd1, ' | dot -Tpng -oviewdot.png',DotCmd),
    shell(DotCmd),
    shell('open viewdot.png').
