:- [reparse].
:- [re2nfa].

re2nfa(Regex, NFA) :-
	assert(prevstate(0)),
	regex(start, Regex),
	report_nfa,
	findall([Symbol,To,From],nfa_transition(Symbol,To,From),NFA),
	retractall(prevstate(_)),
	retractall(nfa_transition(_,_,_)).

regexnfa(Regex) :-
	regexp(Sexp, Regex, []),
	regex_to_nfa(Sexp,NFA),
	nfa_graphviz(NFA,'nfa.dot'),
	viewdot('nfa.dot').

