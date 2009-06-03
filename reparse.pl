%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A simple DCG for parsing regular expressions
% Christian Theil Have, April 2009
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A parameter is used to build the parsetree
% of the regular expression as a s-expression
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

% A few symbols
symbol(epsilon) --> [epsilon].
symbol(a) --> [a].
symbol(b) --> [b].
symbol(c) --> [c].
symbol(d) --> [d].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Utilities for testing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A simple testing rule:
re(Exp) :-
	regexp(ParseTree,Exp, []),
	write(ParseTree),nl.

% Testcases that verify that the grammar works correctly
reparse_test :-
	write('test concatenation: '),
	(regexp([concat,a,[concat,b,[concat,c,[concat,d,[]]]]],[a,b,c,d],[]) ->
	 write('OK'),nl ; write('Fail'),nl),
	
	write('test repetition 1: '),
	(regexp([star,a], [a,*],[]) ->
	 write('OK'),nl ; write('Fail'),nl),

	write('test combine selection and concatenation: '),
	(regexp([concat,[star,a],[concat,[star,b],[]]], [a,*,b,*],[]) ->
	 write('OK'),nl ; write('Fail'),nl),

	write('(a|b)* - '),
	(regexp([star,[or,[concat,a,[]],[concat,b,[]]]], ['(',a,or,b,')',star],[]) ->
	 write('OK'),nl ; write('Fail'),nl),

	write('(a|b)*|(b|c)* - '),
	(regexp([or,[star,[or,[concat,a,[]],[concat,b,[]]]],[star,[or,[concat,b,[]],[concat,c,[]]]]],
		['(',a,or,b,')',star,or,'(',b,or,c,')',*],[]) ->
	 write('OK'),nl ; write('Fail'),nl).
