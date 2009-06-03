repetion([repeat,S]) -->
	grouped_sequence(S),
	op_kleene_star.

repetion([select, S, [ repeat, S ]]) -->
	grouped_sequence(S),
	op_plus.

repetion([select, S, []]) -->
	grouped_sequence(S),
	op_questionmark.

selection([select,S1, S2]) -->
	sequence(S1),
	op_pipe,
	sequence(S2).

selection([select,S1, S2]) -->
	sequence(S1),
	op_pipe,
	selection(S2).

grouped_sequence([S]) -->
	symbol(S).

grouped_sequence(S) -->
	startparan,
	symbol_list(S),
	end_paran.

symbol_list_or_grouped_sequence(S) -->
	symbol_list(S).

symbol_list_or_grouped_sequence(S) -->
	grouped_sequence(S).

symbol_list([S]) -->
	symbol(S).

symbol_list([S|R]) -->
	symbol(S),
	symbol_list(Rest).



