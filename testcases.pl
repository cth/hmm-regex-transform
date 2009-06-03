% Works:
regexnfa([select,['a','b'],['c','d']]).

regexnfa([select,[kleene,['a','b']],['c','d']]).

regexnfa([select,[kleene,['a','b']],['c','d']]).

% works
regexnfa(['a',[kleene,['a','b']]]).

% works
regexnfa(['a',[kleene,['a','b']], 'b']).

regexnfa([[kleene, [a, b]], b]).
