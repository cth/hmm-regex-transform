

:- chr_constraint dotest/0, tst/0.

dotest ==>
  tst, tst.

tst, tst <=> write('double tst rule') | true.

tst <=> write('single tst rule') | true.


test :-
  dotest.
