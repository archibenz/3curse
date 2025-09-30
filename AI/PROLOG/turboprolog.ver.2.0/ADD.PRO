DOMAINS
  X,Y,Z = real
PREDICATES
  add
CLAUSES
  add:-
    write("X = "),nl,
    readreal(X),
    write("Y = "),nl,
    readreal(Y),
    Z=X+Y,
    write("Sum= ",Z),nl.