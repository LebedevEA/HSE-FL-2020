transition A :- (state A), (makes0 X).
transition A :- (state A), (state B), (makes1 X), (next A B).
transition B :- (state A), (state B), (makes2 X), (next A B).
