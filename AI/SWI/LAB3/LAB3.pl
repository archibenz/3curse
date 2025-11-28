:- use_module(library(readutil)).

sort_file(Input, Output) :-
    read_numbers_from_file(Input, Numbers),
    cocktail_sort(Numbers, Sorted),
    open(Output, write, Stream),
    write(Stream, Sorted),
    nl(Stream),
    close(Stream).

read_numbers_from_file(File, Numbers) :-
    open(File, read, Stream),
    read_stream_to_codes(Stream, Codes),
    close(Stream),
    phrase(numbers(Numbers), Codes).

numbers(Ns) --> blanks, number_list(Ns).
blanks --> [C], { code_type(C, space) }, !, blanks.
blanks --> [].

number_list([N|Ns]) --> number(N), blanks, !, number_list(Ns).
number_list([]) --> [].

number(N) --> sign(S), digits(Ds), { Ds \= [], number_codes(Abs, Ds), N is S*Abs }.
sign(1) --> "+", !.
sign(-1) --> "-", !.
sign(1) --> [].

digits([D|Ds]) --> [D], { code_type(D, digit) }, !, digits(Ds).
digits([]) --> [].

cocktail_sort([], []).
cocktail_sort([X], [X]).
cocktail_sort(List, Sorted) :-
    List = [_, _|_],
    cocktail_step(List, Sorted).

cocktail_step(List, Sorted) :-
    forward_pass(List, AfterForward, false, SwappedForward),
    (   SwappedForward = false
    ->  Sorted = AfterForward
    ;   backward_pass(AfterForward, AfterBackward, false, SwappedBackward),
        (   SwappedBackward = false
        ->  Sorted = AfterBackward
        ;   cocktail_step(AfterBackward, Sorted)
        )
    ).

forward_pass([X], [X], Swapped, Swapped).
forward_pass([X, Y|T], [X|Rest], SwIn, SwOut) :-
    X =< Y,
    forward_pass([Y|T], Rest, SwIn, SwOut).
forward_pass([X, Y|T], [Y|Rest], _SwIn, SwOut) :-
    X > Y,
    forward_pass([X|T], Rest, true, SwOut).

backward_pass([X], [X], Swapped, Swapped).
backward_pass([X, Y|T], [Z|Rest2], SwIn, SwOut) :-
    backward_pass([Y|T], [Y1|TailRest], SwIn, Sw1),
    (   X > Y1
    ->  Z = Y1,
        Rest2 = [X|TailRest],
        SwOut = true
    ;   Z = X,
        Rest2 = [Y1|TailRest],
        SwOut = Sw1
    ).