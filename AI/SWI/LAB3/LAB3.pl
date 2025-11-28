:- use_module(library(readutil)).

sort_file(Input, Output) :-
    read_numbers_from_file(Input, Numbers, Extra),
    cocktail_sort(Numbers, Sorted),
    open(Output, write, Stream),
    write(Stream, Sorted),
    nl(Stream),
    close(Stream),
    ( Extra == true ->
        writeln('Предупреждение: во входном файле были лишние символы, они были проигнорированы.')
    ; true ).

read_numbers_from_file(File, Numbers, Extra) :-
    open(File, read, Stream),
    read_stream_to_codes(Stream, Codes),
    close(Stream),
    scan_codes(Codes, Numbers, Extra).

scan_codes(Codes, Numbers, Extra) :-
    scan_codes(Codes, [], Rev, false, Extra),
    reverse(Rev, Numbers).

scan_codes([], Acc, Acc, Extra, Extra).
scan_codes([C|Cs], AccIn, AccOut, ExtraIn, ExtraOut) :-
    ( code_type(C, space) ->
        scan_codes(Cs, AccIn, AccOut, ExtraIn, ExtraOut)
    ; take_number([C|Cs], Rest, N) ->
        scan_codes(Rest, [N|AccIn], AccOut, ExtraIn, ExtraOut)
    ; Extra1 = true,
      ( ExtraIn == true -> E2 = true ; E2 = Extra1 ),
      scan_codes(Cs, AccIn, AccOut, E2, ExtraOut)
    ).

take_number(Codes, Rest, N) :-
    take_sign_and_digits(Codes, Rest, Sign, Digits),
    Digits \= [],
    number_codes(Abs, Digits),
    N is Sign*Abs.

take_sign_and_digits([C|Cs], Rest, Sign, Digits) :-
    ( (C =:= 0'+ ; C =:= 0'-) ->
        ( C =:= 0'- -> Sign = -1 ; Sign = 1 ),
        collect_digits(Cs, Rest, Digits),
        Digits \= []
    ; code_type(C, digit) ->
        Sign = 1,
        collect_digits(Cs, Rest, More),
        Digits = [C|More]
    ).

collect_digits([C|Cs], Rest, [C|Ds]) :-
    code_type(C, digit), !,
    collect_digits(Cs, Rest, Ds).
collect_digits(Rest, Rest, []).

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