:- use_module(library(readutil)).
:- use_module(library(lists)).

kmp_file(Input, Output) :-
    read_strings_from_file(Input, PatternStr, TextStr, Extra),
    kmp_search_all(PatternStr, TextStr, Positions),
    open(Output, write, Stream),
    write(Stream, Positions),
    nl(Stream),
    close(Stream),
    ( Extra == true ->
        writeln('Warning: extra lines in input file were ignored.')
    ; true ).

read_strings_from_file(File, Pattern, Text, Extra) :-
    open(File, read, Stream),
    read_line_to_string(Stream, Pattern0),
    read_line_to_string(Stream, Text0),
    ( read_line_to_string(Stream, _) -> Extra = true ; Extra = false ),
    close(Stream),
    Pattern = Pattern0,
    Text = Text0.

kmp_search_all(PatternStr, TextStr, Positions1) :-
    string_codes(PatternStr, P),
    string_codes(TextStr, T),
    ( P = [] ->
        Positions1 = []
    ; length(P, M),
      build_prefix_function(P, Pi),
      kmp_loop(T, P, Pi, M, 0, 0, [], PosRev),
      reverse(PosRev, Pos0),
      maplist(plus(1), Pos0, Positions1)
    ).

kmp_loop([], _, _, _, _, _, Acc, Acc).
kmp_loop([C|Cs], Pattern, Pi, M, I, J, Acc, Result) :-
    kmp_advance(J, C, Pattern, Pi, J1),
    ( J1 =:= M ->
        Start0 is I - M + 1,
        M1 is M - 1,
        nth0(M1, Pi, JReset),
        I1 is I + 1,
        kmp_loop(Cs, Pattern, Pi, M, I1, JReset, [Start0|Acc], Result)
    ; I1 is I + 1,
      kmp_loop(Cs, Pattern, Pi, M, I1, J1, Acc, Result)
    ).

kmp_advance(0, C, Pattern, _Pi, JOut) :-
    nth0(0, Pattern, P0),
    ( P0 =:= C -> JOut = 1 ; JOut = 0 ).
kmp_advance(JIn, C, Pattern, Pi, JOut) :-
    JIn > 0,
    nth0(JIn, Pattern, PJ),
    ( PJ =:= C ->
        JOut is JIn + 1
    ; JPrevIndex is JIn - 1,
      nth0(JPrevIndex, Pi, JPrev),
      kmp_advance(JPrev, C, Pattern, Pi, JOut)
    ).

build_prefix_function(Pattern, Pi) :-
    length(Pattern, M),
    ( M =:= 0 ->
        Pi = []
    ; build_pi_loop(1, M, Pattern, [0], Pi)
    ).

build_pi_loop(I, M, _, PiAcc, PiAcc) :-
    I >= M.
build_pi_loop(I, M, Pattern, PiAcc, Pi) :-
    I < M,
    nth0(I, Pattern, Ci),
    last(PiAcc, KPrev),
    compute_k(KPrev, Ci, Pattern, PiAcc, K),
    append(PiAcc, [K], PiAcc1),
    I1 is I + 1,
    build_pi_loop(I1, M, Pattern, PiAcc1, Pi).

compute_k(KIn, Ci, Pattern, PiAcc, KOut) :-
    ( KIn > 0 ->
        nth0(KIn, Pattern, PK),
        ( PK =:= Ci ->
            KOut is KIn + 1
        ; KPrevIndex is KIn - 1,
          nth0(KPrevIndex, PiAcc, KPrev),
          compute_k(KPrev, Ci, Pattern, PiAcc, KOut)
        )
    ; nth0(0, Pattern, P0),
      ( P0 =:= Ci -> KOut = 1 ; KOut = 0 )
    ).