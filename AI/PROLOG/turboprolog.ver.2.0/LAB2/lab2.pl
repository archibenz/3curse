:- use_module(library(readutil)).
:- set_prolog_flag(double_quotes, string).

main :-
    write("Программа: точки пересечения эллипсоида и куба"), nl,
    read_center(Cx,Cy,Cz),
    read_axes(A,B,C),
    read_vertices(Vs),
    ( cube_valid(Vs,L2) ->
        edges_by_length(Vs,L2,1e-6,Edges),
        intersections(p(Cx,Cy,Cz),A,B,C,Edges,[],Raw),
        unique_points(Raw,1e-6,Out),
        write("Найденные точки пересечения:"), nl,
        print_points(Out)
    ; write("Ошибка: введённые точки не образуют корректный куб."), nl
    ).

read_center(Cx,Cy,Cz) :-
    write("Введите центр эллипсоида (пример: 0 0 0 или (0,0,0) или [0,0,0]): "),
    read_line_to_string(user_input,S),
    parse_point(S,p(Cx,Cy,Cz)) -> true ; read_center(Cx,Cy,Cz).

read_axes(A,B,C) :-
    write("Введите полуоси эллипсоида (пример: 3 2 1 или (3,2,1) или [3,2,1]): "),
    read_line_to_string(user_input,S),
    parse_point(S,p(A,B,C)),
    A>0, B>0, C>0, !.
read_axes(A,B,C) :- write("Ошибка: полуоси должны быть > 0."), nl, read_axes(A,B,C).

read_vertices(Vs) :-
    write("Введите 8 вершин куба: одной строкой или по одной строке."), nl,
    write("Если одной строкой, подойдёт формат: (-1,-1,-1),(1,-1,-1),..."), nl,
    write("Если построчно, просто введите первую вершину: "),
    read_line_to_string(user_input,S),
    numbers_in_string(S,Nums1),
    ( length(Nums1,N), N>=24 ->
        take_first_24(Nums1,N24),
        triples_to_points(N24,Vs)
    ; length(Nums1,3) ->
        triples_to_points(Nums1,[P1]),
        read_more_points(7,[P1],Vs)
    ; read_more_points(8,[],Vs)
    ).

read_more_points(0,Acc,Vs) :- reverse(Acc,Vs).
read_more_points(K,Acc,Vs) :-
    write("Вершина "), K1 is 9-K, write(K1), write(" (x y z или (x,y,z) или [x,y,z]): "),
    read_line_to_string(user_input,S),
    parse_point(S,P) -> (K2 is K-1, read_more_points(K2,[P|Acc],Vs))
    ; write("Неверный ввод точки."), nl, read_more_points(K,Acc,Vs).

parse_point(S,p(X,Y,Z)) :-
    numbers_in_string(S,Nums),
    Nums=[X,Y,Z].

numbers_in_string(S,Ns) :-
    split_string(S,"(),;[] \t"," \t",Parts),
    exclude(=(""),Parts,NonEmpty),
    findall(N,(member(T,NonEmpty), catch(number_string(N,T),_,fail)),Ns).

take_first_24(Ns,N24) :- length(N24,24), append(N24,_,Ns).

triples_to_points([],[]).
triples_to_points([X,Y,Z|T],[p(X,Y,Z)|R]) :- triples_to_points(T,R).

cube_valid(Vs,L2) :-
    all_pair_d2(Vs,Ds),
    min_pos(Ds,L2),
    L2>0,
    count_close(Ds,L2,1e-6,C1),
    T2 is 2.0*L2, count_close(Ds,T2,1e-6,C2),
    T3 is 3.0*L2, count_close(Ds,T3,1e-6,C3),
    C1=:=12, C2=:=12, C3=:=4.

edges_by_length(Vs,L2,Tol,Edges) :-
    findall(edge(Pi,Pj),
        (nth1(I,Vs,Pi), nth1(J,Vs,Pj), I<J, dist2(Pi,Pj,D2), close_val(D2,L2,Tol)),
        Edges).

intersections(C,A,B,D,[],Acc,Acc).
intersections(C,A,B,D,[edge(P1,P2)|T],Acc,Out) :-
    edge_ellipsoid_points(C,A,B,D,P1,P2,Pts),
    append(Acc,Pts,Acc2),
    intersections(C,A,B,D,T,Acc2,Out).

edge_ellipsoid_points(p(Cx,Cy,Cz),A,B,C,p(X1,Y1,Z1),p(X2,Y2,Z2),Pts) :-
    DX is X2-X1, DY is Y2-Y1, DZ is Z2-Z1,
    AX is DX/A, BX is (X1-Cx)/A,
    AY is DY/B, BY is (Y1-Cy)/B,
    AZ is DZ/C, BZ is (Z1-Cz)/C,
    Qa is AX*AX + AY*AY + AZ*AZ,
    Qb is 2.0*(AX*BX + AY*BY + AZ*BZ),
    Qc is BX*BX + BY*BY + BZ*BZ - 1.0,
    quad_roots(Qa,Qb,Qc,Ts),
    findall(p(X,Y,Z),
        (member(T,Ts), T>=-1e-9, T=<1.0+1e-9,
         X is X1 + T*DX, Y is Y1 + T*DY, Z is Z1 + T*DZ),
        Pts).

quad_roots(A,B,C,[]) :- abs(A)=<1e-9, abs(B)=<1e-9, !.
quad_roots(A,B,C,[T]) :- abs(A)=<1e-9, T is -C/B, !.
quad_roots(A,B,C,[]) :- D is B*B - 4*A*C, D < -1e-9, !.
quad_roots(A,B,C,[T]) :- D is B*B - 4*A*C, abs(D)=<1e-9, T is -B/(2*A), !.
quad_roots(A,B,C,[T1,T2]) :-
    D is B*B - 4*A*C, D > 1e-9, S is sqrt(D),
    T1 is (-B - S)/(2*A), T2 is (-B + S)/(2*A).

unique_points([],_,[]).
unique_points([P|T],Tol,Out) :-
    unique_points(T,Tol,Rest),
    ( exists_close(P,Rest,Tol) -> Out=Rest ; Out=[P|Rest] ).

exists_close(_,[],_) :- fail.
exists_close(p(X,Y,Z),[p(X2,Y2,Z2)|T],Tol) :-
    DX is X-X2, DY is Y-Y2, DZ is Z-Z2,
    D2 is DX*DX + DY*DY + DZ*DZ,
    (D2 =< Tol*Tol -> true ; exists_close(p(X,Y,Z),T,Tol)).

print_points([]) :- write("нет точек"), nl.
print_points([p(X,Y,Z)|T]) :- format("(~6f, ~6f, ~6f)~n",[X,Y,Z]), print_points(T).

dist2(p(X1,Y1,Z1),p(X2,Y2,Z2),D2) :-
    DX is X1-X2, DY is Y1-Y2, DZ is Z1-Z2, D2 is DX*DX + DY*DY + DZ*DZ.

all_pair_d2(Vs,Ds) :-
    findall(D2,(nth1(I,Vs,Pi), nth1(J,Vs,Pj), I<J, dist2(Pi,Pj,D2)),Ds).

min_pos([H|T],M) :- (H>1e-12 -> M1=H ; M1=inf), min_pos_acc(T,M1,M).
min_pos_acc([],M,M).
min_pos_acc([H|T],C,M) :-
    (H=<1e-12 -> C1=C
    ; C==inf -> C1=H
    ; H<C -> C1=H
    ; C1=C),
    min_pos_acc(T,C1,M).

count_close([],_,_,0).
count_close([D|T],Target,Tol,Count) :-
    count_close(T,Target,Tol,C2),
    (close_val(D,Target,Tol) -> Count is C2+1 ; Count=C2).

close_val(V,Target,Tol) :-
    Diff is abs(V-Target),
    B is abs(Target),
    S is 1.0 + B,
    Diff =< Tol*S.