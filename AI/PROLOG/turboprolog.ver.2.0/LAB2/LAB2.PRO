domains
    point = point(real, real, real).
    edge  = edge(point, point).
    points = point*.
    edges  = edge*.
    roots = no_root; one_root(real); two_roots(real, real).

predicates
    main
    read_ellipsoid(point, real, real, real)
    read_ellipsoid_valid(point, real, real, real)
    valid_axes(real, real, real)
    read_cube_vertices(points)
    read_cube_vertices_valid(points)
    read_vertex(point)
    join_points(points, points, points)
    quad_solve(real, real, real, roots)
    on_segment_t(real)
    not_on_segment_t(real)
    ellipsoid_edge_intersections(point, real, real, real, point, point, points)
    points_from_roots(roots, point, point, points)
    maybe_point(real, point, point, points)
    collect_edge_points(point, real, real, real, edges, points, points)
    unique_points(points, real, points)
    exists_close(point, points, real)
    dist2(point, point, real)
    min_nonzero_dist2(points, real)
    min_nonzero_dist2_acc(points, real, real)
    min_from_p_to_rest_nonzero(point, points, real, real)
    select_min_pos(real, real, real)
    count_pairs_close_to(points, real, real, real)
    count_pairs_close_from(point, points, real, real, real)
    close_val(real, real, real)
    build_edges_auto(points, edges)
    build_edges_from_length(points, real, real, edges, edges)
    print_points(points)

clauses

main :-
    write("Программа: точки пересечения эллипсоида и куба"), nl,
    read_ellipsoid_valid(Center, A, B, C),
    read_cube_vertices_valid(Verts),
    build_edges_auto(Verts, Edges),
    collect_edge_points(Center, A, B, C, Edges, [], RawPts),
    unique_points(RawPts, 1E-6, Pts),
    write("Найденные точки пересечения:"), nl,
    print_points(Pts).

read_ellipsoid(point(Cx,Cy,Cz), A, B, C) :-
    write("Введите центр эллипсоида (x y z): "), readreal(Cx), readreal(Cy), readreal(Cz),
    write("Введите полуоси эллипсоида (a b c) > 0: "), readreal(A), readreal(B), readreal(C).

read_ellipsoid_valid(Center, A, B, C) :-
    read_ellipsoid(Center, A, B, C),
    valid_axes(A, B, C).
read_ellipsoid_valid(Center, A, B, C) :-
    write("Ошибка: полуоси должны быть положительными. Повторите ввод."), nl,
    read_ellipsoid_valid(Center, A, B, C).

valid_axes(A, B, C) :-
    A > 0.0, B > 0.0, C > 0.0.

read_cube_vertices([V1,V2,V3,V4,V5,V6,V7,V8]) :-
    write("Введите 8 вершин куба. Каждую вершину как три числа через пробел: x y z"), nl,
    write("Вершина 1 (x y z): "), read_vertex(V1),
    write("Вершина 2 (x y z): "), read_vertex(V2),
    write("Вершина 3 (x y z): "), read_vertex(V3),
    write("Вершина 4 (x y z): "), read_vertex(V4),
    write("Вершина 5 (x y z): "), read_vertex(V5),
    write("Вершина 6 (x y z): "), read_vertex(V6),
    write("Вершина 7 (x y z): "), read_vertex(V7),
    write("Вершина 8 (x y z): "), read_vertex(V8).

read_cube_vertices_valid(Verts) :-
    read_cube_vertices(Verts),
    build_edges_auto(Verts, _).
read_cube_vertices_valid(Verts) :-
    write("Ошибка: заданные 8 точек не образуют корректный куб. Повторите ввод."), nl,
    read_cube_vertices_valid(Verts).

read_vertex(point(X,Y,Z)) :-
    readreal(X), readreal(Y), readreal(Z).

join_points([], B, B).
join_points([H|T], B, [H|R]) :- join_points(T, B, R).

quad_solve(A,B,C, Roots) :-
    Eps = 1E-9,
    abs(A) =< Eps,
    abs(B) =< Eps,
    Roots = no_root.

quad_solve(A,B,C, Roots) :-
    Eps = 1E-9,
    abs(A) =< Eps,
    abs(B) > Eps,
    T = -C / B,
    Roots = one_root(T).

quad_solve(A,B,C, Roots) :-
    Eps = 1E-9,
    abs(A) > Eps,
    D = B*B - 4.0*A*C,
    D < -Eps,
    Roots = no_root.

quad_solve(A,B,C, Roots) :-
    Eps = 1E-9,
    abs(A) > Eps,
    D = B*B - 4.0*A*C,
    abs(D) =< Eps,
    T0 = -B / (2.0*A),
    Roots = one_root(T0).

quad_solve(A,B,C, Roots) :-
    Eps = 1E-9,
    abs(A) > Eps,
    D = B*B - 4.0*A*C,
    D > Eps,
    S = sqrt(D),
    T1 = (-B - S) / (2.0*A),
    T2 = (-B + S) / (2.0*A),
    Roots = two_roots(T1, T2).

on_segment_t(T) :-
    Eps = 1E-9, T >= -Eps, T =< 1.0 + Eps.

not_on_segment_t(T) :-
    Eps = 1E-9, (T < -Eps ; T > 1.0 + Eps).

ellipsoid_edge_intersections(point(Cx,Cy,Cz), A,B,C, point(X1,Y1,Z1), point(X2,Y2,Z2), Points) :-
    DX = X2 - X1, DY = Y2 - Y1, DZ = Z2 - Z1,
    AX = DX / A, BX = (X1 - Cx) / A,
    AY = DY / B, BY = (Y1 - Cy) / B,
    AZ = DZ / C, BZ = (Z1 - Cz) / C,
    Qa = AX*AX + AY*AY + AZ*AZ,
    Qb = 2.0*(AX*BX + AY*BY + AZ*BZ),
    Qc = BX*BX + BY*BY + BZ*BZ - 1.0,
    quad_solve(Qa,Qb,Qc, R),
    points_from_roots(R, point(X1,Y1,Z1), point(X2,Y2,Z2), Points).

points_from_roots(no_root, _, _, []).
points_from_roots(one_root(T0), P1, P2, Points) :-
    maybe_point(T0, P1, P2, Points).
points_from_roots(two_roots(Ta,Tb), P1, P2, Points) :-
    maybe_point(Ta, P1, P2, Pts1),
    maybe_point(Tb, P1, P2, Pts2),
    join_points(Pts1, Pts2, Points).

maybe_point(T, point(X1,Y1,Z1), point(X2,Y2,Z2), [point(X,Y,Z)]) :-
    on_segment_t(T),
    DX = X2 - X1, DY = Y2 - Y1, DZ = Z2 - Z1,
    X = X1 + T*DX, Y = Y1 + T*DY, Z = Z1 + T*DZ.
maybe_point(T, _, _, []) :-
    not_on_segment_t(T).

collect_edge_points(_,_,_,_, [], Acc, Acc).
collect_edge_points(Center, A,B,C, [edge(P1,P2) | T], Acc, Out) :-
    ellipsoid_edge_intersections(Center, A,B,C, P1, P2, Pts),
    join_points(Acc, Pts, Acc2),
    collect_edge_points(Center, A,B,C, T, Acc2, Out).

unique_points([], _, []).
unique_points([P | Rest], Tol, Unique) :-
    unique_points(Rest, Tol, TailU),
    ( exists_close(P, TailU, Tol) -> Unique = TailU
    ; Unique = [P | TailU]
    ).

exists_close(point(X,Y,Z), [point(X2,Y2,Z2) | T], Tol) :-
    DX = X - X2, DY = Y - Y2, DZ = Z - Z2,
    D2 = DX*DX + DY*DY + DZ*DZ,
    ( D2 =< Tol*Tol -> true ; exists_close(point(X,Y,Z), T, Tol) ).
exists_close(_, [], _) :- fail.

dist2(point(X1,Y1,Z1), point(X2,Y2,Z2), D2) :-
    DX = X1 - X2, DY = Y1 - Y2, DZ = Z1 - Z2,
    D2 = DX*DX + DY*DY + DZ*DZ.

min_nonzero_dist2(Points, MinD2) :-
    min_nonzero_dist2_acc(Points, -1.0, MinD2).

min_nonzero_dist2_acc([], Curr, Curr).
min_nonzero_dist2_acc([P | Rest], Curr, Out) :-
    min_from_p_to_rest_nonzero(P, Rest, -1.0, M1),
    select_min_pos(Curr, M1, M2),
    min_nonzero_dist2_acc(Rest, M2, Out).

min_from_p_to_rest_nonzero(_, [], Curr, Curr).
min_from_p_to_rest_nonzero(P, [Q | T], Curr, Out) :-
    dist2(P, Q, D2),
    Eps = 1E-9,
    ( D2 =< Eps -> Curr2 = Curr ; select_min_pos(Curr, D2, Curr2) ),
    min_from_p_to_rest_nonzero(P, T, Curr2, Out).

select_min_pos(Curr, D, Out) :-
    ( Curr < 0.0 -> Out = D
    ; ( D < 0.0 -> Out = Curr
      ; ( D < Curr -> Out = D ; Out = Curr )
      )
    ).

count_pairs_close_to([], _, _, 0.0).
count_pairs_close_to([P | Rest], Target, Tol, Count) :-
    count_pairs_close_from(P, Rest, Target, Tol, C1),
    count_pairs_close_to(Rest, Target, Tol, C2),
    Count = C1 + C2.

count_pairs_close_from(_, [], _, _, 0.0).
count_pairs_close_from(P, [Q | T], Target, Tol, Count) :-
    dist2(P, Q, D2),
    ( close_val(D2, Target, Tol) -> C0 = 1.0 ; C0 = 0.0 ),
    count_pairs_close_from(P, T, Target, Tol, Cn),
    Count = C0 + Cn.

close_val(V, Target, Tol) :-
    Diff = abs(V - Target),
    B = abs(Target),
    S = 1.0 + B,
    Diff =< Tol * S.

build_edges_auto(Verts, Edges) :-
    Tol = 1E-6,
    min_nonzero_dist2(Verts, L2),
    L2 > 0.0,
    count_pairs_close_to(Verts, L2, Tol, C1),
    T2 = 2.0 * L2, count_pairs_close_to(Verts, T2, Tol, C2),
    T3 = 3.0 * L2, count_pairs_close_to(Verts, T3, Tol, C3),
    C1 = 12.0, C2 = 12.0, C3 = 4.0,
    build_edges_from_length(Verts, L2, Tol, [], Edges).

build_edges_from_length([], _, _, Acc, Acc).
build_edges_from_length([P | Rest], L2, Tol, Acc, Out) :-
    build_edges_from_length(Rest, L2, Tol, Acc, Out1),
    add_edges_from_point(P, Rest, L2, Tol, Out1, Out).

predicates
    add_edges_from_point(point, points, real, real, edges, edges).

clauses
add_edges_from_point(_, [], _, _, Acc, Acc).
add_edges_from_point(P, [Q | T], L2, Tol, Acc, Out) :-
    dist2(P, Q, D2),
    ( close_val(D2, L2, Tol) ->
        add_edges_from_point(P, T, L2, Tol, [edge(P,Q) | Acc], Out)
    ;
        add_edges_from_point(P, T, L2, Tol, Acc, Out)
    ).

print_points([]) :-
    write("нет точек"), nl.
print_points([point(X,Y,Z) | T]) :-
    write("("), write(X), write(", "), write(Y), write(", "), write(Z), write(")"), nl,
    print_points(T).