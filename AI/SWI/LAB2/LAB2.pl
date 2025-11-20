/* LAB2.pl
   Эллипсоид ∩ куб: интерактивный ввод, понятные ошибки, валидация геометрии,
   точки пересечения по рёбрам + вершины на поверхности и редкие касания граней (для ось-куба).
*/

:- use_module(library(lists)).
:- use_module(library(readutil)).

% ===== Точка входа =====

run :-
    Eps = 1.0e-6,
    banner,
    ask_ellipsoid(Ell),
    ask_cube_vertices(Vs),
    nl,
    validate_ellipsoid_verbose(Ell),
    validate_cube_verbose(Vs, Eps, EdgeLen, Edges),
    nl,
    format("✓ Валидация пройдена. Считаю пересечение...~n", []),
    intersect_ellipsoid_with_edges(Ell, Vs, Edges, EdgeLen, Eps, RawPts),
    TolDedupe is max(Eps, EdgeLen*1.0e-9),
    dedup_points(RawPts, TolDedupe, Pts),
    nl, format("Результат:~n", []),
    print_points(Pts),
    format("Готово.~n", []).

banner :-
    format("~n=== Пересечение эллипсоида и куба ===~n", []),
    format("Вводи термы Prolog и закрывай точкой. Пример: [0,0,0].~n~n", []).

% ===== Ввод =====

ask_ellipsoid(ellipsoid(Cx,Cy,Cz, Ax,Ay,Az)) :-
    format("Центр эллипсоида [Cx,Cy,Cz]:~n> "),
    ( safe_read(CList) -> true ; fail ),
    ( must_be_point(CList) -> true ; fail ),
    CList = [Cx,Cy,Cz],
    format("Полуоси эллипсоида [Ax,Ay,Az] (>0):~n> "),
    ( safe_read(AList) -> true ; fail ),
    ( must_be_axes(AList) -> true ; fail ),
    AList = [Ax,Ay,Az].

ask_cube_vertices(Vs) :-
    format("Вершины куба списком из 8 точек [[x,y,z],...]:~n> "),
    ( safe_read(Term) -> true ; fail ),
    ( is_list(Term), length(Term,8) ->
        ( map_verify_points(Term, 1) -> Vs = Term ; fail )
      ; format("✗ Ошибка: ожидалось ровно 8 вершин в виде [[x,y,z],...].~n", []), fail
    ).

% Читает ОДНУ строку, отрезает всё после ПЕРВОЙ точки и парсит как терм.
% Лекарство от ввода вида: [0,0,0].], [[[...]]]. и прочих чудес.
safe_read(Term) :-
    read_line_to_string(user_input, S0),
    ( var(S0) -> format("✗ Пустой ввод.~n", []), fail ; true ),
    normalize_space(string(S1), S0),
    ( sub_string(S1, Pos, 1, _, ".") ->
        Len is Pos+1, sub_string(S1, 0, Len, _, S2)
    ;   S2 = S1
    ),
    catch(atom_string(A, S2), _, (format("✗ Не удалось прочитать строку.~n", []), fail)),
    catch(atom_to_term(A, Term, _), E, (format("✗ Синтаксическая ошибка ввода: ~w~n", [E]), fail)).

must_be_point([X,Y,Z]) :-
    number(X), number(Y), number(Z), !.
must_be_point(T) :-
    format("✗ Ошибка формата точки: ожидалось [X,Y,Z], получено: ~w~n", [T]), fail.

map_verify_points([], _).
map_verify_points([[X,Y,Z]|R], I) :-
    ( number(X), number(Y), number(Z) ->
        I1 is I+1, map_verify_points(R, I1)
    ; format("✗ Вершина №~d не число/не формата [X,Y,Z]: ~w~n", [I, [X,Y,Z]]), fail
    ).
map_verify_points([T|_], I) :-
    format("✗ Вершина №~d должна быть [X,Y,Z], получено: ~w~n", [I, T]), fail.

must_be_axes([Ax,Ay,Az]) :-
    ( number(Ax), number(Ay), number(Az) ->
        ( Ax>0, Ay>0, Az>0 -> true
        ; format("✗ Полуоси должны быть > 0. Найдены: Ax=~w, Ay=~w, Az=~w~n", [Ax,Ay,Az]), fail
        )
    ; format("✗ Полуоси должны быть числами [Ax,Ay,Az], получено: ~w~n", [[Ax,Ay,Az]]), fail
    ).
must_be_axes(T) :-
    format("✗ Ошибка формата полуосей: ожидалось [Ax,Ay,Az], получено: ~w~n", [T]), fail.

% ===== Валидация =====

validate_ellipsoid_verbose(ellipsoid(_,_,_, Ax,Ay,Az)) :-
    ( number(Ax), number(Ay), number(Az) -> true
    ; format("✗ Эллипсоид: полуоси должны быть числами.~n", []), fail
    ),
    ( Ax>0, Ay>0, Az>0 -> format("✓ Эллипсоид принят: полуоси положительны.~n", [])
    ; format("✗ Эллипсоид: полуоси должны быть строго > 0. Найдены: Ax=~w, Ay=~w, Az=~w~n", [Ax,Ay,Az]), fail
    ).

% Подробная проверка «это именно куб», а не «прямоугольный параллелепипед»
validate_cube_verbose(Vs, Eps, EdgeLen, Edges) :-
    length(Vs, L),
    ( L =:= 8 -> true
    ; format("✗ Куб: нужно ровно 8 вершин, получено ~d.~n", [L]),
      fail
    ),
    % проверим дубликаты вершин
    sort(Vs, Unique),
    ( length(Unique,8) -> true
    ; find_duplicates(Vs, Dups), format("✗ Куб: есть повторяющиеся вершины: ~w~n", [Dups]), fail
    ),
    % все попарные расстояния
    all_dist2(Vs, Ps, Ds2),
    ( Ds2 \= [] -> true
    ; format("✗ Куб: не удалось вычислить попарные расстояния (пустой список). Проверь формат вершин.~n", []),
      fail
    ),
    ( min_positive(Ds2, E2) -> true
    ; format("✗ Куб: не найдено ни одного положительного расстояния между вершинами. Похоже, вершины совпадают.~n", []),
      fail
    ),
    ( E2 > 0 -> true
    ; format("✗ Куб: минимальная ненулевая дистанция не положительна: ~w~n", [E2]),
      fail
    ),
    classify_distances(Ds2, E2, Eps, Cedge, Cface, Cspace, Cbad),
    ( Cedge=:=12, Cface=:=12, Cspace=:=4, Cbad=:=0 ->
        EdgeLen is sqrt(E2),
        edge_pairs_by_e(Vs, E2, Eps, Edges),
        format("✓ Куб принят. Длина ребра ≈ ~g.~n", [EdgeLen])
    ;   format("✗ Эти точки не образуют куб.~n", []),
        report_if_axis_aligned_box(Vs, Eps),
        format("  Ожидалось: 12 расстояний ≈ e^2, 12 ≈ 2e^2, 4 ≈ 3e^2, без прочих.~n", []),
        format("  Получено:   рёбра=~d, диагонали_грани=~d, диагонали_пространства=~d, прочие=~d.~n",
               [Cedge,Cface,Cspace,Cbad]),
        show_distance_diagnostics(Ds2, E2, Eps, Ps),
        fail
    ).

find_duplicates(L, Dups) :-
    msort(L, S),
    findall(X, (append(_,[X,X|_], S)), Tmp),
    sort(Tmp, Dups).

all_dist2(Vs, Pairs, Ds2) :-
    findall(I-J-D2,
        ( nth1(I,Vs,P1), nth1(J,Vs,P2), I<J, distance2(P1,P2,D2) ),
        Triples),
    findall(D2, member(_-_-D2, Triples), Ds2),
    findall(I-J, member(I-J-_, Triples), Pairs).

min_positive(Ds2, Min) :-
    include(gt_zero, Ds2, Pos),
    sort(Pos, [Min|_]).

gt_zero(X) :- X > 0.0.

classify_distances(Ds2, E2, Eps, Cedge, Cface, Cspace, Cbad) :-
    Tol is max(Eps, E2*1.0e-6),
    E2_2 is 2.0*E2, E2_3 is 3.0*E2,
    foldl(classify_step(Tol,E2,E2_2,E2_3), Ds2, counts(0,0,0,0), counts(Cedge,Cface,Cspace,Cbad)).

classify_step(Tol,E2,E2_2,E2_3, D2, counts(A,B,C,Din), counts(A1,B1,C1,D1)) :-
    ( abs(D2 - E2)   =< Tol -> A1 is A+1, B1=B,   C1=C,   D1=Din
    ; abs(D2 - E2_2) =< Tol -> A1=A,   B1 is B+1, C1=C,   D1=Din
    ; abs(D2 - E2_3) =< Tol -> A1=A,   B1=B,   C1 is C+1, D1=Din
    ; A1=A, B1=B, C1=C, D1 is Din+1
    ).

edge_pairs_by_e(Vs, E2, Eps, Edges) :-
    Tol is max(Eps, E2*1.0e-6),
    findall([P,Q],
        ( nth1(I,Vs,P), nth1(J,Vs,Q), I<J,
          distance2(P,Q,D2),
          abs(D2 - E2) =< Tol
        ),
        Raw),
    maplist(canon_pair, Raw, Canon),
    sort(Canon, S),
    maplist(uncanon_pair, S, Edges).

% Тихо вычисляем рёбра и длину ребра из 8 вершин. Фейлим с понятным сообщением,
% если это не куб (на случай незвязанного Edges в дальнейшем коде).
edges_from_vertices_strict(Vs, Eps, EdgeLen, Edges) :-
    all_dist2(Vs, _Pairs, Ds2),
    min_positive(Ds2, E2),
    E2 > 0,
    classify_distances(Ds2, E2, Eps, Cedge, Cface, Cspace, Cbad),
    ( Cedge=:=12, Cface=:=12, Cspace=:=4, Cbad=:=0 ->
        EdgeLen is sqrt(E2),
        edge_pairs_by_e(Vs, E2, Eps, Edges)
    ;   format("✗ Внутренняя проверка: переданы вершины не-куба (рёбра=~d, диагонали_грани=~d, диагонали_пространства=~d, прочие=~d).~n",
               [Cedge,Cface,Cspace,Cbad]),
        fail
    ).

% Если это ось-выравнённый прямоугольный параллелепипед, сообщим его размеры
report_if_axis_aligned_box(Vs, Eps) :-
    ( cube_extents_axis_aligned(Vs, Eps, Xmin,Xmax,Ymin,Ymax,Zmin,Zmax) ->
        Lx is Xmax - Xmin, Ly is Ymax - Ymin, Lz is Zmax - Zmin,
        format("  Распознано: прямоугольный параллелепипед со сторонами ≈ (~g, ~g, ~g).~n", [Lx,Ly,Lz]),
        ( approx(Lx,Ly, max(Eps,1.0e-9)), approx(Ly,Lz, max(Eps,1.0e-9)) ->
            format("  (Стороны почти равны — возможно, это куб в пределах погрешности.)~n", [])
        ; true )
    ; true ).

show_distance_diagnostics(Ds2, E2, Eps, Pairs) :-
    format("  Диагностика расстояний (показаны первые 10 «прочих»):~n", []),
    Tol is max(Eps, E2*1.0e-6),
    findall(R-I-J,
        ( nth1(K, Ds2, D2), nth1(K, Pairs, I-J),
          R is D2 / E2,
          \+ approx(R,1.0,Tol/E2), \+ approx(R,2.0,Tol/E2), \+ approx(R,3.0,Tol/E2)
        ),
        Ratios),
    ( Ratios = [] ->
        format("  Все расстояния попали в допуски, но счётчики не сошлись (невероятно).~n", [])
    ; take_first_n(Ratios, 10, Sample),
      forall(member(R-I-J, Sample),
            format("   пара (~d,~d): D2/E2 ≈ ~g~n", [I,J,R]))
    ).

approx(A,B,Tol) :- abs(A-B) =< Tol.

take_first_n(L, N, R) :- length(R, N), append(R, _, L), !.
take_first_n(L, _, L).

% ===== Геометрия и пересечения =====

% --- Робастные утилиты для пересечения отрезка с эллипсоидом ---
ellipsoid_F(ellipsoid(Cx,Cy,Cz, Ax,Ay,Az), [X,Y,Z], F) :-
    NX is (X-Cx)/Ax, NY is (Y-Cy)/Ay, NZ is (Z-Cz)/Az,
    F is NX*NX + NY*NY + NZ*NZ - 1.0.

point_on_segment([Ax0,Ay0,Az0],[Bx0,By0,Bz0], T, [Px,Py,Pz]) :-
    Px is Ax0 + T*(Bx0-Ax0),
    Py is Ay0 + T*(By0-Ay0),
    Pz is Az0 + T*(Bz0-Az0).

% Поиск корня методом бисекции на интервале [T0,T1]
segment_root_bisect(Ell, A, B, T0, T1, EpsT, TRoot) :-
    MaxIt = 60,
    segment_root_bisect_(Ell,A,B,T0,T1,EpsT,MaxIt,TRoot).

segment_root_bisect_(Ell,A,B,Lo,Hi,EpsT,_,TRoot) :-
    Width is Hi-Lo,
    ( Width =< EpsT -> TRoot is (Lo+Hi)/2, !
    ; Mid is (Lo+Hi)/2,
      point_on_segment(A,B,Lo,Plo), ellipsoid_F(Ell,Plo,Flo),
      point_on_segment(A,B,Mid,Pmid), ellipsoid_F(Ell,Pmid,Fmid),
      ( Flo =:= 0.0 -> TRoot = Lo, !
      ; Fmid =:= 0.0 -> TRoot = Mid, !
      ; Flo*Fmid =< 0 -> segment_root_bisect_(Ell,A,B,Lo,Mid,EpsT,_,TRoot)
      ; segment_root_bisect_(Ell,A,B,Mid,Hi,EpsT,_,TRoot)
      )
    ).

% Сканирование по t с шагом, сбор брекетов со сменой знака
segment_intersections_scan(Ell, A, B, Eps, TsOut) :-
    Steps = 400,
    Step is 1.0/Steps,
    EpsT is max(1.0e-9, 50*Eps),
    findall([Tlo,Thi],
        ( between(0, Steps-1, K),
          T0 is K*Step, T1 is T0+Step,
          point_on_segment(A,B,T0,P0), ellipsoid_F(Ell,P0,F0),
          point_on_segment(A,B,T1,P1), ellipsoid_F(Ell,P1,F1),
          ( F0*F1 =< 0 -> Tlo = T0, Thi = T1 ; fail )
        ),
        Brackets0),
    findall(T,
        ( member([Lo,Hi], Brackets0),
          segment_root_bisect(Ell,A,B,Lo,Hi,EpsT,T)
        ),
        Ts0),
    sort(Ts0, TsOut).

intersect_ellipsoid_with_edges(Ell, Vs, Edges, EdgeLen, Eps, PointsOut) :-
    % Если Edges не связаны (на всякий случай), тихо пересчитаем их из вершин
    ( var(Edges) ->
        edges_from_vertices_strict(Vs, Eps, EdgeLenS, EdgesS)
    ;   EdgeLenS = EdgeLen,
        EdgesS = Edges
    ),
    % пересечения с рёбрами
    findall(P,
        ( member([A,B], EdgesS),
          segment_ellipsoid_intersections(Ell, A, B, Eps, Ps),
          member(P, Ps)
        ),
        EdgePts0
    ),
    % вершины на поверхности
    findall(V,
        ( member(V, Vs), on_ellipsoid(Ell, V, Eps) ),
        VertexPts0
    ),
    % точки касания к граням (для ось-выравненного куба)
    axis_aligned_face_tangent_points(Ell, Vs, Eps, FaceTangentPts0),
    append([EdgePts0, VertexPts0, FaceTangentPts0], AllPts0),
    TolDedupe is max(Eps, EdgeLenS*1.0e-9),
    dedup_points(AllPts0, TolDedupe, PointsOut).

on_ellipsoid(ellipsoid(Cx,Cy,Cz, Ax,Ay,Az), [X,Y,Z], Eps) :-
    NX is (X-Cx)/Ax, NY is (Y-Cy)/Ay, NZ is (Z-Cz)/Az,
    T is NX*NX + NY*NY + NZ*NZ,
    Abs is abs(T - 1.0),
    Tol is max(50*Eps, 1.0e-6),
    Abs =< Tol.

segment_ellipsoid_intersections(Ell, A, B, Eps, Points) :-
    % 1) Быстрый и точный путь для ось-выравненных рёбер (две координаты постоянны)
    ( seg_int_axis_aligned(Ell, A, B, Eps, Paxis) -> Points = Paxis
    ; % 2) Безопасная оболочка общего метода: любые исключения -> пустой список
      ( catch(seg_int_core(Ell,A,B,Eps,P0), _, P0 = []), !, Points = P0
      ; Points = []
      )
    ).

% Вспомогательный предикат: быстрый пересчёт пересечений для ось-выравненных рёбер
seg_int_axis_aligned(ellipsoid(Cx,Cy,Cz, Ax,Ay,Az), A, B, Eps, Points) :-
    A = [X1,Y1,Z1], B = [X2,Y2,Z2],
    Tol is max(1.0e-9, 50*Eps),
    ( abs(X1-X2) =< Tol, abs(Y1-Y2) =< Tol ->
        % вертикальное ребро: меняется Z
        Const is ((X1-Cx)/Ax)*((X1-Cx)/Ax) + ((Y1-Cy)/Ay)*((Y1-Cy)/Ay),
        R0 is 1.0 - Const,
        ( R0 < -Tol -> Points = []
        ; R is max(0.0, R0), S is sqrt(R),
          Zm is Cz - Az*S, Zp is Cz + Az*S,
          z_in_segment(Z1,Z2,Zm,Tol,Ok1), z_in_segment(Z1,Z2,Zp,Tol,Ok2),
          findall([X1,Y1,Z], ( (Ok1=:=1, Z=Zm); (Ok2=:=1, Z=Zp) ), P0),
          dedup_points(P0, Tol, Points)
        )
    ; abs(X1-X2) =< Tol, abs(Z1-Z2) =< Tol ->
        % ребро вдоль Y
        Const is ((X1-Cx)/Ax)*((X1-Cx)/Ax) + ((Z1-Cz)/Az)*((Z1-Cz)/Az),
        R0 is 1.0 - Const,
        ( R0 < -Tol -> Points = []
        ; R is max(0.0, R0), S is sqrt(R),
          Ym is Cy - Ay*S, Yp is Cy + Ay*S,
          y_in_segment(Y1,Y2,Ym,Tol,Ok1), y_in_segment(Y1,Y2,Yp,Tol,Ok2),
          findall([X1,Y,Z1], ( (Ok1=:=1, Y=Ym); (Ok2=:=1, Y=Yp) ), P0),
          dedup_points(P0, Tol, Points)
        )
    ; abs(Y1-Y2) =< Tol, abs(Z1-Z2) =< Tol ->
        % ребро вдоль X
        Const is ((Y1-Cy)/Ay)*((Y1-Cy)/Ay) + ((Z1-Cz)/Az)*((Z1-Cz)/Az),
        R0 is 1.0 - Const,
        ( R0 < -Tol -> Points = []
        ; R is max(0.0, R0), S is sqrt(R),
          Xm is Cx - Ax*S, Xp is Cx + Ax*S,
          x_in_segment(X1,X2,Xm,Tol,Ok1), x_in_segment(X1,X2,Xp,Tol,Ok2),
          findall([X,Y1,Z1], ( (Ok1=:=1, X=Xm); (Ok2=:=1, X=Xp) ), P0),
          dedup_points(P0, Tol, Points)
        )
    ; fail % не ось-выравненное ребро — пусть обработает общий метод
    ).

x_in_segment(A,B,X,Tol,Ok) :-
    Min is min(A,B)-Tol, Max is max(A,B)+Tol,
    ( X >= Min, X =< Max -> Ok = 1 ; Ok = 0 ).

y_in_segment(A,B,Y,Tol,Ok) :-
    Min is min(A,B)-Tol, Max is max(A,B)+Tol,
    ( Y >= Min, Y =< Max -> Ok = 1 ; Ok = 0 ).

z_in_segment(A,B,Z,Tol,Ok) :-
    Min is min(A,B)-Tol, Max is max(A,B)+Tol,
    ( Z >= Min, Z =< Max -> Ok = 1 ; Ok = 0 ).

seg_int_core(Ell, A, B, Eps, Points) :-
    % Сначала — аналитика по квадратному уравнению
    Ell = ellipsoid(Cx,Cy,Cz, Ax,Ay,Az),
    A = [Ax0,Ay0,Az0], B = [Bx0,By0,Bz0],
    Dx is Bx0 - Ax0,  Dy is By0 - Ay0,  Dz is Bz0 - Az0,
    X0 is (Ax0 - Cx)/Ax,  Y0 is (Ay0 - Cy)/Ay,  Z0 is (Az0 - Cz)/Az,
    dX is Dx/Ax, dY is Dy/Ay, dZ is Dz/Az,
    QA is dX*dX + dY*dY + dZ*dZ,
    QB is 2.0*(X0*dX + Y0*dY + Z0*dZ),
    QC is X0*X0 + Y0*Y0 + Z0*Z0 - 1.0,
    solve_quadratic(QA,QB,QC,Eps, TsA),
    include(in_01(Eps), TsA, TsA01),
    findall(PtA, ( member(Ta, TsA01), point_on_segment(A,B,Ta,PtA) ), PtsA0),
    ( PtsA0 \= [] ->
        Points = PtsA0
    ;   % Фоллбэк — робастный скан + бисекция
        segment_intersections_scan(Ell, A, B, Eps, TsB),
        include(in_01(Eps), TsB, TsB01),
        findall(PtB, ( member(Tb, TsB01), point_on_segment(A,B,Tb,PtB) ), PtsB0),
        Points = PtsB0
    ).

solve_quadratic(A,B,C,Eps, Ts) :-
    ( abs(A) =< Eps ->
        ( abs(B) =< Eps ->
            ( abs(C) =< Eps -> Ts = [0.0, 1.0] ; Ts = [] )
        ;   T is -C / B, Ts = [T]
        )
    ;   D is B*B - 4.0*A*C,
        ( D < -Eps -> Ts = []
        ; abs(D) =< Eps -> T is -B / (2.0*A), Ts = [T]
        ; S is sqrt(max(0.0,D)),
          T1 is (-B - S) / (2.0*A),
          T2 is (-B + S) / (2.0*A),
          Ts = [T1,T2]
        )
    ).

in_01(Eps, T) :- T >= -Eps, T =< 1.0 + Eps.

% ===== Касания к граням (для ось-выравненного куба) =====

axis_aligned_face_tangent_points(Ell, Vs, Eps, Points) :-
    ( cube_extents_axis_aligned(Vs, Eps, Xmin,Xmax,Ymin,Ymax,Zmin,Zmax) ->
        Ell = ellipsoid(Cx,Cy,Cz, Ax,Ay,Az),
        findall([Xc,Cy,Cz],
            ( member(Xc,[Xmin,Xmax]),
              NX is (Xc - Cx)/Ax,
              S is 1.0 - NX*NX, abs(S) =< 5*Eps,
              Cy >= min(Ymin,Ymax)-Eps, Cy =< max(Ymin,Ymax)+Eps,
              Cz >= min(Zmin,Zmax)-Eps, Cz =< max(Zmin,Zmax)+Eps
            ), Xpts),
        findall([Cx,Yc,Cz],
            ( member(Yc,[Ymin,Ymax]),
              NY is (Yc - Cy)/Ay,
              S is 1.0 - NY*NY, abs(S) =< 5*Eps,
              Cx >= min(Xmin,Xmax)-Eps, Cx =< max(Xmin,Xmax)+Eps,
              Cz >= min(Zmin,Zmax)-Eps, Cz =< max(Zmin,Zmax)+Eps
            ), Ypts),
        findall([Cx,Cy,Zc],
            ( member(Zc,[Zmin,Zmax]),
              NZ is (Zc - Cz)/Az,
              S is 1.0 - NZ*NZ, abs(S) =< 5*Eps,
              Cx >= min(Xmin,Xmax)-Eps, Cx =< max(Xmin,Xmax)+Eps,
              Cy >= min(Ymin,Ymax)-Eps, Cy =< max(Ymin,Ymax)+Eps
            ), Zpts),
        append([Xpts,Ypts,Zpts], P0),
        dedup_points(P0, Eps, Points)
    ; Points = []
    ).

cube_extents_axis_aligned(Vs, Eps, Xmin,Xmax,Ymin,Ymax,Zmin,Zmax) :-
    findall(X, member([X,_,_], Vs), Xs),
    findall(Y, member([_,Y,_], Vs), Ys),
    findall(Z, member([_,_,Z], Vs), Zs),
    min_list(Xs, Xmin), max_list(Xs, Xmax),
    min_list(Ys, Ymin), max_list(Ys, Ymax),
    min_list(Zs, Zmin), max_list(Zs, Zmax),
    maplist(coord_on_two_levels(Xmin,Xmax,Eps), Xs),
    maplist(coord_on_two_levels(Ymin,Ymax,Eps), Ys),
    maplist(coord_on_two_levels(Zmin,Zmax,Eps), Zs).

coord_on_two_levels(Mi,Ma,Eps, V) :-
    ( abs(V - Mi) =< 5*Eps ; abs(V - Ma) =< 5*Eps ).

% ===== Утилиты =====

distance2([X1,Y1,Z1],[X2,Y2,Z2],D2) :-
    DX is X2-X1, DY is Y2-Y1, DZ is Z2-Z1,
    D2 is DX*DX + DY*DY + DZ*DZ.

canon_pair([A,B], A1-B1) :- (A @=< B -> A1=A, B1=B ; A1=B, B1=A).
uncanon_pair(A-B, [A,B]).

dedup_points(Points, Eps, Unique) :-
    foldl(dedupe_step(Eps), Points, [], R),
    reverse(R, Unique).

dedupe_step(Eps, P, Acc, Acc) :- member_close(P, Acc, Eps), !.
dedupe_step(_, P, Acc, [P|Acc]).

member_close(P, [Q|_], Eps) :- points_close(P,Q,Eps), !.
member_close(P, [_|R], Eps) :- member_close(P, R, Eps).
member_close(_, [], _) :- fail.

points_close([X1,Y1,Z1], [X2,Y2,Z2], Eps) :-
    distance2([X1,Y1,Z1],[X2,Y2,Z2], D2),
    D is sqrt(D2), D =< Eps.

print_points([]) :-
    format("Точек пересечения нет.~n", []).
print_points(Ps) :-
    length(Ps,N),
    format("Найдено точек: ~d~n", [N]),
    forall(member([X,Y,Z], Ps),
        format("(~g, ~g, ~g)~n", [X,Y,Z])
    ).

% ===== Набор автотестов без интерактива =====

selftest_ok :-
    Eps = 1.0e-6,
    Ell = ellipsoid(0,0,0, 1.5,1.5,0.5),
    Cube = [[-1,-1,-1],[1,-1,-1],[-1,1,-1],[1,1,-1],[-1,-1,1],[1,-1,1],[-1,1,1],[1,1,1]],
    validate_cube_verbose(Cube, Eps, EdgeLen, Edges),
    validate_ellipsoid_verbose(Ell),
    intersect_ellipsoid_with_edges(Ell, Cube, Edges, EdgeLen, Eps, Raw),
    dedup_points(Raw, max(Eps, EdgeLen*1.0e-9), P),
    print_points(P).

selftest_touch :-
    Eps = 1.0e-6,
    Ell = ellipsoid(0,0,0, 1,1,1),
    Cube = [[-1,-1,-1],[1,-1,-1],[-1,1,-1],[1,1,-1],[-1,-1,1],[1,-1,1],[-1,1,1],[1,1,1]],
    validate_cube_verbose(Cube, Eps, EdgeLen, Edges),
    validate_ellipsoid_verbose(Ell),
    intersect_ellipsoid_with_edges(Ell, Cube, Edges, EdgeLen, Eps, Raw),
    dedup_points(Raw, max(Eps, EdgeLen*1.0e-9), P),
    print_points(P).

selftest_none :-
    Eps = 1.0e-6,
    Ell = ellipsoid(3,0,0, 0.4,0.4,0.4),
    Cube = [[-1,-1,-1],[1,-1,-1],[-1,1,-1],[1,1,-1],[-1,-1,1],[1,-1,1],[-1,1,1],[1,1,1]],
    validate_cube_verbose(Cube, Eps, EdgeLen, Edges),
    validate_ellipsoid_verbose(Ell),
    intersect_ellipsoid_with_edges(Ell, Cube, Edges, EdgeLen, Eps, Raw),
    dedup_points(Raw, max(Eps, EdgeLen*1.0e-9), P),
    print_points(P).

% ===== Запуск с данными без диалога =====
run_data([Cx,Cy,Cz],[Ax,Ay,Az], Cube) :-
    Eps = 1.0e-6,
    Ell = ellipsoid(Cx,Cy,Cz, Ax,Ay,Az),
    validate_ellipsoid_verbose(Ell),
    validate_cube_verbose(Cube, Eps, EdgeLen, Edges),
    nl, format("✓ Валидация пройдена. Считаю пересечение...~n", []),
    intersect_ellipsoid_with_edges(Ell, Cube, Edges, EdgeLen, Eps, Raw),
    dedup_points(Raw, max(Eps, EdgeLen*1.0e-9), P),
    nl, format("Результат:~n", []),
    print_points(P).
% ===== Диагностика: печать пересечений по всем рёбрам на кейсе =====

diag_case_ok :-
    Eps = 1.0e-6,
    Ell = ellipsoid(0,0,0, 1.5,1.5,0.5),
    Cube = [[-1,-1,-1],[1,-1,-1],[-1,1,-1],[1,1,-1],[-1,-1,1],[1,-1,1],[-1,1,1],[1,1,1]],
    validate_cube_verbose(Cube, Eps, EdgeLen, Edges),
    format("Edges: ~g~n", [EdgeLen]),
    length(Edges, NEdges), format("Всего рёбер: ~d~n", [NEdges]),
    forall(
        member([A,B], Edges),
        ( segment_ellipsoid_intersections(Ell, A, B, Eps, Ps),
          length(Ps, K),
          format("Ребро ~w -> ~w: ~d точек~n", [A,B, K]),
          forall(member(P, Ps), format("  ~w~n", [P]))
        )
    ),
    intersect_ellipsoid_with_edges(Ell, Cube, Edges, EdgeLen, Eps, Raw),
    dedup_points(Raw, max(Eps, EdgeLen*1.0e-9), P),
    length(P, NP),
    format("Итого после дедупликации: ~d точек~n", [NP]),
    print_points(P).