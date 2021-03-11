:- dynamic actor_position/2.
:- dynamic have_immunity/1.
:- dynamic covid/2.
:- dynamic doctor/2.
:- dynamic mask/2.
:- dynamic home/2.
:- dynamic best_length_path/1.

size(9).
best_length_path(10000).
scenario(1).
have_immunity(0).
actor_position(1, 1).

covid(6,4).
covid(1,4).
doctor(3,3).
mask(4,7).
home(7,6).

actor(X, Y, I) :-
    have_immunity(I),
    actor_position(X, Y).

gen_covid:-
    size(Size1),
    Size is Size1 + 1,
    random(1, Size, X1),
    random(1, Size, Y1),
    assert(covid(X1, Y1)),
    random(1, Size, X2),
    random(1, Size, Y2),
    assert(covid(X2, Y2)),
    write("Covid_1: "), print([X1, Y1]), nl,
    write("Covid_2: "), print([X2, Y2]), nl.

gen_doctor:-
    retractall(doctor(_, _)),
    size(Size1),
    Size is Size1 + 1,
    random(1, Size, X),
    random(1, Size, Y),
    (covid_zone(X, Y) -> gen_doctor; write("Doctor: "), print([X, Y]), nl),
    assert(doctor(X, Y)).

gen_mask:-
    retractall(mask(_, _)),
    size(Size1),
    Size is Size1 + 1,
    random(1, Size, X),
    random(1, Size, Y),
    (covid_zone(X, Y) -> gen_mask; write("Mask: "), print([X, Y]), nl),
    assert(mask(X, Y)).

gen_home:-
    retractall(home(_, _)),
    size(Size1),
    Size is Size1 + 1,
    random(1, Size, X),
    random(1, Size, Y),
    (covid_zone(X, Y) -> gen_home; (write("Home: "), print([X, Y]), nl)),
    assert(home(X, Y)).

gen_all:-
    gen_covid,
    gen_doctor,
    gen_mask,
    gen_home.

covid_zone(X, Y) :-
    covid(X1, Y1),
    Xl is X1-1, Xr is X1+1,
    Yb is Y1-1, Yu is Y1+1,
    between(Xl, Xr, X),
    between(Yb, Yu, Y).

field_zone(X, Y):-
    size(Size),
    between(1, Size, X),
    between(1, Size, Y).

print_path([[X, Y] | NextSteps]):-
    write("["), print(X), write(","), print(Y), write("]"), nl,
    (print_path(NextSteps);true).

%Backtracking

:- dynamic best_path_backtracking/3.
best_path_backtracking([], inf, 0).

next_move(CX, CY, NX, NY, I):-
    (
        (NX is CX, NY is CY + 1);
        (NX is CX + 1, NY is CY + 1);
        (NX is CX + 1, NY is CY);
        (NX is CX + 1, NY is CY - 1);
        (NX is CX, NY is CY - 1);
        (NX is CX - 1, NY is CY - 1);
        (NX is CX - 1, NY is CY);
        (NX is CX - 1, NY is CY + 1)
    ),
    ((I = 0) -> 
        (
            not(covid_zone(NX, NY))
        );
        true
    ),
    field_zone(NX, NY). 

backtracking_path_rec(X, Y, X, Y, _, _, [[X, Y]], L):-
    best_length_path(BL),
    ((BL > L) -> 
        (
            retractall(best_length_path(_)), assert(best_length_path(L))
        );
        true
    ).
backtracking_path_rec(SX, SY, EX, EY, Immunity, Keep, [[SX, SY] | NextSteps], L):-
    NL is L + 1,
    best_length_path(BL),
    not(NL > BL),

    retractall(have_immunity(_)), assert(have_immunity(Immunity)),

    doctor(XD, YD),
    mask(XM, YM),
    (Immunity = 0 -> 
        (
            (((XD = SX),(YD = SY));((XM = SX),(YM = SY)) -> 
                (
                    retractall(have_immunity(_)), assert(have_immunity(1))
                );
                true
            )
        );
        true
    ),

    have_immunity(I),


    next_move(SX, SY, NX, NY, I),
    not(member([NX, NY], Keep)),

    backtracking_path_rec(NX, NY, EX, EY, I, [[SX, SY] | Keep], NextSteps, NL).

test:-
    %gen_all,
    home(XH, YH),
    (statistics(runtime, [ST|_]), bagof(Path, backtracking_path_rec(1, 1, XH, YH, 0, [[1, 1]], Path, 1), AllPaths),statistics(runtime, [ET|_]), T is (ET - ST) / 1000 -> 
        (
            last(AllPaths, BestBath),
            best_length_path(BL),
            write("Win!"), nl, nl,
            write("Path: "), nl, print_path(BestBath), nl,
            write("Steps: "), nl, print(BL), nl, nl,
            write("Time: "), nl, print(T), nl
        );
        write("Lose!")
    ).