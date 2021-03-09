:- dynamic actor_position/2.
:- dynamic have_immunity/1.
:- dynamic covid/2.
:- dynamic doctor/2.
:- dynamic mask/2.
:- dynamic home/2.
:- dynamic best_length_path/1.
:- dynamic aver_time/1.

size(9).
aver_time(0).
best_length_path(inf).
scenario(1).
have_immunity(0).
actor_position(1, 1).

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

next_move(CX, CY, NX, NY):-
    home(XH, YH),
    DX is sign(XH - CX),
    DY is sign(YH - CY),
    (
        (NX is CX + DX, NY is CY + DY);
        (NX is CX, NY is CY + 1);
        (NX is CX + 1, NY is CY + 1);
        (NX is CX + 1, NY is CY);
        (NX is CX + 1, NY is CY - 1);
        (NX is CX, NY is CY - 1);
        (NX is CX - 1, NY is CY - 1);
        (NX is CX - 1, NY is CY);
        (NX is CX - 1, NY is CY + 1)
    ),
    have_immunity(I),
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
            retractall(best_length_path(_)), assert(best_length_path(L)),
            write("L: "), print(L), nl
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

    next_move(SX, SY, NX, NY),
    not(member([NX, NY], Keep)),

    have_immunity(I),

    backtracking_path_rec(NX, NY, EX, EY, I, [[SX, SY] | Keep], NextSteps, NL).

test:-
    %retractall(covid(_, _)), retractall(doctor(_, _)), retractall(mask(_, _)), retractall(home(_, _)),
    %retractall(best_length_path(_)), assert(best_length_path(inf)),
    %gen_all,

    %assert(covid(8, 3)), assert(covid(8, 6)), assert(doctor(1, 3)), assert(mask(1, 8)), assert(home(1, 10)),
    home(XH, YH),
    (statistics(runtime, [ST|_]), bagof(Path, backtracking_path_rec(1, 1, XH, YH, 0, [[1, 1]], Path, 1), AllPaths),statistics(runtime, [ET|_]), T is (ET - ST) / 1000 -> 
        (
            last(AllPaths, BestBath),
            best_length_path(BL),
            aver_time(AT),
            NT is AT + T,
            retractall(aver_time(_)), assert(aver_time(NT)),
            write("Win!"), nl, nl,
            write("Path: "), nl, print_path(BestBath), nl,
            write("Steps: "), nl, print(BL), nl, nl,
            write("Time: "), nl, print(T), nl
        );
        write("Lose!")
    ).

av_time(I):-
    (I > 0 -> 
        (
            test,
            NI is I - 1,
            av_time(NI)
        );
        aver_time(AT),
        nl, write("Average time: "), print(AT), nl
    ).