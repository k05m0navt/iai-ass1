:- dynamic actor_position/2.
:- dynamic have_immunity/1. % Fact that control do actor have immunity
:- dynamic covid/2.
:- dynamic doctor/2.
:- dynamic mask/2.
:- dynamic home/2.
:- dynamic best_length_path/1. % Fact that saves path best lenght

size(9). % Size of the field 
best_length_path(inf).
scenario(1). % Variant of scenario
have_immunity(0).
actor_position(1, 1). % Start actor position

actor(X, Y, I) :-
    have_immunity(I),
    actor_position(X, Y).

% Rule to randomly generate covids
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

% Rule to randomly generate doctor
gen_doctor:-
    retractall(doctor(_, _)),
    size(Size1),
    Size is Size1 + 1,
    random(1, Size, X),
    random(1, Size, Y),
    (covid_zone(X, Y) -> gen_doctor; write("Doctor: "), print([X, Y]), nl), % Recursion if doctor on covid zone
    assert(doctor(X, Y)).

% Rule to randomly generate mask
gen_mask:-
    retractall(mask(_, _)),
    size(Size1),
    Size is Size1 + 1,
    random(1, Size, X),
    random(1, Size, Y),
    (covid_zone(X, Y) -> gen_mask; write("Mask: "), print([X, Y]), nl), % Recursion if mask on covid zone
    assert(mask(X, Y)).

% Rule to randomly generate home
gen_home:-
    retractall(home(_, _)),
    size(Size1),
    Size is Size1 + 1,
    random(1, Size, X),
    random(1, Size, Y),
    (covid_zone(X, Y) -> gen_home; (write("Home: "), print([X, Y]), nl, nl)), % Recursion if home on covid zone
    assert(home(X, Y)).

% Rule to generate map
gen_all:-
    gen_covid,
    gen_doctor,
    gen_mask,
    gen_home.

% Rule that checks that coordinates in covid zone
covid_zone(X, Y) :-
    covid(X1, Y1),
    Xl is X1-1, Xr is X1+1,
    Yb is Y1-1, Yu is Y1+1,
    between(Xl, Xr, X),
    between(Yb, Yu, Y).

% Rule that checks that coordinates in map zone
field_zone(X, Y):-
    size(Size),
    between(1, Size, X),
    between(1, Size, Y).

% Rule to print path
print_path([[X, Y] | NextSteps]):-
    write("["), print(X), write(","), print(Y), write("]"), nl,
    (print_path(NextSteps);true).

%Backtracking

:- dynamic best_path_backtracking/3.
best_path_backtracking([], inf, 0).

% Find nex possible move with some euristics
next_move(CX, CY, NX, NY, I):-
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
    ((I = 0) -> 
        (
            not(covid_zone(NX, NY))
        );
        true
    ),
    field_zone(NX, NY). 

% Base case of backtracking recursion
backtracking_path_rec(X, Y, X, Y, _, _, [[X, Y]], L):-
    best_length_path(BL),
    ((BL > L) -> 
        (
            retractall(best_length_path(_)), assert(best_length_path(L)) % If current path better than best_length_path - update best_length_path
        );
        true
    ).

% Main backtracking recursion
backtracking_path_rec(SX, SY, EX, EY, Immunity, Keep, [[SX, SY] | NextSteps], L):-
    retractall(have_immunity(_)), assert(have_immunity(Immunity)),
    
    NL is L + 1,
    best_length_path(BL),
    not(NL > BL),

    doctor(XD, YD),
    mask(XM, YM),
    (Immunity = 0 -> 
        (
            (((XD = SX),(YD = SY));((XM = SX),(YM = SY)) -> 
                (
                    retractall(have_immunity(_)), assert(have_immunity(1)) % If current position on doctor or on mask - update immunity
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

% Test rule that generate map and print the result
test:-
    gen_all,
    home(XH, YH),
    (statistics(runtime, [ST|_]), bagof(Path, backtracking_path_rec(1, 1, XH, YH, 0, [[1, 1]], Path, 1), AllPaths),statistics(runtime, [ET|_]), T is (ET - ST) / 1000 -> 
        (
            last(AllPaths, BestPath),
            best_length_path(BL),
            write("Win!"), nl, nl,
            write("Path: "), nl, print_path(BestPath), nl,
            write("Steps: "), nl, print(BL), nl, nl,
            write("Time: "), nl, print(T), write(" sec"), nl
        );
        write("Lose!"), nl, nl
    ).

% Run test function I times and print sum af all times execution
sum_time(I):-
    assert(time(0)),
    (I > 0 -> 
        (
            retractall(covid(_, _)), retractall(doctor(_, _)), retractall(mask(_, _)), retractall(home(_, _)),
            retractall(best_length_path(_)), assert(best_length_path(inf)),
            write("Iteration: "), print(I), nl,
            test,
            NI is I - 1,
            sum_time(NI)
        );
        time(T),
        nl, write("Sum time: "), print(T), nl
    ).