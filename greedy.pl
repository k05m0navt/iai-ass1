:- dynamic actor_position/2.
:- dynamic have_immunity/1.
:- dynamic covid/2.
:- dynamic doctor/2.
:- dynamic mask/2.
:- dynamic home/2.
:- dynamic best_length_path/1.

size(9).
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

%Greedy

:- dynamic path_greedy/3.
path_greedy([], inf, 0).

distance_between_two_points(SX, SY, D):-
    home(HX, HY),
    D is sqrt((SX - HX)**2 + (SY - HY)**2).

min_distance(CX, CY, [D1, D2, D3, D4, D5, D6, D7, D8], I):-
    (
    	(NX1 is CX, NY1 is CY + 1),
        ((I =:= 0) -> 
            (
            	(field_zone(NX1, NY1), not(covid_zone(NX1, NY1)) ->  
                	(
                    	distance_between_two_points(NX1, NY1, D1)
                    );
                	D1 is inf
                )
            );
            true
        ),
        ((I =:= 1) -> 
            (
            	(field_zone(NX1, NY1) ->  
                	(
                    	distance_between_two_points(NX1, NY1, D1)
                    );
                	D1 is inf
                )
            );
            true
        )
    ),
    (
    	(NX2 is CX + 1, NY2 is CY + 1),
        ((I =:= 0) -> 
            (
            	(field_zone(NX2, NY2), not(covid_zone(NX2, NY2)) ->  
                	(
                    	distance_between_two_points(NX2, NY2, D2)
                    );
                	D2 is inf
                )
            );
            true
        ),
        ((I =:= 1) -> 
            (
            	(field_zone(NX2, NY2) ->  
                	(
                    	distance_between_two_points(NX2, NY2, D2)
                    );
                	D2 is inf
                )
            );
            true
        )
    ),
    (
    	(NX3 is CX + 1, NY3 is CY),
        ((I =:= 0) -> 
            (
            	(field_zone(NX3, NY3), not(covid_zone(NX3, NY3)) ->  
                	(
                    	distance_between_two_points(NX3, NY3, D3)	
                    );
                	D3 is inf
                )
            );
            true
        ),
        ((I =:= 1) -> 
            (
            	(field_zone(NX3, NY3) ->  
                	(
                    	distance_between_two_points(NX3, NY3, D3)	
                    );
                	D3 is inf
                )
            );
            true
        )
    ),
    (
    	(NX4 is CX + 1, NY4 is CY - 1),
        ((I =:= 0) -> 
            (
            	(field_zone(NX4, NY4), not(covid_zone(NX4, NY4)) ->  
                	(
                    	distance_between_two_points(NX4, NY4, D4)	
                    );
                	D4 is inf
                )
            );
            true
        ),
        ((I =:= 1) -> 
            (
            	(field_zone(NX4, NY4) ->  
                	(
                    	distance_between_two_points(NX4, NY4, D4)	
                    );
                	D4 is inf
                )
            );
			true
        )
    ),
    (
    	(NX5 is CX, NY5 is CY - 1),
        ((I =:= 0) -> 
            (
            	(field_zone(NX5, NY5), not(covid_zone(NX5, NY5)) ->  
                	(
                    	distance_between_two_points(NX5, NY5, D5)
                    );
                	D5 is inf
                )
            );
            true
        ),
        ((I =:= 1) -> 
            (
            	(field_zone(NX5, NY5) ->  
                	(
                    	distance_between_two_points(NX5, NY5, D5)
                    );
                	D5 is inf
                )
            );
            true
        )
    ),
    (
    	(NX6 is CX - 1, NY6 is CY - 1),
        ((I =:= 0) -> 
            (
            	(field_zone(NX6, NY6), not(covid_zone(NX6, NY6)) ->  
                	(
                    	distance_between_two_points(NX6, NY6, D6)
                    );
                	D6 is inf
                )
            );
            true
        ),
        ((I =:= 1) -> 
            (
            	(field_zone(NX6, NY6) ->  
                	(
                    	distance_between_two_points(NX6, NY6, D6)
                    );
                	D6 is inf
                )
            );
            true
        )
    ),
    (
    	(NX7 is CX - 1, NY7 is CY),
        ((I =:= 0) -> 
            (
            	(field_zone(NX7, NY7), not(covid_zone(NX7, NY7)) ->  
                	(
                    	distance_between_two_points(NX7, NY7, D7)
                    );
                	D7 is inf
                )
            );
            true
        ),
        ((I =:= 1) -> 
            (
            	(field_zone(NX7, NY7) ->  
                	(
                    	distance_between_two_points(NX7, NY7, D7)
                    );
                	D7 is inf
                )
            );
            true
        )
    ),
    (
    	(NX8 is CX - 1, NY8 is CY + 1),
        ((I =:= 0) -> 
            (
            	(field_zone(NX8, NY8), not(covid_zone(NX8, NY8)) ->  
                	(
                    	distance_between_two_points(NX8, NY8, D8)
                    );
                	D8 is inf
                )
            );
            true
        ),
        ((I =:= 1) -> 
            (
            	(field_zone(NX8, NY8) ->  
                	(
                    	distance_between_two_points(NX8, NY8, D8)
                    );
                	D8 is inf
                )
            );
            true
        )
    ).

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

greedy_algo(X, Y, X, Y, _, _, [[X, Y]], BL):-
    retractall(best_length_path(_)), assert(best_length_path(BL)).

greedy_algo(SX, SY, EX, EY, Immunity, Keep, [[SX, SY] | NextSteps], L):-
    NL is L + 1,
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
	
    min_distance(SX, SY, All_D, I),
    
    min_list(All_D, Min_D),
    
    next_move(SX, SY, NX, NY, I),
    
   	distance_between_two_points(NX, NY, CD),
    
    (Min_D =:= CD),
    
    not(member([NX, NY], Keep)),
    
    greedy_algo(NX, NY, EX, EY, I, [[SX, SY] | Keep], NextSteps, NL).
    
test:-
    gen_all,
    home(EX, EY),
    (statistics(runtime, [ST|_]), greedy_algo(1, 1, EX, EY, 0, [], P, 1), statistics(runtime, [ET|_]), T is (ET - ST) / 1000 ->
    	(
        	best_length_path(BL),
        	write("Win!"), nl, nl,
            write("Path: "), nl, print_path(P), nl,
            write("Steps: "), nl, print(BL), nl, nl,
            write("Time: "), nl, print(T), write(" sec"), nl
        );
    	write("Lose!")
    ).