%HARD_VERSION

init(MAX_N) :- prime_init(MAX_N, 2, 2) ; construct_init_prime(MAX_N, 2).
% Без этого не проходят тесты
composite(1).
prime_init(MAX_N, N, P) :-
	N1 is N * N, add_P_class_composite_numbers(MAX_N, N1, P),
	N2 is N + 1, MAX_N >= N2 * N2,
	find_prime(MAX_N, N2, N2).

find_prime(MAX_N, N, P) :-
	composite(N), !,
	N1 is N + 1,
	find_prime(MAX_N, N1, N1).
find_prime(MAX_N, N, P) :- prime_init(MAX_N, N, P), !.

add_P_class_composite_numbers(MAX_N, N, P) :- N > MAX_N, !.
add_P_class_composite_numbers(MAX_N, N, P) :- assert(composite(N)), N1 is N + P, add_P_class_composite_numbers(MAX_N, N1, P).

%prime
construct_init_prime(MAX_N, N) :- N > MAX_N, !.
construct_init_prime(MAX_N, N) :- \+ composite(N), assert(prime(N)), N = 1.
construct_init_prime(MAX_N, N) :- N1 is N + 1, construct_init_prime(MAX_N, N1).
%%%%%
prime_divisors(N, Divisors) :- number(N), !, find_divisors(N, Divisors, 1).
prime_divisors(N, Divisors) :- find_N(N, Divisors, 1, 1).

%N -
find_N(N, [], _, N).
find_N(N, [H | T], L, NX) :-
		correct_divisor(H, L),
		NX1 is H * NX,
		find_N(N, T, H, NX1).

%Divisors -.
find_divisors(1, D, _) :- !, length(D, 0).
find_divisors(N, [H | T], L) :-
		correct_divisor(H, L), 0 is mod(N, H),
		N1 is N / H, find_divisors(N1, T, H), !.

correct_divisor(H, L) :- prime(H), H >= L.
%Mod		
%%%%%%%%%%%%%%%%%%%%%%%%
unique_prime_divisors(1, V) :- V = [], !.		
unique_prime_divisors(N, Divisors) :- 
	prime_divisors(N, Divisors_V),
	reverse(Divisors_V, Divisors_V1),
	unique_prime_divisors_impl(Divisors_V1, 0, Ans),
	correct_divisors(Ans, Divisors), Divisors = Ans, !.
		
unique_prime_divisors_impl(Divisors, X, Ans) :- length(Divisors, 0), !.

unique_prime_divisors_impl([H | T], H, Ans) :- unique_prime_divisors_impl(T, H, Ans), !.

unique_prime_divisors_impl([H | T], X, Ans) :- append(Ans1, [H], Ans), unique_prime_divisors_impl(T, H, Ans1), !.

correct_divisors(Ans, Divisors) :- var(Divisors); (length(Ans, X), length(Divisors, X)).
