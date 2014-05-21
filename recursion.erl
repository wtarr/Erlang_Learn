%% from reading "learn you some erlang"

-module(recursion).
-export([factorial/1, factorialOpt/1, fibonacci/1, len/1, tail_fac/1, tail_fac/2,
 lenTailRec/1, lenTailRec/2, duplicate/2, tail_duplicate/2, tail_duplicate/3, calculateFees/2, calculateFees/3,
 reverse/1, tail_reverse/1, tail_reverse/2, zip/2, ziptailRec/2, ziptailRec/3]).

factorial(N) when N == 0 -> 1;
factorial(N) when N > 0 -> N * factorial(N - 1).

%% 6! using above
%% f(6) -> 6 * f(6 - 1)
%% f(5) -> 6 * 5 * f(5 - 1)
%% f(4) -> 6 * 5 * 4 * f(4 - 1)
%% f(3) -> 6 * 5 * 4 * 3 * f(3 - 1)
%% f(2) -> 6 * 5 * 4 * 3 * 2 * f(2 - 1)
%% f(1) -> 6 * 5 * 4 * 3 * 2 * 1
%%		   6 * 5 * 4 * 3 * 2
%%		   6 * 5 * 4 * 6
%%		   6 * 5 * 24
%%		   6 * 120
%%		   720


factorialOpt(0) -> 1;
factorialOpt(N) -> N * factorialOpt(N - 1).

%% 0 based fibonacci
fibonacci(0) -> 0;
fibonacci(1) -> 1;
fibonacci(2) -> 1;
fibonacci(N) -> fibonacci(N - 1) + fibonacci(N - 2).
	
%% Length of list
%% This is OK for short lists
len([]) -> 0;
len([_|T]) -> 1 + len(T).

%% Tail recursion approach
%% The count travels with the function
%% only need for two stores
tail_fac(N) -> tail_fac(N, 1).

tail_fac(0, Acc) -> Acc;
tail_fac(N, Acc) when N > 0 -> tail_fac(N-1, N*Acc).

%% 6! using the above 
%% tf(6)    ->  tf(6    ,     1)
%% tf(6, 1) ->  tf(6 - 1, 6 * 1)
%% tf(5, 6) ->  tf(5 - 1, 5 * 6)
%% tf(4, 30)->  tf(4 - 1, 4 * 30)
%% tf(3, 120)-> tf(3 - 1, 3 * 120)
%% tf(2, 360)-> tf(2 - 1, 2 * 360)
%% tf(1, 720)-> tf(1 - 1, 1 * 720)
%% tf(0, 720)-> 720 

lenTailRec(L) -> lenTailRec(L, 0).

lenTailRec([], Acc) -> Acc;
lenTailRec([_|T], Acc) -> lenTailRec(T, Acc+1).

%% Duplicate
%%
%% note:
%%> [a | []].
%%> [a]
duplicate(0, _) -> [];
duplicate(N, Term) when N > 0 -> [Term|duplicate(N-1, Term)].

%% d(3, a) -> [a|d(3 - 1, a)]
%%				[a|[a|d(2 - 1, a)]]
%%				[a|[a|[a|d(1 - 1, a)]]]
%% 				[a|[a|[a|[]]]
%%				[a, a, a]

%% Tail recursive duplicate function
%% the adapter
tail_duplicate(N, Term) ->
	tail_duplicate(N, Term, []).

tail_duplicate(0, _, List) ->
	List;
tail_duplicate(N, Term, List) when N > 0 ->
	tail_duplicate(N - 1, Term, [Term|List]).

%% http://blog.dynamicprogrammer.com/2012/04/08/learning-erlang-6-recursion.html
%% calculate fees - 
%% 1. separate the head from the tail [H|T]
%% 2. evaluate for the head
%% 3. call calculate fees on the Tail with the accumulated amount (step 2)
calculateFees(Amount, Taxes) ->
	calculateFees(Amount, Taxes, 0).

calculateFees(Amount, [], Accum) -> {Amount, Accum};
calculateFees(Amount, [H|T], Accum) -> calculateFees(Amount, T, ((Amount * H) + Accum)).

%% cf(10, [1, 2, 3]) -> cf(10, [1, 2, 3], 0)
%% cf(10, [1|2 , 3], 0) -> cf(10, [2, 3], ((10 * 1) + 0))
%% cf(10, [2 | 3], 10) -> cf(10, [3], ((10 * 2) + 10))
%% cf(10, [3], 30) -> cf(10, [], ((30 * 3)+ 30))
%% cf(10, [], 120) -> {10, 120}

reverse([]) -> [];
reverse([H|T]) -> reverse(T)++reverse(H).

tail_reverse(L) -> tail_reverse(L, []).

tail_reverse([], Acc) -> Acc;
tail_reverse([H|T], Acc) -> tail_reverse(T, [H|Acc]).
%% tr([1, 2, 3, 4])     -> tr([1, 2, 3, 4],[])
%% tr([1| 2, 3, 4], []) -> tr([2, 3, 4], [1|[]])
%% tr([2 | 3, 4], [1])  -> tr([3, 4], [2|[1]]) **note [2|[1]] = [2, 1]
%% tr([3| 4], [2, 1])   -> tr([4], [3| [2, 1])
%% tr([4], [3, 2, 1])   -> tr([], [4| [3, 2, 1]])
%% tr([], [4, 3, 2, 1]) -> [4, 3, 2, 1]



zip([], []) -> [];
zip([X|Xs], [Y|Ys]) -> [{X, Y}| zip(Xs, Ys)].



ziptailRec(La, Lb) -> ziptailRec(La, Lb, []).

ziptailRec(_, [], Acc) -> Acc;
ziptailRec([], _, Acc) -> Acc;
ziptailRec([Hx|Tx], [Hy|Ty], Acc) -> ziptailRec(Tx, Ty, [{Hx, Hy} | Acc]).

