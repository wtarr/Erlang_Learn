%% from reading "learn you some erlang"

-module(recursion).
-export([factorial/1, factorialOpt/1, fibonacci/1, len/1, tail_fac/1, tail_fac/2, lenTailRec/1, lenTailRec/2]).

factorial(N) when N == 0 -> 1;
factorial(N) when N > 0 -> N * factorial(N - 1).

%% 6! using above
%% f(6) -> 6 * f(6 - 1)
%% f(5) -> 6 * 5 * f(5 - 1)
%% f(4) -> 6 * 5 * 4 * f(4 - 1)
%% f(3) -> 6 * 5 * 4 * 3 * f(3 - 1)
%% f(2) -> 6 * 5 * 4 * 3 * 2 * f(2 - 1)
%% f(1) -> 6 * 5 * 4 * 3 * 2 * 1
%%	   6 * 5 * 4 * 3 * 2
%%	   6 * 5 * 4 * 6
%%	   6 * 5 * 24
%%	   6 * 120
%%	   720


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
