-module(recursion).
-export([factorial/1, factorialOpt/1, fibonacci/1]).

factorial(N) when N == 0 -> 1;
factorial(N) when N > 0 -> N * factorial(N - 1).

factorialOpt(0) -> 1;
factorialOpt(N) -> N * factorialOpt(N - 1).


fibonacci(0) -> 0;
fibonacci(1) -> 1;
fibonacci(2) -> 1;
fibonacci(N) -> fibonacci(N - 1) + fibonacci(N - 2).
	
	

