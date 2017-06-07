%% Factorial
factorial(0) -> 1;
factorial(N) -> N * factorial(N-1).

%% Fibonacci Sequence
fib(0) -> 0;
fib(1) -> 1;
fib(N) -> fib(N-1) + fib(N-2).
