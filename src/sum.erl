%% Copyright
-module(sum).
-author("kris").

%% API
-export([sum/1, sum/2]).

sum(0) -> 0;
sum(N) when N > 0 -> N + sum(N-1).

sum(M,M) -> M;
sum(N,M) when N =< M -> N + sum(N+1,M).
