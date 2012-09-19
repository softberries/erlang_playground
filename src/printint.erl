%% Copyright
-module(printint).
-author("kris").

%% API
-export([printint/1, printevenint/1]).

printint(0) -> print(0);
printint(N) -> printint(N-1),print(N).
print(M) -> io:format("Number:~p~n",[M]).


printevenint(0) -> printeven(0);
printevenint(M) -> printevenint(M-1),printeven(M).
printeven(M) when (M rem 2) == 0 -> io:format("Number:~p~n",[M]);
printeven(_) -> io:format("").