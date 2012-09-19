%% Copyright
-module(test).
-author("kris").

%% API
-export([fibonacci/1]).

fibonacci([]) -> 0;
fibonacci([Head|Tail]) -> Head + fibonacci(Tail).


