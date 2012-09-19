%% Copyright
-module(create).
-author("kris").

%% API
-export([create/1,create_reverse/1]).

create(0) -> [];
create(N) -> create(N-1) ++ [N].

create_reverse(0) -> [];
create_reverse(N) -> [N]++create_reverse(N-1).
