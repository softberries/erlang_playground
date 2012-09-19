%% Copyright
-module(xmlimporter).
-author("kris").

-import(queue,[in/1,out/1,new/0]).
%% user interface
-export([start/0]).

-record(location,{country="",state="",city="",latitude="",longitude=""}).
-record(artist,{id="",name="",url="",image="", mbgid="", location=#location{}}).

%start() -> run("/Users/kris/Downloads/dbdump_artistalbumtrack.0.290905586176.xml",0).
start() -> run("/Users/kris/Documents/projects/erlang_projects/playground/src/testsmall.xml",0).

run(File, Result) ->
  case file:read_file(xml(File)) of
    {ok, Bin} ->
      {ok,_,[]} = erlsom:parse_sax(Bin, [], fun callback/2);
    Error ->
      Error
  end,
  Result.

callback(Event, Acc) -> processTag(Event,Acc).

isValidTag(T) -> lists:member(T,
    ["artist","id","name","image","url","mbgid","country","state","city","location","latitude","longitude"]
).

processTag({startElement,[],T,[],[]},Acc) ->
  case isValidTag(T) of
            true -> processStartTag(T,Acc);
            _Else    -> Acc
  end;

processTag({endElement,[],T,[]},Acc) ->
  case isValidTag(T) of
            true -> processEndTag(T,Acc);
            _Else    -> Acc
  end;

processTag({characters,Data},Acc) -> [Path,Artist] = Acc, io:format("Path: ~p, Data: ~p~n",[Path,Data]),
  case Path of {["artist"],[]} -> Acc;
               {["id"],["artist"]} -> [Path,Artist#artist{id=Data}];
               {["name"],["artist"]} -> [Path,Artist#artist{name=Data}];
               {["url"],["artist"]} -> [Path,Artist#artist{url=Data}];
               {["image"],["artist"]} -> [Path,Artist#artist{image=Data}];
               {["mbgid"],["artist"]} -> [Path,Artist#artist{mbgid=Data}];
               {["city","location"],["artist"]} ->  [Path,Artist#artist{location=#location{city=Data}}];
               {["state","location"],["artist"]} -> [Path,Artist#artist{location=#location{state=Data}}];
               {["country","location"],["artist"]} -> [Path,Artist#artist{location=#location{country=Data}}];
               {["latitude","location"],["artist"]} -> [Path,Artist#artist{location=#location{latitude=Data}}];
               {["longitude","location"],["artist"]} -> [Path,Artist#artist{location=#location{longitude=Data}}];
    _Else    -> Acc
  end;

processTag(_,ok) -> [];
processTag(_,Acc) -> Acc.


processStartTag("artist", _) -> Path = queue:new(), [queue:in("artist",Path),#artist{}];
processStartTag(T,Acc) -> [Path,Artist] = Acc, [queue:in(T,Path),Artist].

processEndTag("artist",Acc) -> io:format("~p~n",[Acc]), [];
processEndTag(_,Acc) -> [Path,Artist] = Acc, {_,Queue} = queue:out_r(Path), [Queue, Artist].





%% this is just to make it easier to test this little example
xml(File) -> filename:join([codeDir(), File]).
codeDir() -> filename:dirname(code:which(?MODULE)).