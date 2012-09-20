%% Copyright
-module(xmlimporter).
-author("kris").

-import(queue,[in/1,out/1,new/0]).
%% user interface
-export([start/0]).

%-record(location,{country="",state="",city="",latitude="",longitude=""}).
%-record(artist,{id="",name="",url="",image="", mbgid="", location=#location{}}).

%Doc = {[{<<"foo">>, <<"bar">>}]}

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
    ["artist","id","name","image","url","mbgid","country","state","city","location","latitude","longitude","album","Albums"]
).

processTag({startElement,[],"location",[],[]},Acc) -> [Path,N,Artist] = Acc,
  processStartTag("location",[Path,N,Artist++[{<<"location">>,[]}]]);
processTag({startElement,[],"Albums",[],[]},Acc) -> [Path,N,Artist] = Acc,
  processStartTag("Albums",[Path,N,Artist++[{<<"Albums">>,[]}]]);
processTag({startElement,[],"album",[],[]},Acc) -> [Path,N,Artist] = Acc,
  {Key,Albums} = getByKey(Artist,<<"Albums">>),
  processStartTag("album",[Path,N+1,lists:keyreplace(Key,1,Artist,{Key,Albums++[<<"album">>,[]]})]);

processTag({startElement,[],T,[],[]},Acc) ->
  case isValidTag(T) of
            true  -> processStartTag(T,Acc);
            _Else -> Acc
  end;

processTag({endElement,[],T,[]},Acc) ->
  case isValidTag(T) of
            true  -> processEndTag(T,Acc);
            _Else -> Acc
  end;

processTag({characters,Data},Acc) -> [Path,N,Artist] = Acc,
%  io:format("Path: ~p, Data: ~p~n",[Path,Data]),
  case Path of
    %Artist
               {["artist"],[]} -> Acc;
               {["id"],["artist"]} -> [Path,N,Artist++[{<<"id">>,Data}]];
               {["name"],["artist"]} -> [Path,N,Artist++[{<<"name">>,Data}]];
               {["url"],["artist"]} -> [Path,N,Artist++[{<<"url">>,Data}]];
               {["image"],["artist"]} -> [Path,N,Artist++[{<<"image">>,Data}]];
               {["mbgid"],["artist"]} -> [Path,N,Artist++[{<<"mbgid">>,Data}]];
    %Location
               {["city","location"],["artist"]} -> T = {<<"city">>,Data}, {Key,Loc} = getByKey(Artist,<<"location">>),
                 [Path,N,lists:keyreplace(Key,1,Artist,{Key,Loc++[T]})];
               {["state","location"],["artist"]} -> T = {<<"state">>,Data}, {Key,Loc} = getByKey(Artist,<<"location">>),
                 [Path,N,lists:keyreplace(Key,1,Artist,{Key,Loc++[T]})];
               {["country","location"],["artist"]} -> T = {<<"country">>,Data}, {Key,Loc} = getByKey(Artist,<<"location">>),
                 [Path,N,lists:keyreplace(Key,1,Artist,{Key,Loc++[T]})];
               {["latitude","location"],["artist"]} -> T = {<<"latitude">>,Data}, {Key,Loc} = getByKey(Artist,<<"location">>),
                 [Path,N,lists:keyreplace(Key,1,Artist,{Key,Loc++[T]})];
               {["longitude","location"],["artist"]} -> T = {<<"langitude">>,Data}, {Key,Loc} = getByKey(Artist,<<"location">>),
                 [Path,N,lists:keyreplace(Key,1,Artist,{Key,Loc++[T]})];
   %Albums
%               {["id","album","Albums"],["artist"]} -> Acc;
    _Else    -> Acc
  end;

processTag(_,ok) -> [];
processTag(_,Acc) -> Acc.

processStartTag("artist", _) -> Path = queue:new(), [queue:in("artist",Path),0,[]];
processStartTag(T,Acc) -> [Path,N,Artist] = Acc, [queue:in(T,Path),N,Artist].


processEndTag("artist",Acc) -> io:format("~p~n",[Acc]), [];
processEndTag("Albums",Acc) -> [Path,_,Artist] = Acc, {_,Queue} = queue:out_r(Path), [Queue, 0, Artist];
processEndTag(_,Acc) -> [Path,N,Artist] = Acc, {_,Queue} = queue:out_r(Path), [Queue, N, Artist].


getByKey(Artist,Key) -> lists:keyfind(Key, 1, Artist).


%% this is just to make it easier to test this little example
xml(File) -> filename:join([codeDir(), File]).
codeDir() -> filename:dirname(code:which(?MODULE)).