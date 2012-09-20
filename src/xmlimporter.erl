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
    ["artist","id","name","image","url","mbgid","country","state","city","location","latitude","longitude","album","Albums","id3genre","licence_artwork","releasedate","filename"]
).

processTag({startElement,[],"location",[],[]},Acc) -> [Path,N,Artist] = Acc,
  processStartTag("location",[Path,N+1,Artist++[{<<"location">>,[]}]]);
processTag({startElement,[],"Albums",[],[]},Acc) -> [Path,N,Artist] = Acc,
  processStartTag("Albums",[Path,N,Artist++[{<<"Albums">>,[]}]]);
processTag({startElement,[],"album",[],[]},Acc) -> [Path,N,Artist] = Acc,
  {Key,Albums} = getByKey(Artist,<<"Albums">>,1),
  processStartTag("album",[Path,N+1,lists:keyreplace(Key,1,Artist,{Key,Albums++[{<<"album">>,[]}]})]);

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
  io:format("~p, Path: ~p, Data: ~p~n",[N,Path,Data]),
  case Path of
    %Artist
               {["artist"],[]} -> Acc;
               {["id"],["artist"]} -> setArtistProperty(Path,Artist,Data,N,<<"id">>);
               {["name"],["artist"]} -> setArtistProperty(Path,Artist,Data,N,<<"name">>);
               {["url"],["artist"]} -> setArtistProperty(Path,Artist,Data,N,<<"url">>);
               {["image"],["artist"]} -> setArtistProperty(Path,Artist,Data,N,<<"image">>);
               {["mbgid"],["artist"]} -> setArtistProperty(Path,Artist,Data,N,<<"mbgid">>);
    %Location
               {["city","location"],["artist"]} -> setLocationProperty(Path,Artist,Data,N,<<"city">>);
               {["state","location"],["artist"]} -> setLocationProperty(Path,Artist,Data,N,<<"state">>);
               {["country","location"],["artist"]} -> setLocationProperty(Path,Artist,Data,N,<<"country">>);
               {["latitude","location"],["artist"]} -> setLocationProperty(Path,Artist,Data,N,<<"latitude">>);
               {["longitude","location"],["artist"]} -> setLocationProperty(Path,Artist,Data,N,<<"longitude">>);
   %Albums
               {["id","album","Albums"],["artist"]} -> setAlbumProperty(Path,Artist,Data,N,<<"id">>);
               {["name","album","Albums"],["artist"]} -> setAlbumProperty(Path,Artist,Data,N,<<"name">>);
               {["url","album","Albums"],["artist"]} -> setAlbumProperty(Path,Artist,Data,N,<<"url">>);
               {["id3genre","album","Albums"],["artist"]} -> setAlbumProperty(Path,Artist,Data,N,<<"id3genre">>);
               {["mbgid","album","Albums"],["artist"]} -> setAlbumProperty(Path,Artist,Data,N,<<"mbgid">>);
               {["license_artwork","album","Albums"],["artist"]} -> setAlbumProperty(Path,Artist,Data,N,<<"license_artwork">>);
               {["releasedate","album","Albums"],["artist"]} -> setAlbumProperty(Path,Artist,Data,N,<<"releasedate">>);
               {["filename","album","Albums"],["artist"]} -> setAlbumProperty(Path,Artist,Data,N,<<"filename">>);
    _Else    -> Acc
  end;

processTag(_,ok) -> [];
processTag(_,Acc) -> Acc.

processStartTag("artist", _) -> Path = queue:new(), [queue:in("artist",Path),0,[]];
processStartTag(T,Acc) -> [Path,N,Artist] = Acc, [queue:in(T,Path),N,Artist].


processEndTag("artist",Acc) -> io:format("~p~n",[Acc]), [];
processEndTag("Albums",Acc) -> [Path,_,Artist] = Acc, {_,Queue} = queue:out_r(Path), [ Queue, 0, Artist];
processEndTag("location",Acc) -> [Path,_,Artist] = Acc, {_,Queue} = queue:out_r(Path), [Queue, 0, Artist];
processEndTag(_,Acc) -> [Path,N,Artist] = Acc, {_,Queue} = queue:out_r(Path), [Queue, N, Artist].

setAlbumProperty(Path,Artist,Data,N,Property) -> T = {Property,Data},
  {_,Albums} = getByKey(Artist,<<"Albums">>,1),
  {_,Album} = lists:nth(N,Albums),
  Replaced = replaceElement(Albums,N,{<<"album">>,Album++[T]}),
  NewArtist = lists:keyreplace(<<"Albums">>,1,Artist,{<<"Albums">>,Replaced}),
  [Path,N,NewArtist].

setLocationProperty(Path,Artist,Data,N,Property) -> T = {Property,Data}, {Key,Loc} = getByKey(Artist,<<"location">>,N),
  [Path,N,lists:keyreplace(Key,1,Artist,{Key,Loc++[T]})].

setArtistProperty(Path,Artist,Data,N,Property) -> [Path,N,Artist++[{Property,Data}]].

replaceElement(List,N,Element) ->
%  io:format("List: ~p, N: ~p, Element: ~p~n",[List,N,Element]),
  case N of
    1 -> []++[Element];
    _Else -> lists:sublist(List,N-1) ++ [Element]
  end.

getByKey(Artist,Key,Pos) ->  lists:keyfind(Key, Pos, Artist).


%% this is just to make it easier to test this little example
xml(File) -> filename:join([codeDir(), File]).
codeDir() -> filename:dirname(code:which(?MODULE)).