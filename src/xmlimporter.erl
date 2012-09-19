%% Copyright
-module(xmlimporter).
-author("kris").

%% user interface
-export([start/0]).

-record(artist,{id="",name="",url="",mbgid=""}).

start() -> run("/Users/kris/Downloads/dbdump_artistalbumtrack.0.290905586176.xml",0).

run(File, Result) ->
  case file:read_file(xml(File)) of
    {ok, Bin} ->
      {ok,_,[]} = erlsom:parse_sax(Bin, [], fun callback/2);
    Error ->
      Error
  end,
  Result.

callback(Event, Acc) -> processTag(Event,Acc).

processTag({startElement,[],"artist",[],[]},Acc) -> processStartArtist(Acc);
processTag({startElement,[],"id",[],[]},Acc) -> [{<<"id">>}] ++ Acc;
processTag({startElement,[],"name",[],[]},Acc) -> [{<<"name">>}] ++ Acc;
processTag({startElement,[],"url",[],[]},Acc) -> [{<<"url">>}] ++ Acc;
processTag({startElement,[],"mbgid",[],[]},Acc) -> [{<<"mbgid">>}] ++ Acc;

processTag({characters,Data},[]) -> [];
processTag({characters,Data},Acc) -> Val = {element(1,hd(Acc)),Data}, [Val] ++ lists:nthtail(1,Acc);

processTag({endElement,[],"artist",[]},Acc) -> processEndArtist(Acc);
processTag(_,ok) -> [];
processTag(_,Acc) -> Acc.



%% Artist processing

processStartArtist(Acc) -> io:format("~p~n",["start artist"]), [].
processEndArtist(Acc) -> io:format("~p ~p~n",["end artist: ", Acc]).
%% this is just to make it easier to test this little example
xml(File) -> filename:join([codeDir(), File]).
codeDir() -> filename:dirname(code:which(?MODULE)).