%% Copyright
-module(xmlimporter).
-author("kris").

%% user interface
-export([start/0]).

-record(artist,{id="",name="",url="",image="", mbgid=""}).

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

processTag({startElement,[],"artist",[],[]},Acc) -> processStartArtist(Acc);
processTag({startElement,[],"id",[],[]},Acc) -> processProperty(hd(Acc),id, element(1,hd(Acc)));
processTag({endElement,[],"id",[]},Acc)->processProperty(hd(Acc),undefined, element(1,hd(Acc)));
processTag({startElement,[],"name",[],[]},Acc) -> processProperty(hd(Acc),name, element(1,hd(Acc)));
processTag({endElement,[],"name",[]},Acc) -> processProperty(hd(Acc),undefined, element(1,hd(Acc)));
processTag({startElement,[],"url",[],[]},Acc) -> processProperty(hd(Acc),url, element(1,hd(Acc)));
processTag({endElement,[],"url",[]},Acc) -> processProperty(hd(Acc),undefined, element(1,hd(Acc)));
processTag({startElement,[],"mbgid",[],[]},Acc) -> processProperty(hd(Acc),mbgid, element(1,hd(Acc)));
processTag({endElement,[],"mbgid",[]},Acc) -> processProperty(hd(Acc),undefined, element(1,hd(Acc)));
processTag({startElement,[],"image",[],[]},Acc) -> processProperty(hd(Acc),image, element(1,hd(Acc)));
processTag({endElement,[],"image",[]},Acc) -> processProperty(hd(Acc),undefined, element(1,hd(Acc)));
processTag({startElement,[],_,[],[]},Acc) -> Acc;
processTag({characters,Data},Acc) -> processSingle(hd(Acc),Data,Acc);

processTag({endElement,[],"artist",[]},Acc) -> processEndArtist(Acc);
processTag(_,ok) -> [];
processTag(_,Acc) -> Acc.


processProperty({E,_},Property,Element) -> [{E,Property},Element].

processSingle({E,id},Data, Acc) when is_record(E,artist) -> [_,A] = Acc, [{E#artist{id=Data},undefined},A];
processSingle({E,name},Data,Acc) when is_record(E,artist) -> [_,A] = Acc, [{E#artist{name=Data},undefined},A];
processSingle({E,url},Data,Acc) when is_record(E,artist) -> [_,A] = Acc, [{E#artist{url=Data},undefined},A];
processSingle({E,mbgid},Data,Acc) when is_record(E,artist) -> [_,A] = Acc, [{E#artist{mbgid=Data},undefined},A];
processSingle({E,image},Data,Acc) when is_record(E,artist) -> [_,A] = Acc, [{E#artist{image=Data},undefined},A];
processSingle({_,undefined},_,Acc) -> Acc.

%% Artist processing

processStartArtist(_) -> io:format("~p~n",["start artist"]), E = #artist{id="",name="",url="",mbgid=""}, [{E,undefined},E].
processEndArtist(Acc) -> [_,ToPrint] = Acc, io:format("~p ~p~n",["end artist: ", ToPrint]), [].

%% this is just to make it easier to test this little example
xml(File) -> filename:join([codeDir(), File]).
codeDir() -> filename:dirname(code:which(?MODULE)).