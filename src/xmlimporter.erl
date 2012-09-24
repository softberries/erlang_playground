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
    ["artist","id","name","image","url","mbgid","country","state","city","location","latitude","longitude","album","Albums","id3genre","licence_artwork","releasedate","filename","Tracks", "track","license", "Tags", "tag","idstr","weight"]
).

addTracks(Path,AlbumNumber,TrackNumber, TagNumber, Artist) ->
%  io:format("Tracks started ~p, AlbumNumber: ~p~n",[Path,AlbumNumber]),
  {_,Albums} = getByKeyFromArtist(Artist,<<"Albums">>),
  {_,Album} = lists:nth(AlbumNumber,Albums),
  Replaced = replaceElement(Albums,AlbumNumber,{<<"album">>,Album++[{<<"Tracks">>,[]}]}),
  NewArtist = lists:keyreplace(<<"Albums">>,1,Artist,{<<"Albums">>,Replaced}),
  [Path,AlbumNumber,TrackNumber,TagNumber,NewArtist].

addTags(Path,AlbumNumber,TrackNumber,TagNumber,Artist) ->
%  io:format("Tags started ~p, AlbumNumber: ~p, TrackNumber: ~p~n",[Path,AlbumNumber,TrackNumber]),
  {_,Albums} = getByKeyFromArtist(Artist,<<"Albums">>),
  {_,Tracks} = getByKeyFromAlbum(Artist,<<"Tracks">>,AlbumNumber),
  {_,Album} = lists:nth(AlbumNumber,Albums),
  {_,Track} = getByKeyFromTracks(Tracks,TrackNumber),
  TracksNew = replaceElement(Tracks,TrackNumber,{<<"track">>,Track++[{<<"Tags">>,[]}]}),
  AlbumNew = lists:keyreplace(<<"Tracks">>,1,Album,{<<"Tracks">>,TracksNew}),
  AlbumsNew = replaceElement(Albums,AlbumNumber,{<<"album">>,AlbumNew}),
  NewArtist = lists:keyreplace(<<"Albums">>,1,Artist,{<<"Albums">>,AlbumsNew}),
  [Path,AlbumNumber,TrackNumber,TagNumber,NewArtist].

processTag({startElement,[],"location",[],[]},Acc) -> [Path,AlbumNumber,TrackNumber,TagNumber,Artist] = Acc,
  processStartTag("location",[Path,AlbumNumber+1,TrackNumber,TagNumber,Artist++[{<<"location">>,[]}]]);
processTag({startElement,[],"Albums",[],[]},Acc) -> [Path,AlbumNumber,TrackNumber,TagNumber,Artist] = Acc,
  processStartTag("Albums",[Path,AlbumNumber,TrackNumber, TagNumber,Artist++[{<<"Albums">>,[]}]]);

processTag({startElement,[],"album",[],[]},Acc) -> [Path,AlbumNumber,TrackNumber,TagNumber,Artist] = Acc,
  {Key,Albums} = getByKeyFromArtist(Artist,<<"Albums">>),
  processStartTag("album",[Path,AlbumNumber+1,TrackNumber,TagNumber,lists:keyreplace(Key,1,Artist,{Key,Albums++[{<<"album">>,[]}]})]);

processTag({startElement,[],"track",[],[]},Acc) -> [Path,AlbumNumber,TrackNumber,TagNumber,Artist] = Acc,
  {_,Albums} = getByKeyFromArtist(Artist,<<"Albums">>),
%  io:format("ALBUMS: ~p~n",[Albums]),
  {KeyT,Tracks} = getByKeyFromAlbum(Artist,<<"Tracks">>,AlbumNumber),
%  io:format("TRACKS: ~p~n",[Tracks]),
  {_,Album} = lists:nth(AlbumNumber,Albums),
%  io:format("ALBUM: ~p~n",[Album]),
  TracksNew = lists:keyreplace(KeyT,1,Album,{KeyT,Tracks++[{<<"track">>,[]}]}),
%  io:format("TRACKS_NEW: ~p KeyT: ~p~n",[TracksNew, KeyT]),
  AlbumsNew = replaceElement(Albums,AlbumNumber,{<<"album">>,TracksNew}),
%  io:format("ALBUMS_NEW: ~p~n",[AlbumsNew]),
  NewArtist = lists:keyreplace(<<"Albums">>,1,Artist,{<<"Albums">>,AlbumsNew}),
%  io:format("ARTIST: ~p~n",[NewArtist]),
  processStartTag("track",[Path,AlbumNumber,TrackNumber+1,TagNumber,NewArtist]);

processTag({startElement,[],"tag",[],[]},Acc) -> [Path,AlbumNumber,TrackNumber,TagNumber,Artist] = Acc,
  {_,Albums} = getByKeyFromArtist(Artist,<<"Albums">>),
%  io:format("Albums Tag: ~p~n",[Albums]),
  {_,Tracks} = getByKeyFromAlbum(Artist,<<"Tracks">>,AlbumNumber),
%  io:format("Tracks: ~p~n",[Tracks]),
  {_,Album} = lists:nth(AlbumNumber,Albums),
%  io:format("Album Tag: ~p~n",[Album]),
  {_,Track} = lists:nth(TrackNumber,Tracks),
%  io:format("Track: ~p~n",[Track]),
  {_,Tags} = lists:keyfind(<<"Tags">>, 1, Track),
%  io:format("Tags: ~p~n",[Tags]),
  TrackNew = lists:keyreplace(<<"Tags">>,1,Track,{<<"Tags">>,Tags++[{<<"tag">>,[]}]}),
%  io:format("TrackNew: ~p~n",[TrackNew]),
  TracksNew = replaceElement(Tracks,TrackNumber,{<<"track">>,TrackNew}),
%  io:format("TRACKS_NEW: ~p~n",[TracksNew]),
  AlbumNew = lists:keyreplace(<<"Tracks">>,1,Album,{<<"Tracks">>,TracksNew}),
%  io:format("AlbumNew: ~p~n",[AlbumNew]),
  AlbumsNew = replaceElement(Albums,AlbumNumber,{<<"album">>,AlbumNew}),
%  io:format("ALBUMS_NEW: ~p~n",[AlbumsNew]),
  NewArtist = lists:keyreplace(<<"Albums">>,1,Artist,{<<"Albums">>,AlbumsNew}),
%  io:format("ARTIST: ~p~n",[NewArtist]),
  processStartTag("tag",[Path,AlbumNumber,TrackNumber,TagNumber+1,NewArtist]);


processTag({startElement,[],"Tracks",[],[]},Acc) -> [Path,AlbumNumber,TrackNumber,TagNumber,Artist] = Acc, processStartTag("Tracks",addTracks(Path,AlbumNumber,TrackNumber, TagNumber,Artist));
processTag({startElement,[],"Tags",[],[]},Acc) -> [Path,AlbumNumber,TrackNumber,TagNumber,Artist] = Acc, processStartTag("Tags",addTags(Path,AlbumNumber,TrackNumber, TagNumber,Artist));

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

processTag({characters,Data},Acc) -> [Path,AlbumNumber,TrackNumber,TagNumber,Artist] = Acc,
%  io:format("AlbumNr: ~p, TrackNr: ~p, TagNr: ~p, Path: ~p, Data: ~p~n",[AlbumNumber,TrackNumber,TagNumber,Path,Data]),
  case Path of
    %Artist
               {["artist"],[]} -> Acc;
               {["id"],["artist"]} -> setArtistProperty(Path,Artist,Data,AlbumNumber,TrackNumber,TagNumber,<<"id">>);
               {["name"],["artist"]} -> setArtistProperty(Path,Artist,Data,AlbumNumber,TrackNumber,TagNumber,<<"name">>);
               {["url"],["artist"]} -> setArtistProperty(Path,Artist,Data,AlbumNumber,TrackNumber,TagNumber,<<"url">>);
               {["image"],["artist"]} -> setArtistProperty(Path,Artist,Data,AlbumNumber,TrackNumber,TagNumber,<<"image">>);
               {["mbgid"],["artist"]} -> setArtistProperty(Path,Artist,Data,AlbumNumber,TrackNumber,TagNumber,<<"mbgid">>);
    %Location
               {["city","location"],["artist"]} -> setLocationProperty(Path,Artist,Data,AlbumNumber,TrackNumber,TagNumber,<<"city">>);
               {["state","location"],["artist"]} -> setLocationProperty(Path,Artist,Data,AlbumNumber,TrackNumber,TagNumber,<<"state">>);
               {["country","location"],["artist"]} -> setLocationProperty(Path,Artist,Data,AlbumNumber,TrackNumber,TagNumber,<<"country">>);
               {["latitude","location"],["artist"]} -> setLocationProperty(Path,Artist,Data,AlbumNumber,TrackNumber,TagNumber,<<"latitude">>);
               {["longitude","location"],["artist"]} -> setLocationProperty(Path,Artist,Data,AlbumNumber,TrackNumber,TagNumber,<<"longitude">>);
    %Albums
               {["id","album","Albums"],["artist"]} -> setAlbumProperty(Path,Artist,Data,AlbumNumber,TrackNumber,TagNumber,<<"id">>);
               {["name","album","Albums"],["artist"]} -> setAlbumProperty(Path,Artist,Data,AlbumNumber,TrackNumber,TagNumber,<<"name">>);
               {["url","album","Albums"],["artist"]} -> setAlbumProperty(Path,Artist,Data,AlbumNumber,TrackNumber,TagNumber,<<"url">>);
               {["id3genre","album","Albums"],["artist"]} -> setAlbumProperty(Path,Artist,Data,AlbumNumber,TrackNumber,TagNumber,<<"id3genre">>);
               {["mbgid","album","Albums"],["artist"]} -> setAlbumProperty(Path,Artist,Data,AlbumNumber,TrackNumber,TagNumber,<<"mbgid">>);
               {["license_artwork","album","Albums"],["artist"]} -> setAlbumProperty(Path,Artist,Data,AlbumNumber,TrackNumber,TagNumber,<<"license_artwork">>);
               {["releasedate","album","Albums"],["artist"]} -> setAlbumProperty(Path,Artist,Data,AlbumNumber,TrackNumber,TagNumber,<<"releasedate">>);
               {["filename","album","Albums"],["artist"]} -> setAlbumProperty(Path,Artist,Data,AlbumNumber,TrackNumber,TagNumber,<<"filename">>);
    %Tracks
               {["id","track","Tracks","album","Albums"],["artist"]} -> setTrackProperty(Path,Artist,Data,AlbumNumber,TrackNumber,TagNumber,<<"id">>);
               {["name","track","Tracks","album","Albums"],["artist"]} -> setTrackProperty(Path,Artist,Data,AlbumNumber,TrackNumber,TagNumber,<<"name">>);
               {["filename","track","Tracks","album","Albums"],["artist"]} -> setTrackProperty(Path,Artist,Data,AlbumNumber,TrackNumber,TagNumber,<<"filename">>);
               {["mbgid","track","Tracks","album","Albums"],["artist"]} -> setTrackProperty(Path,Artist,Data,AlbumNumber,TrackNumber,TagNumber,<<"mbgid">>);
               {["numalbum","track","Tracks","album","Albums"],["artist"]} -> setTrackProperty(Path,Artist,Data,AlbumNumber,TrackNumber,TagNumber,<<"numalbum">>);
               {["id3genre","track","Tracks","album","Albums"],["artist"]} -> setTrackProperty(Path,Artist,Data,AlbumNumber,TrackNumber,TagNumber,<<"id3genre">>);
               {["license","track","Tracks","album","Albums"],["artist"]} -> setTrackProperty(Path,Artist,Data,AlbumNumber,TrackNumber,TagNumber,<<"license">>);
    %Tags
               {["idstr","tag","Tags","track","Tracks","album","Albums"],["artist"]} -> setTagProperty(Path,Artist,Data,AlbumNumber,TrackNumber,TagNumber,<<"idstr">>);
               {["weight","tag","Tags","track","Tracks","album","Albums"],["artist"]} -> setTagProperty(Path,Artist,Data,AlbumNumber,TrackNumber,TagNumber,<<"weight">>);
    _Else    -> Acc
  end;

processTag(_,ok) -> [];
processTag(_,Acc) -> Acc.

processStartTag("artist", _) -> Path = queue:new(), [queue:in("artist",Path),0,0,0,[]];
processStartTag(T,Acc) -> [Path,AlbumNumber,TrackNumber,TagNumber,Artist] = Acc, [queue:in(T,Path),AlbumNumber,TrackNumber,TagNumber,Artist].


processEndTag("artist",Acc) -> io:format("~p~n",[Acc]), [];
processEndTag("Albums",Acc) -> [Path,_,_,_,Artist] = Acc, {_,Queue} = queue:out_r(Path), [ Queue, 0,0,0, Artist];
processEndTag("Tracks",Acc) -> [Path,AlbumNumber,_,_,Artist] = Acc, {_,Queue} = queue:out_r(Path), [ Queue, AlbumNumber,0,0, Artist];
processEndTag("Tags",Acc) -> [Path,AlbumNumber,TrackNumber,_,Artist] = Acc, {_,Queue} = queue:out_r(Path), [ Queue, AlbumNumber,TrackNumber,0, Artist];
processEndTag("location",Acc) -> [Path,_,_,_,Artist] = Acc, {_,Queue} = queue:out_r(Path), [Queue, 0,0,0, Artist];
processEndTag(_,Acc) -> [Path,AlbumNumber,TrackNumber,TagNumber,Artist] = Acc, {_,Queue} = queue:out_r(Path), [Queue, AlbumNumber,TrackNumber,TagNumber, Artist].

setAlbumProperty(Path,Artist,Data,AlbumNumber,TrackNumber,TagNumber,Property) -> T = {Property,Data},
  {_,Albums} = getByKeyFromArtist(Artist,<<"Albums">>),
  {_,Album} = lists:nth(AlbumNumber,Albums),
  Replaced = replaceElement(Albums,AlbumNumber,{<<"album">>,Album++[T]}),
  NewArtist = lists:keyreplace(<<"Albums">>,1,Artist,{<<"Albums">>,Replaced}),
  [Path,AlbumNumber,TrackNumber,TagNumber,NewArtist].

setTrackProperty(Path,Artist,Data,AlbumNumber,TrackNumber,TagNumber,Property) -> T = {Property,Data},
  {_,Albums} = getByKeyFromArtist(Artist,<<"Albums">>),
%  io:format("ALBUMS: ~p~n",[Albums]),
  {_,Tracks} = getByKeyFromAlbum(Artist,<<"Tracks">>,AlbumNumber),
%  io:format("TRACKS: ~p~n",[Tracks]),
  {_,Album} = lists:nth(AlbumNumber,Albums),
%  io:format("Album: ~p~n",[Album]),
  {_,Track} = getByKeyFromTracks(Tracks,TrackNumber),
%  io:format("TRACK: ~p~n",[Track]),
  TracksNew = replaceElement(Tracks,TrackNumber,{<<"track">>,Track++[T]}),
%  io:format("TRACKS_NEW: ~p KeyT: ~n",[TracksNew]),
  AlbumNew = lists:keyreplace(<<"Tracks">>,1,Album,{<<"Tracks">>,TracksNew}),
%  io:format("ALBUM NEW: ~p~n",[AlbumNew]),
  AlbumsNew = replaceElement(Albums,AlbumNumber,{<<"album">>,AlbumNew}),
  NewArtist = lists:keyreplace(<<"Albums">>,1,Artist,{<<"Albums">>,AlbumsNew}),
  [Path,AlbumNumber,TrackNumber,TagNumber,NewArtist].

setTagProperty(Path,Artist,Data,AlbumNumber,TrackNumber,TagNumber,Property) -> T = {Property, Data},
  io:format("setting tagNr: ~p, tagName: ~p, tagValue: ~p~n",[TagNumber,Property,Data]),
  {_,Albums} = getByKeyFromArtist(Artist,<<"Albums">>),
%  io:format("ALBUMS: ~p~n",[Albums]),
  {_,Tracks} = getByKeyFromAlbum(Artist,<<"Tracks">>,AlbumNumber),
%  io:format("TRACKS: ~p~n",[Tracks]),
  {_,Album} = lists:nth(AlbumNumber,Albums),
%  io:format("Album: ~p~n",[Album]),
  {_,Track} = getByKeyFromTracks(Tracks,TrackNumber),
  {_,Tags} = getByKeyFromTrack(Track,<<"Tags">>),
  io:format("Tags: ~p~n",[Tags]),
  {_,Tag} = lists:nth(TagNumber,Tags),
  io:format("Tag: ~p~n",[Tag]),
  TagsNew = replaceElement(Tags,TagNumber,{<<"tag">>,Tag++[T]}),
  io:format("Tagnew: ~p~n",[TagsNew]),
  TrackNew = lists:keyreplace(<<"Tags">>,1,Track,{<<"Tags">>,TagsNew}),
  io:format("Track New: ~p~n",[TrackNew]),
  TracksNew = replaceElement(Tracks,TrackNumber,{<<"track">>,TrackNew}),
  io:format("Tracks New: ~p~n",[TracksNew]),
  AlbumNew = lists:keyreplace(<<"Tracks">>,1,Album,{<<"Tracks">>,TracksNew}),
  io:format("ALBUM NEW: ~p~n",[AlbumNew]),
  AlbumsNew = replaceElement(Albums,AlbumNumber,{<<"album">>,AlbumNew}),
  NewArtist = lists:keyreplace(<<"Albums">>,1,Artist,{<<"Albums">>,AlbumsNew}),
  [Path,AlbumNumber,TrackNumber,TagNumber,NewArtist].

setLocationProperty(Path,Artist,Data,AlbumNumber,TrackNumber, TagNumber,Property) -> T = {Property,Data}, {Key,Loc} = getByKeyFromArtist(Artist,<<"location">>),
  [Path,AlbumNumber,TrackNumber,TagNumber,lists:keyreplace(Key,1,Artist,{Key,Loc++[T]})].

setArtistProperty(Path,Artist,Data,AlbumNumber,TrackNumber,TagNumber,Property) -> [Path,AlbumNumber,TrackNumber,TagNumber,Artist++[{Property,Data}]].

replaceElement(List,Position,Element) ->
%  io:format("List: ~p, AlbumNumber: ~p, Element: ~p~n",[List,Position,Element]),
  case Position of
    1 -> []++[Element];
    _Else -> lists:sublist(List,Position-1) ++ [Element]
  end.

getByKeyFromArtist(Artist,Key) ->  lists:keyfind(Key, 1, Artist).

getByKeyFromAlbum(Artist,Key,AlbumNumber) -> {_,Albums} = getByKeyFromArtist(Artist,<<"Albums">>),
%  io:format("AlbumNumber: ~p, Albums: ~p",[AlbumNumber,Albums]),
  {_,Album} = lists:nth(AlbumNumber,Albums),
%  io:format("Albumx: ~p, Key: ~p, AlbumNumber: ~p~n",[Album,Key,AlbumNumber]),
  lists:keyfind(Key, 1, Album).

getByKeyFromTracks(Tracks,TrackNumber) -> lists:nth(TrackNumber,Tracks).

getByKeyFromTrack(Track,Key) -> lists:keyfind(Key,1,Track).

%% this is just to make it easier to test this little example
xml(File) -> filename:join([codeDir(), File]).
codeDir() -> filename:dirname(code:which(?MODULE)).