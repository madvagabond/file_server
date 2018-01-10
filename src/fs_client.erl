-module(fs_client). 
-export([get_hash/1, upload_file/2, download_file/2, delete_file/2, gen_uri/2]).

gen_uri(Host, HashID) ->
  BS = string:join([Host, "/store/", HashID], ""),
  list_to_binary(BS).

get_hash_path() -> 
  filename:join(["priv", "get_hash"]).

get_hash(Path) -> 
  COM_STR = lists:flatten( io_lib:format("~p ~p", [get_hash_path(), Path]) ),
  Rep = os:cmd(COM_STR),
  string:trim(Rep).

upload_file(Host, FPath) -> 
  HashID = get_hash(FPath),
  URL = gen_uri(Host, HashID), 
  {ok, Ref} = hackney:request(post, URL, [], stream_multipart, []),
  hackney:send_multipart_body(Ref, {file, list_to_binary(FPath)}),
  {ok, _, _, Ref2} = hackney:start_response(Ref),
  hackney:body(Ref2).


download_file(Host, ID) ->
  URL = gen_uri(Host, ID),
  {ok, _, _, ClientRef} = hackney:get(URL, [], []),
  {ok, FD} = file:open(ID, [append]),
  handle_download(ClientRef, FD). 
  
delete_file(Host, ID) ->
  URL = gen_uri(Host, ID),
  {ok, _, _, ClientRef} = hackney:delete(URL, [], []),
  hackney:body(ClientRef). 
  
handle_download(Ref, FD) ->
  case hackney:stream_body(Ref) of 
    {ok, Data} ->
      file:write(FD, Data),
      handle_download(Ref, FD);
    done ->
      file:close(FD), 
      ok
  end.
      
      
