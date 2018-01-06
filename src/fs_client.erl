-module(fs_client). 
-export([get_hash/1, upload_file/2, download_file/2, delete_file/2]).

gen_uri(Host, HashID) ->
  BS = string:join([Host, "/store/", HashID], ""),
  list_to_binary(BS).

get_hash_path() -> 
  filename:join(["/priv", "get_hash"]).

get_hash(Path) -> 
  COM_STR = lists:flatten( io_lib:format("~p ~p", [get_hash_path(), Path]) ),
  Rep = os:cmd(COM_STR),
  string:trim(Rep).

upload_file(Host, FPath) -> 
  HashID = get_hash(FPath),
  URL = gen_uri(Host, HashID), 
  {ok, ClientRef} = hackney:request(post, URL, [], stream_multipart, []),
  hackney:send_multipart_body(ClientRef, {file, FPath}).


download_file(Host, ID) ->
  URL = gen_uri(Host, ID),
  {ok, ClientRef} = hackney:get(URL, [], []),
  {ok, FD} = file:open(ID, [append]),
  handle_download(ClientRef, FD). 
  
delete_file(Host, ID) ->
  URL = gen_uri(Host, ID),
  {ok, ClientRef} = hackney:delete(URL, [], []),
  hackney:body(ClientRef). 
  
handle_download(Ref, FD) ->
  case hackney:stream_multipart(Ref) of 
    {headers, _} ->
      handle_download(Ref, FD);
    {body, Data} ->
      file:write(FD, Data),
      handle_download(Ref, FD);
    end_of_part ->
      file:close(FD),
      ok;
    eof ->
      file:close(FD),
      ok
  end.
      
      
