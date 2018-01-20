-module(file_store). 
-export([init/2]).

store_path(ID) -> 
  {ok, CWD} = file:get_cwd(),  
  Root = "store/",
  filename:join([CWD, Root, ID]). 

delete(ID, Req) ->
  file:delete(store_path(ID)), 
  Body = io_lib:format("File ~p was deleted from store ~n", [ID]) ,
  cowboy_req:reply(200, #{}, Body, Req). 

upload_file(Req0, FD) ->
  case cowboy_req:read_part_body(Req0) of
    {ok, _LastBodyChunk, Req} ->
      file:write(FD, _LastBodyChunk),
      file:close(FD),
      Req;
    {more, _BodyChunk, Req} ->
      file:write(FD, _BodyChunk), 
      upload_file(Req, FD)
  end.

create(ID, Req) -> 
  case cowboy_req:read_part(Req, #{}) of 
    {ok, _, Req2} -> 
      {ok, FD} = file:open(store_path(ID), [append]),
      upload_file(Req2, FD),
      Body = io_lib:format("File ~p was uploaded to store ~n", [ID]),
      cowboy_req:reply(200, #{}, Body, Req2);

    {done, Req} ->
      Body = "No file was uploaded \n",
      cowboy_req:reply(400, #{}, Body, Req)
  end.

get_id(Req) ->  
 cowboy_req:binding(id, Req).
      
reply_file(Path, Req) ->
  Rep = cowboy_req:stream_reply(200, Req),
  {ok, FD} = file:open(Path, [read]),
  stream_file(FD, Rep).

stream_file(FD, Rep) ->
  case file:read(FD, 1024) of 
    {ok, Data} ->
      cowboy_req:stream_body(Data, nofin, Rep),
      stream_file(FD, Rep);
    eof ->
      cowboy_req:stream_body(<<"">>, fin, Rep)
  end. 

  

init(Req, State) ->
  Method = cowboy_req:method(Req),
  ID = get_id(Req),
  case Method of 
    <<"POST">> ->
      Rep = create(ID, Req), 
      {ok, Rep, State};
    <<"DELETE">> ->
      Rep = delete(ID, Req),
      {ok, Rep, State};
    <<"GET">> ->
      Rep = reply_file(store_path(ID), Req), 
      {ok, Rep, State}
  end.
