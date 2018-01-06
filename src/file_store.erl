-module(file_store). 
-export([handle/2, init/3]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
split_range(Range) ->
    [list_to_integer(I) || I <- string:tokens(binary_to_list(Range), "-")].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
set_resp_body_fun(Size, FilePath, Req) ->
    Fun = fun(S, T) -> T:sendfile(S, FilePath) end,
    cowboy_req:set_resp_body_fun(Size, Fun, Req).
set_resp_body_fun(First, Size, FilePath, Req) ->
    Fun = fun(S, T) -> T:sendfile(S, FilePath, First, Size, []) end,
    cowboy_req:set_resp_body_fun(Size, Fun, Req).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
file_headers(Size, Path) ->
    file_headers(0, Size - 1, Size, Path).
file_headers(First, Last, Size, Path) ->
    {Type, SubType, []} = cow_mimetypes:all(list_to_binary(Path)),
    [{<<"content-type">>, <<Type/binary, $/, SubType/binary>>},
     {<<"content-length">>, integer_to_binary(Last - First + 1)},
     {<<"accept-ranges">>, <<"bytes">>},
     {<<"content-range">>, <<"bytes ",
                             (integer_to_binary(First))/binary, $-,
                             (integer_to_binary(Last))/binary, $/,
                             (integer_to_binary(Size))/binary>>}].

%%------------------------------------------------------------------------------

store_path(ID) -> 
  Root = "store/",
  string:concat(Root, ID). 

delete(ID, Req) ->
  file:delete(store_path(ID)), 
  Body = lists:flatten( io_lib:format("File ~p was deleted from store ~n", ID) ) ,
  cowboy:req_reply(200, #{}, Body, Req). 

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
  case cowboy_req:part(Req) of 
    {ok, Headers, Req2} -> 

      {file, _FieldName, _Filename, _CType, _CTransferEncoding} = cow_multipart:form_data(Headers),
      {ok, FD} = file:open(store_path(ID), [append]),
      upload_file(Req2, FD),

      Body = lists:flatten( io_lib:format("File ~p was uploaded to store ~n", ID) ),
      cowboy_req:reply(200, #{}, Body, Req2);

    {done, Req} ->
      Body = "No file was uploaded \n",
      cowboy_req:reply(400, #{}, Body, Req)
  end.

get_id(Req) ->  
  {ID, _} = cowboy_req:binding(id, Req),
  ID.
      
init(_Transport, Req, State) ->
  {ok, Req, State}.
    
reply_file(FilePath, Req0) ->
  FileSize = filelib:file_size(FilePath),
  case cowboy_req:header(<<"range">>, Req0) of
    {Range, Req0} when Range =:= undefined; Range =:= <<>> ->
      Req1 = set_resp_body_fun(FileSize, FilePath, Req0),
      Headers = file_headers(FileSize, FilePath),
      cowboy_req:reply(200, Headers, Req1);
    {<<"bytes=", Range/binary>>, Req0} ->
      case split_range(Range) of
        [F] when F >= 0, F < FileSize ->
          L = FileSize - 1;
        [F, L] when F >= 0, L > 0, L < FileSize, F < L ->
          ok
      end,
      Req1 = cowboy_req:set_resp_body_fun(F, L - F + 1, FilePath, Req0),
      Headers = file_headers(F, L, FileSize, FilePath),
      cowboy_req:reply(206, Headers, Req1)
  end.



handle(Req, State) ->
  {Method, _} = cowboy_req:method(Req),
  ID = get_id(Req),
  case Method of 
    <<"POST">> ->
      {ok, Rep} = create(ID, Req), 
      {ok, Rep, State};
    <<"DELETE">> ->
      {ok, Rep} = delete(ID, Req),
      {ok, Rep, State};
    <<"GET">> ->
      {ok, Rep} = reply_file(store_path(ID), Req), 
      {ok, Rep, State}
  end.
