-module(util). 
-export([start_up/0, dl/0, upload/0, rm/0]).

start_up() ->
  application:start(magic_man),
  application:ensure_all_started(hackney),
  PoolName = test_pool,
  Options = [{timeout, 150000}, {max_connections, 200}],
  ok = hackney_pool:start_pool(PoolName, Options).
  
dl() ->
  ID = fs_client:get_hash("priv/test_file"),
  Host = "http://127.0.0.1:8080",
  fs_client:download_file(Host, ID).

upload() ->
  Host = "http://127.0.0.1:8080",
  fs_client:upload_file(Host, priv_path("test_file") ).

rm() ->
  ID = fs_client:get_hash("priv/test_file"),
  fs_client:delete_file("http://127.0.0.1:8080", ID).
priv_path(Filename) ->
    case code:priv_dir(magic_man) of
        {error, bad_name} ->
            % This occurs when not running as a release; e.g., erl -pa ebin
            % Of course, this will not work for all cases, but should account 
            % for most
            PrivDir = "priv";
        PrivDir ->
            % In this case, we are running in a release and the VM knows
            % where the application (and thus the priv directory) resides
            % on the file system
            ok
    end,
    filename:join([PrivDir, Filename]).