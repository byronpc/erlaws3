%%%-------------------------------------------------------------------
%% @doc Utilities
%% @author Byron Wang <byronpc1@gmail.com>
%% @end
%%%-------------------------------------------------------------------
-module(erlaws3_utils).
-export([
  get_date/0,
  get_timestamp/0,
  sha256_to_hex/1,
  http_open/2,
  http_close/1,
  http_get/3,
  http_post/4,
  http_put/4,
  http_stream/5,
  http_stream/7,
  http_delete/3,
  http_response/2
]).

-define(DEFAULT_CHUNK_SIZE, 1048576).

%%====================================================================
%% @doc Utilities
%%====================================================================
get_date() ->
  {Y,M,D} = date(),
  list_to_binary(io_lib:format("~4..0w~2..0w~2..0w", [Y,M,D])).

get_timestamp() ->
  {{Y,M,D},{H,Mi,S}}   = calendar:universal_time(),
  list_to_binary(io_lib:format("~4..0w~2..0w~2..0wT~2..0w~2..0w~2..0wZ", [Y,M,D,H,Mi,S])).

sha256_to_hex(<<Bin:256/big-unsigned-integer>>) ->
  list_to_binary(io_lib:format("~64.16.0b", [Bin])).

http_open(Url, Port) ->
  hackney:connect(hackney_ssl, Url, Port).

http_close(ConnPid) ->
  hackney:close(ConnPid).

http_get(ConnPid, Path, Headers) ->
  http_request(ConnPid, get, Path, Headers, <<>>).

http_post(ConnPid, Path, Headers, Payload) ->
  http_request(ConnPid, post, Path, Headers, Payload).

http_put(ConnPid, Path, Headers, Payload) ->
  http_request(ConnPid, put, Path, Headers, Payload).

http_delete(ConnPid, Path, Headers) ->
  http_request(ConnPid, delete, Path, Headers, <<>>).

http_stream(ConnPid, Method, Path, Headers, File) ->
  FileSize = filelib:file_size(File),
  {ok, Fid} = file:open(File, [read]),
  http_stream(ConnPid, Method, Path, Headers, Fid, 0, FileSize).

http_stream(ConnPid, Method, Path, Headers, Fid, Offset, ContentSize) ->
  ChunkSize = application:get_env(erlaws3, chunk_size, ?DEFAULT_CHUNK_SIZE),
  Headers1 = [{<<"content-length">>, ContentSize}|Headers],
  hackney:send_request(ConnPid, {Method, Path, Headers1, stream}),
  chunk_send_body(ConnPid, Fid, ChunkSize, Offset, ContentSize),
  Response = hackney:start_response(ConnPid),
  http_response(ConnPid, Response).

http_request(ConnPid, Method, Path, Headers, Payload) ->
  Response = hackney:send_request(ConnPid, {Method, Path, Headers, Payload}),
  http_response(ConnPid, Response).

http_response(ConnPid, Response) ->
  case Response of
    {ok, StatusCode, RespHeaders, _Ref} ->
      {ok, Body} = hackney:body(ConnPid),
      Resp = case erlxml:parse(Body) of
        {ok, Xml} -> Xml;
        _ -> Body
      end,
      {ok, #{status_code => StatusCode, headers => RespHeaders, body => Resp}};
    E ->
      E
  end.

chunk_send_body(ConnPid, Fid, ChunkSize, Offset, ContentSize) ->
  NextContentSize = ContentSize - ChunkSize,
  NextOffset = Offset + ChunkSize,
  if NextContentSize < 1 ->
    {ok, Bytes} = file:pread(Fid, [{Offset, ContentSize}]),
    ok = hackney:send_body(ConnPid, Bytes);
  true ->
    {ok, Bytes} = file:pread(Fid, [{Offset, ChunkSize}]),
    ok = hackney:send_body(ConnPid, Bytes),
    chunk_send_body(ConnPid, Fid, ChunkSize, NextOffset, NextContentSize)
  end.
