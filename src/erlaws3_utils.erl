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
  http_delete/3
]).

%%====================================================================
%% @doc Utilities
%%====================================================================
get_date() ->
  {Y,M,D} = date(),
  lists:flatten(io_lib:format("~4..0w~2..0w~2..0w", [Y,M,D])).

get_timestamp() ->
  {{Y,M,D},{H,Mi,S}}   = calendar:universal_time(),
  lists:flatten(io_lib:format("~4..0w~2..0w~2..0wT~2..0w~2..0w~2..0wZ", [Y,M,D,H,Mi,S])).

sha256_to_hex(<<Bin:256/big-unsigned-integer>>) ->
  lists:flatten(io_lib:format("~64.16.0b", [Bin])).

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

http_request(ConnPid, Method, Path, Headers, Payload) ->
  case hackney:send_request(ConnPid, {Method, list_to_binary(Path), Headers, Payload}) of
    {ok, StatusCode, RespHeaders, _Ref} ->
      {ok, Body} = hackney:body(ConnPid),
      Resp = case exml:parse(Body) of
        {ok, Xml} -> Xml;
        _ -> Body
      end,
      {ok, #{status_code => StatusCode, headers => RespHeaders, body => Resp}};
    E ->
      E
  end.