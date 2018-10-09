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
  http_get/4,
  http_post/5,
  http_put/5
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
  case gun:open(Url, Port) of
    {ok, ConnPid} ->
      case gun:await_up(ConnPid) of
        {ok, _Protocol} -> {ok, ConnPid};
        E -> E
      end;
    E2 -> E2
  end.

http_close(ConnPid) ->
  gun:close(ConnPid).

http_get(ConnPid, Path, Headers, Opts) ->
  StreamRef = gun:get(ConnPid, Path, Headers, Opts),
  http_response(ConnPid, StreamRef).

http_post(ConnPid, Path, Headers, Payload, Opts) ->
  StreamRef = gun:post(ConnPid, Path, Headers, Payload, Opts),
  http_response(ConnPid, StreamRef).

http_put(ConnPid, Path, Headers, Payload, Opts) ->
  StreamRef = gun:put(ConnPid, Path, Headers, Payload, Opts),
  http_response(ConnPid, StreamRef).

http_response(ConnPid, StreamRef) ->
  case catch gun:await(ConnPid, StreamRef) of
    {response, fin, StatusCode, Headers} ->
      {ok, #{status_code => StatusCode, headers => Headers, body => <<>>}};
    {response, nofin, StatusCode, Headers} ->
      case catch gun:await_body(ConnPid, StreamRef) of
        {ok, Body} ->
          {ok, Xml} = exml:parse(Body),
          {ok, #{status_code => StatusCode, headers => Headers, body => Xml}};
        E ->
          E
      end;
    E2 ->
      E2
  end.