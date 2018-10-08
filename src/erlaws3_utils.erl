-module(erlaws3_utils).
-export([
  get_date/0,
  get_timestamp/0,
  sha256_to_hex/1,
  http_open/2,
  http_request/6
]).

%%====================================================================
%% Utilities
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
  {ok, ConnPid} = gun:open(Url, Port),
  {ok, _Protocol} = gun:await_up(ConnPid),
  {ok, ConnPid}.

http_request(ConnPid, Method, Path, Headers, Body, Opts) ->
  StreamRef = gun:Method(ConnPid, Path, Headers, Body, Opts),
  case gun:await(ConnPid, StreamRef) of
    {response, fin, StatusCode, _Headers} ->
      {ok, #{body => <<>>, status_code => StatusCode}};
    {response, nofin, StatusCode, _Headers} ->
      case gun:await_body(ConnPid, StreamRef) of
        {ok, Resp} ->
          {ok, Xml} = exml:parse(Resp),
          {ok, #{body => Xml, status_code => StatusCode}};
        E ->
          E
      end;
    E2 ->
      E2
  end.