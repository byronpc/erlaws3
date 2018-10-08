-module(erlaws3).
-export([upload/0]).
-include_lib("exml/include/exml.hrl").
-define(SCOPE, "s3").
-define(BUCKET_URL(Bucket), Bucket ++ ".s3.amazonaws.com").

upload() ->
  ObjectName = "/demo",
  AwsRegion = "ap-southeast-1",
  BucketUrl = ?BUCKET_URL("erlaws3"),
  {ok, ConnPid} = erlaws3_utils:http_open(BucketUrl, 443),
  {ok, UploadId} = initiate_multipart_upload(ConnPid, BucketUrl, ObjectName, AwsRegion),
  io:format("A ~p~n", [UploadId]),
  B = list_parts(ConnPid, BucketUrl, ObjectName, AwsRegion, UploadId),
  io:format("B ~p~n", [B]),
  C = upload_part(ConnPid, BucketUrl, ObjectName, AwsRegion, UploadId, 1, <<"a">>),
  io:format("C ~p~n", [C]).

%%====================================================================
%% Initiate Multipart Upload
%%====================================================================
initiate_multipart_upload(ConnPid, BucketUrl, ObjectName, AwsRegion) ->
  Query = "uploads=",
  Headers = erlaws3_headers:generate(BucketUrl ++ ":443", "POST", ObjectName, Query, AwsRegion, ?SCOPE),
  case erlaws3_utils:http_post(ConnPid, ObjectName ++ "?" ++ Query, Headers, <<>>, #{}) of
    {ok, #{status_code := 200, body := Xml}} ->
      UploadIdXml = exml_query:subelement(Xml, <<"UploadId">>),
      {ok, binary_to_list(exml_query:cdata(UploadIdXml))};
    E -> E
  end.

%%====================================================================
%% List Parts
%%====================================================================
list_parts(ConnPid, BucketUrl, ObjectName, AwsRegion, UploadId) ->
  Query = "uploadId=" ++ UploadId,
  Headers = erlaws3_headers:generate(BucketUrl ++ ":443", "GET", ObjectName, Query, AwsRegion, ?SCOPE),
  case erlaws3_utils:http_get(ConnPid, ObjectName ++ "?" ++ Query, Headers, #{}) of
    {ok, #{status_code := 200, body := #xmlel{children = Xml}}} ->
      %% initiator/owner keys skipped
      {ok, [{Name, Cdata} || #xmlel{name = Name, children = [{xmlcdata, Cdata}]} <- Xml]};
    E -> E
  end.

%%====================================================================
%% Upload Part
%%====================================================================
upload_part(ConnPid, BucketUrl, ObjectName, AwsRegion, UploadId, PartNumber, Payload) ->
  Query = "partNumber=" ++ integer_to_list(PartNumber) ++ "&uploadId=" ++ UploadId,
  Headers = erlaws3_headers:generate(BucketUrl ++ ":443", "PUT", ObjectName, Query, AwsRegion, ?SCOPE),
  case erlaws3_utils:http_put(ConnPid, ObjectName ++ "?" ++ Query, Headers, Payload, #{}) of
    {ok, #{status_code := 200, headers := Resp}} ->
      {<<"etag">>, Etag} = lists:keyfind(<<"etag">>, 1, Resp),
      {ok, re:replace(Etag, "\"", "", [{return, list}, global])};
    E -> E
  end.