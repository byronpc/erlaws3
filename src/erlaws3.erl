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
  {ok, C} = upload_part(BucketUrl, ObjectName, AwsRegion, UploadId, 1, <<"a">>),
  Parts = [{1, C}],
  complete_multipart_upload(ConnPid, BucketUrl, ObjectName, AwsRegion, UploadId, Parts).

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
    E -> E % unhandled errors if any
  end.

%%====================================================================
%% List Parts
%%====================================================================
list_parts(ConnPid, BucketUrl, ObjectName, AwsRegion, UploadId) ->
  Query = "uploadId=" ++ UploadId,
  Headers = erlaws3_headers:generate(BucketUrl ++ ":443", "GET", ObjectName, Query, AwsRegion, ?SCOPE),
  case erlaws3_utils:http_get(ConnPid, ObjectName ++ "?" ++ Query, Headers, #{}) of
    {ok, #{status_code := 200, body := #xmlel{children = Children}}} ->
      %% initiator/owner keys skipped
      {ok, [{Name, Cdata} || #xmlel{name = Name, children = [{xmlcdata, Cdata}]} <- Children]};
    E -> E % unhandled errors if any
  end.

%%====================================================================
%% Upload Part
%% Use upload_part/7 for first part upload to reuse http connection
%% Use upload_part/6 for succeeding parts to spawn new http connection
%%====================================================================
upload_part(BucketUrl, ObjectName, AwsRegion, UploadId, PartNumber, Payload) ->
  {ok, ConnPid} = erlaws3_utils:http_open(BucketUrl, 443),
  upload_part(ConnPid, BucketUrl, ObjectName, AwsRegion, UploadId, PartNumber, Payload).

upload_part(ConnPid, BucketUrl, ObjectName, AwsRegion, UploadId, PartNumber, Payload) ->
  Query = "partNumber=" ++ integer_to_list(PartNumber) ++ "&uploadId=" ++ UploadId,
  Headers = erlaws3_headers:generate(BucketUrl ++ ":443", "PUT", ObjectName, Query, AwsRegion, ?SCOPE),
  Result = erlaws3_utils:http_put(ConnPid, ObjectName ++ "?" ++ Query, Headers, Payload, #{}),
  ok = erlaws3_utils:http_close(ConnPid),
  case Result of
    {ok, #{status_code := 200, headers := Resp}} ->
      {<<"etag">>, Etag} = lists:keyfind(<<"etag">>, 1, Resp),
      {ok, Etag};
    E -> E % unhandled errors if any
  end.

%%====================================================================
%% Complete Multipart Upload Part
%%====================================================================
complete_multipart_upload(ConnPid, BucketUrl, ObjectName, AwsRegion, UploadId, Parts) ->
  Query = "uploadId=" ++ UploadId,
  Headers = erlaws3_headers:generate(BucketUrl ++ ":443", "POST", ObjectName, Query, AwsRegion, ?SCOPE),
  PartsXml = [
    #xmlel{name = <<"Part">>, children = [
      #xmlel{name = <<"PartNumber">>, children = [{xmlcdata, integer_to_binary(PartNumber)}]},
      #xmlel{name = <<"ETag">>, children = [{xmlcdata, Etag}]}
    ]}
  || {PartNumber, Etag} <- Parts],
  Payload = exml:to_binary(#xmlel{name = <<"CompleteMultipartUpload">>, children = PartsXml}),
  case erlaws3_utils:http_post(ConnPid, ObjectName ++ "?" ++ Query, Headers, Payload, #{}) of
    {ok, #{body := #xmlel{name = <<"CompleteMultipartUploadResult">>, children = Children}}} ->
      {ok, [{Name, Cdata} || #xmlel{name = Name, children = [{xmlcdata, Cdata}]} <- Children]};
    {ok, #{body := #xmlel{name = <<"Error">>, children = Children}}} ->
      {error, [{Name, Cdata} || #xmlel{name = Name, children = [{xmlcdata, Cdata}]} <- Children]};
    E -> E % unhandled errors if any
  end.