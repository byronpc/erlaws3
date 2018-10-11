%%%-------------------------------------------------------------------
%% @doc HTTP API functions for AWS s3 Multipart Upload
%% @author Byron Wang <byronpc1@gmail.com>
%% @end
%%%-------------------------------------------------------------------
-module(erlaws3_lib).
-export([
  single_upload/5,
  initiate_multipart_upload/4,
  list_parts/5,
  upload_part/6,
  upload_part/7,
  complete_multipart_upload/6,
  abort_multipart_upload/5
]).

-include_lib("exml/include/exml.hrl").
-define(SCOPE, "s3").

%%====================================================================
%% @doc Single Upload
%%====================================================================
single_upload(ConnPid, BucketUrl, ObjectName, AwsRegion, File) ->
  single_upload(ConnPid, BucketUrl, ObjectName, AwsRegion, File, 0).

single_upload(ConnPid, BucketUrl, ObjectName, AwsRegion, File, Retry) ->
  MaxRetry = application:get_env(erlaws3, max_retry, 3),
  Headers = erlaws3_headers:generate(BucketUrl, "PUT", ObjectName, "", AwsRegion, ?SCOPE),
  Result = erlaws3_utils:http_stream(ConnPid, put, ObjectName, Headers, File),
  case Result of
    {ok, #{status_code := 200, headers := Resp}} ->
      {<<"ETag">>, Etag} = lists:keyfind(<<"ETag">>, 1, Resp),
      {ok, Etag};
    {error, Error} ->
      if Retry < MaxRetry ->
        single_upload(ConnPid, BucketUrl, ObjectName, AwsRegion, File, Retry + 1);
      true ->
        {error, Error}
      end;
    E -> E % unhandled errors if any
  end.

%%====================================================================
%% @doc Initiate Multipart Upload
%%====================================================================
initiate_multipart_upload(ConnPid, BucketUrl, ObjectName, AwsRegion) ->
  Query = "uploads=",
  Headers = erlaws3_headers:generate(BucketUrl, "POST", ObjectName, Query, AwsRegion, ?SCOPE),
  case erlaws3_utils:http_post(ConnPid, ObjectName ++ "?" ++ Query, Headers, <<>>) of
    {ok, #{status_code := 200, body := Xml}} ->
      {ok, binary_to_list(exml_query:cdata(exml_query:subelement(Xml, <<"UploadId">>)))};
    {_, Error} ->
      {error, Error} % unhandled errors if any
  end.

%%====================================================================
%% @doc List Parts
%%====================================================================
list_parts(ConnPid, BucketUrl, ObjectName, AwsRegion, UploadId) ->
  Query = "uploadId=" ++ UploadId,
  Headers = erlaws3_headers:generate(BucketUrl, "GET", ObjectName, Query, AwsRegion, ?SCOPE),
  case erlaws3_utils:http_get(ConnPid, ObjectName ++ "?" ++ Query, Headers) of
    {ok, #{status_code := 200, body := #xmlel{children = Children}}} ->
      %% initiator/owner keys skipped
      {ok, [{Name, Cdata} || #xmlel{name = Name, children = [{xmlcdata, Cdata}]} <- Children]};
    {_, Error} ->
      {error, Error} % unhandled errors if any
  end.

%%====================================================================
%% @doc Upload Part
%% Use upload_part/6 for succeeding parts to spawn new http connection
%%====================================================================
upload_part(BucketUrl, ObjectName, AwsRegion, UploadId, PartNumber, Payload) ->
  upload_part(BucketUrl, ObjectName, AwsRegion, UploadId, PartNumber, Payload, 0).

upload_part(BucketUrl, ObjectName, AwsRegion, UploadId, PartNumber, Payload, Retry) ->
  {ok, ConnPid} = erlaws3_utils:http_open(BucketUrl, 443),
  MaxRetry = application:get_env(erlaws3, max_retry, 3),
  Query = "partNumber=" ++ integer_to_list(PartNumber) ++ "&uploadId=" ++ UploadId,
  Headers = erlaws3_headers:generate(BucketUrl, "PUT", ObjectName, Query, AwsRegion, ?SCOPE),
  Result = erlaws3_utils:http_put(ConnPid, ObjectName ++ "?" ++ Query, Headers, Payload),
  case Result of
    {ok, #{status_code := 200, headers := Resp}} ->
      {<<"ETag">>, Etag} = lists:keyfind(<<"ETag">>, 1, Resp),
      {ok, Etag};
    {_, Error} ->
      erlaws3_utils:http_close(ConnPid),
      if Retry < MaxRetry ->
        upload_part(BucketUrl, ObjectName, AwsRegion, UploadId, PartNumber, Payload, Retry + 1);
      true ->
        {error, Error}
      end
  end.

%%====================================================================
%% @doc Complete Multipart Upload Part
%%====================================================================
complete_multipart_upload(ConnPid, BucketUrl, ObjectName, AwsRegion, UploadId, Parts) ->
  Query = "uploadId=" ++ UploadId,
  Headers = erlaws3_headers:generate(BucketUrl, "POST", ObjectName, Query, AwsRegion, ?SCOPE),
  PartsXml = [
    #xmlel{name = <<"Part">>, children = [
      #xmlel{name = <<"PartNumber">>, children = [{xmlcdata, integer_to_binary(PartNumber)}]},
      #xmlel{name = <<"ETag">>, children = [{xmlcdata, Etag}]}
    ]}
  || {PartNumber, Etag} <- Parts],
  Payload = exml:to_binary(#xmlel{name = <<"CompleteMultipartUpload">>, children = PartsXml}),
  case erlaws3_utils:http_post(ConnPid, ObjectName ++ "?" ++ Query, Headers, Payload) of
    {ok, #{body := #xmlel{name = <<"CompleteMultipartUploadResult">>} = Xml}} ->
      {ok, exml_query:cdata(exml_query:subelement(Xml, <<"ETag">>))};
    {ok, #{body := #xmlel{name = <<"Error">>, children = Children}}} ->
      {error, [{Name, Cdata} || #xmlel{name = Name, children = [{xmlcdata, Cdata}]} <- Children]};
    {_, Error} ->
      {error, Error} % unhandled errors if any
  end.

abort_multipart_upload(ConnPid, BucketUrl, ObjectName, AwsRegion, UploadId) ->
  Query = "uploadId=" ++ UploadId,
  Headers = erlaws3_headers:generate(BucketUrl, "DELETE", ObjectName, Query, AwsRegion, ?SCOPE),

  case erlaws3_utils:http_delete(ConnPid, ObjectName ++ "?" ++ Query, Headers) of
    {ok, #{status_code := 204}} ->
      {ok, true};
    {ok, #{status_code := 404}} ->
      {error, no_such_upload};
    {_, Error} ->
      {error, Error} % unhandled errors if any
  end.