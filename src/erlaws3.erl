-module(erlaws3).
-export([upload/0]).
-define(SCOPE, "s3").
-define(BUCKET_URL(Bucket), Bucket ++ ".s3.amazonaws.com").

upload() ->
  initiate_multipart_upload("erlaws3", "/demo", "ap-southeast-1").

%%====================================================================
%% Initiate Multipart Upload
%%====================================================================
initiate_multipart_upload(Bucket, ObjectName, AwsRegion) ->
  BucketUrl = ?BUCKET_URL(Bucket),
  Headers = erlaws3_headers:generate(BucketUrl ++ ":443", "POST", ObjectName, "uploads=", AwsRegion, ?SCOPE),
  {ok, ConnPid} = erlaws3_utils:http_open(BucketUrl, 443),
  case erlaws3_utils:http_request(ConnPid, post, ObjectName ++ "?uploads=", Headers, <<>>, #{}) of
    {ok, #{status_code := 200, body := Xml}} ->
      UploadIdXml = exml_query:subelement(Xml, <<"UploadId">>),
      {ok, exml_query:cdata(UploadIdXml)};
    E -> E
  end.
