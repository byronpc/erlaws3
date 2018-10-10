%%%-------------------------------------------------------------------
%% @doc Mail API for uploading files and binary data to AWS s3
%% @author Byron Wang <byronpc1@gmail.com>
%% @end
%%%-------------------------------------------------------------------
-module(erlaws3).
-export([upload/4, upload/5]).
-define(BUCKET_URL(Bucket), Bucket ++ ".s3.amazonaws.com").
-define(MIN_PART_SIZE, 5242880). % S3 Minumum Size for Multipart Upload

%%====================================================================
%% @doc Upload File
%% upload("bucket", "region", "/sample_file", "file_path")
%%====================================================================
upload(Bucket, AwsRegion, ObjectName, File) ->
  BucketUrl = ?BUCKET_URL(Bucket),
  case erlaws3_utils:http_open(BucketUrl, 443) of
    {ok, ConnPid} ->
      Result = upload(ConnPid, Bucket, AwsRegion, ObjectName, File),
      erlaws3_utils:http_close(ConnPid),
      Result;
    E -> E
  end.

upload(ConnPid, Bucket, AwsRegion, ObjectName, File) ->
  BucketUrl = ?BUCKET_URL(Bucket),
  case filelib:file_size(File) of

    % if file is more than 5mb, multipart upload the file
    FileSize when FileSize > ?MIN_PART_SIZE ->

      % initiate multipart upload
      {ok, UploadId} = erlaws3_lib:initiate_multipart_upload(ConnPid, BucketUrl, ObjectName, AwsRegion),
      {PartCount, PartSize} = define_parts(FileSize),

      case upload_parts(BucketUrl, ObjectName, AwsRegion, UploadId, File, PartCount, PartSize) of
        {ok, Parts} ->
          erlaws3_lib:complete_multipart_upload(ConnPid, BucketUrl, ObjectName, AwsRegion, UploadId, Parts);
        {error, ErrorParts} ->
          erlaws3_lib:abort_multipart_upload(ConnPid, BucketUrl, ObjectName, AwsRegion, UploadId),
          {error, ErrorParts}
      end;

    % if file is less than 5MB, single upload the file
    _ ->
      {ok, Payload} = file:read_file(File),
      erlaws3_lib:single_upload(ConnPid, BucketUrl, ObjectName, AwsRegion, Payload)
  end.

%% Utility function to spawn multiple process for uploading parts
upload_parts(BucketUrl, ObjectName, AwsRegion, UploadId, File, PartCount, PartSize) ->
  Timeout = application:get_env(erlaws3, part_upload_timeout, 60000),

  Caller = self(),

  %% open file
  {ok, Fid} = file:open(File, [read]),

  %% spawn process per part upload
  Pids = [ {PartNumber, spawn_link(fun() ->

    %% read bytes
    StartByte = (PartNumber - 1) * PartSize,
    {ok, Bytes} = file:pread(Fid, [{StartByte, PartSize}]),

    %% upload bytes
    Result = erlaws3_lib:upload_part(BucketUrl, ObjectName, AwsRegion, UploadId, PartNumber, Bytes),

    %% send result to caller
    Caller ! {self(), Result}

  end)} || PartNumber <- lists:seq(1, PartCount)],

  %% receive results
  Parts = [ receive {Pid, R} -> {PartNumber, R}
            after Timeout -> {PartNumber, {error, timeout}}
            end || {PartNumber, Pid} <- Pids ],

  %% check if all uploads are successful
  {Success, Errors} = lists:foldr(fun({PartNumber, {Status, Result}}, {S, E}) ->
    case Status of
      ok -> {[{PartNumber, Result}|S], E};
      error -> {S, [{PartNumber, Result}|E]}
    end
  end, {[], []}, Parts),

  if Errors == [] ->
    {ok, Success};
  true ->
    {error, Errors}
  end.

define_parts(FileSize) ->
  PartSize = application:get_env(erlaws3, part_size, ?MIN_PART_SIZE),
  MaxParts = application:get_env(erlaws3, max_parts, 10000),

  if (FileSize > MaxParts * PartSize) ->
    PartSize2 = ceil(FileSize/MaxParts),
    {MaxParts, PartSize2};
  true ->
    PartCount = ceil(FileSize/PartSize),
    {PartCount, PartSize}
  end.

