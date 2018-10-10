%%%-------------------------------------------------------------------
%% @doc Mail API for uploading files and binary data to AWS s3
%% @author Byron Wang <byronpc1@gmail.com>
%% @end
%%%-------------------------------------------------------------------
-module(erlaws3).
-export([
  upload/0,
  upload/4,
  upload/5
]).

-define(BUCKET_URL(Bucket), Bucket ++ ".s3.amazonaws.com").
-define(MIN_CHUNK_SIZE, 5242880). % S3 Minumum Size for Multipart Upload

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
    FileSize when FileSize > ?MIN_CHUNK_SIZE ->

      % initiate multipart upload
      {ok, UploadId} = erlaws3_lib:initiate_multipart_upload(ConnPid, BucketUrl, ObjectName, AwsRegion),
      ChunkSize = application:get_env(erlaws3, chunk_size, ?MIN_CHUNK_SIZE),
      Chunks = ceil(FileSize/ChunkSize),
      case upload_chunks(BucketUrl, ObjectName, AwsRegion, UploadId, File, Chunks, ChunkSize) of
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

%% Utility function to spawn multiple process for uploading chunks
upload_chunks(BucketUrl, ObjectName, AwsRegion, UploadId, File, Chunks, ChunkSize) ->
  Timeout = application:get_env(erlaws3, chunk_upload_timeout, 60000),

  Caller = self(),

  %% open file
  {ok, Fid} = file:open(File, [read]),

  %% spawn process per chunk upload
  Pids = [ {PartNumber, spawn_link(fun() ->

    %% read bytes
    StartByte = (PartNumber - 1) * ChunkSize,
    {ok, Bytes} = file:pread(Fid, [{StartByte, ChunkSize}]),

    %% upload bytes
    Result = erlaws3_lib:upload_part(BucketUrl, ObjectName, AwsRegion, UploadId, PartNumber, Bytes),

    %% send result to caller
    Caller ! {self(), Result}

  end)} || PartNumber <- lists:seq(1, Chunks)],

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

