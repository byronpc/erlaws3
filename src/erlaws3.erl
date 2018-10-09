%%%-------------------------------------------------------------------
%% @doc Mail API for uploading files and binary data to AWS s3
%% @author Byron Wang <byronpc1@gmail.com>
%% @end
%%%-------------------------------------------------------------------
-module(erlaws3).
-export([
  upload/0,
  upload/4
]).

-define(BUCKET_URL(Bucket), Bucket ++ ".s3.amazonaws.com").
-define(MIN_CHUNK_SIZE, 5242880). % S3 Minumum Size for Multipart Upload

upload() ->
  upload("erlaws3", "ap-southeast-1", "/pic2.jpg", "/mnt/c/Users/byron/Documents/erlaws3/pic.jpg").

%%====================================================================
%% @doc Upload File
%% upload("bucket", "region", "/sample_file", "file_path")
%%====================================================================
upload(Bucket, AwsRegion, ObjectName, File) ->
  BucketUrl = ?BUCKET_URL(Bucket),
  case erlaws3_utils:http_open(BucketUrl, 443) of
    {ok, ConnPid} ->
      case filelib:file_size(File) of

        % if file is more than 5mb, multipart upload the file
        FileSize when FileSize > ?MIN_CHUNK_SIZE ->

          % initiate multipart upload
          {ok, UploadId} = erlaws3_lib:initiate_multipart_upload(ConnPid, BucketUrl, ObjectName, AwsRegion),
          ChunkSize = application:get_env(erlaws3, chunk_size, ?MIN_CHUNK_SIZE),
          Chunks = ceil(FileSize/ChunkSize),
          Parts = upload_chunks(BucketUrl, ObjectName, AwsRegion, UploadId, File, Chunks, ChunkSize),
          erlaws3_lib:complete_multipart_upload(ConnPid, BucketUrl, ObjectName, AwsRegion, UploadId, Parts);

        % if file is less than 5MB, single upload the file
        _ ->
          {ok, Payload} = file:read_file(File),
          erlaws3_lib:single_upload(ConnPid, BucketUrl, ObjectName, AwsRegion, Payload)
      end;
    E -> E
  end.

%% Utility function to spawn multiple process for uploading chunks
upload_chunks(BucketUrl, ObjectName, AwsRegion, UploadId, File, Chunks, ChunkSize) ->
  Caller = self(),
  % open file
  {ok, Fid} = file:open(File, [read]),
  Pids = [ spawn_link(fun() ->
    {ok, Payload} = file:pread(Fid, [{((PartNumber-1)*ChunkSize), ChunkSize}]),
    {ok, Etag} = erlaws3_lib:upload_part(BucketUrl, ObjectName, AwsRegion, UploadId, PartNumber, Payload),
    Caller ! {self(), {PartNumber, Etag}}
  end) || PartNumber <- lists:seq(1, Chunks)],
  [ receive {Pid, R} -> R end || Pid <- Pids ].
