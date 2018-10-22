%%%-------------------------------------------------------------------
%% @doc Mail API for uploading files and binary data to AWS s3
%% @author Byron Wang <byronpc1@gmail.com>
%% @end
%%%-------------------------------------------------------------------
-module(erlaws3).
-export([upload/3, upload/5, upload/6, delete/1, delete/3, delete/4]).

% Stream upload
-export([open_stream/3, open_stream/5, upload_to_stream/2, close_stream/1]).

-define(BUCKET_URL(Bucket), <<Bucket/binary, ".s3.amazonaws.com">>).
-define(MIN_PART_SIZE, 5242880). % S3 Minumum Size for Multipart Upload

-type etag() :: bitstring().
-type gun_pid() :: pid().

%%====================================================================
%% @doc Upload File
%% upload(<<"bucket">>, <<"region">>, <<"/aws_file_path">>, <<"/input_file_path">>)
%%====================================================================
-spec upload(bitstring(), bitstring(), list(tuple())) -> {ok, etag()} | {error, any()}.
  upload(ObjectName, File, ExtraHeaders) ->
  {ok, Bucket} = application:get_env(erlaws3, default_bucket),
  {ok, Region} = application:get_env(erlaws3, default_region),
  upload(Bucket, Region, ObjectName, File, ExtraHeaders).

-spec upload(bitstring(), bitstring(), bitstring(), bitstring(), list(tuple())) -> {ok, etag()} | {error, any()}.
upload(Bucket, AwsRegion, ObjectName, File, ExtraHeaders) ->
  BucketUrl = ?BUCKET_URL(Bucket),
  case erlaws3_utils:http_open(BucketUrl, 443) of
    {ok, ConnPid} ->
      Result = upload(ConnPid, Bucket, AwsRegion, ObjectName, File, ExtraHeaders),
      erlaws3_utils:http_close(ConnPid),
      Result;
    E -> E
  end.

-spec upload(gun_pid(), bitstring(), bitstring(), bitstring(), bitstring(), list(tuple())) -> {ok, etag()} | {error, any()}.
upload(ConnPid, Bucket, AwsRegion, ObjectName, File, ExtraHeaders) ->
  BucketUrl = ?BUCKET_URL(Bucket),
  case filelib:file_size(File) of

    % if file is more than 5mb, multipart upload the file
    FileSize when FileSize > ?MIN_PART_SIZE ->

      % initiate multipart upload
      {ok, UploadId} = erlaws3_lib:initiate_multipart_upload(ConnPid, BucketUrl, ObjectName, AwsRegion, ExtraHeaders),
      {PartCount, PartSize, LastSize} = define_parts(FileSize),

      case upload_parts(BucketUrl, ObjectName, AwsRegion, UploadId, File, PartCount, PartSize, LastSize) of
        {ok, Parts} ->
          erlaws3_lib:complete_multipart_upload(ConnPid, BucketUrl, ObjectName, AwsRegion, UploadId, Parts);
        {error, ErrorParts} ->
          erlaws3_lib:abort_multipart_upload(ConnPid, BucketUrl, ObjectName, AwsRegion, UploadId),
          {error, ErrorParts}
      end;

    % if file is less than 5MB, single upload the file
    _ ->
      erlaws3_lib:single_upload(ConnPid, BucketUrl, ObjectName, AwsRegion, File, ExtraHeaders)
  end.

%%====================================================================
%% @doc Delete File
%% delete(<<"bucket">>, <<"region">>, <<"/aws_file_path">>)
%%====================================================================
-spec delete(bitstring()) -> {ok, true} | {error, any()}.
delete(ObjectName) ->
  {ok, Bucket} = application:get_env(erlaws3, default_bucket),
  {ok, Region} = application:get_env(erlaws3, default_region),
  delete(Bucket, Region, ObjectName).

-spec delete(bitstring(), bitstring(), bitstring()) -> {ok, true} | {error, any()}.
delete(Bucket, AwsRegion, ObjectName) ->
  BucketUrl = ?BUCKET_URL(Bucket),
  case erlaws3_utils:http_open(BucketUrl, 443) of
    {ok, ConnPid} ->
      Result = delete(ConnPid, Bucket, AwsRegion, ObjectName),
      erlaws3_utils:http_close(ConnPid),
      Result;
    E -> E
  end.

-spec delete(gun_pid(), bitstring(), bitstring(), bitstring()) -> {ok, true} | {error, any()}.
delete(ConnPid, Bucket, AwsRegion, ObjectName) ->
  BucketUrl = ?BUCKET_URL(Bucket),
  erlaws3_lib:delete_object(ConnPid, BucketUrl, ObjectName, AwsRegion).

%%====================================================================
%% @doc Open Uploading Stream (For manual chunk uploading)
%% open_stream(<<"bucket">>, <<"region">>, <<"/aws_file_path">>, 10)
%%====================================================================
-spec open_stream(bitstring(), integer(), list(tuple())) -> {ok, pid()} | {error, any()}.
open_stream(ObjectName, ContentSize, ExtraHeaders) ->
  {ok, Bucket} = application:get_env(erlaws3, default_bucket),
  {ok, Region} = application:get_env(erlaws3, default_region),
  open_stream(Bucket, Region, ObjectName, ContentSize, ExtraHeaders).

-spec open_stream(bitstring(), bitstring(), bitstring(), integer(), list(tuple())) -> {ok, pid()} | {error, any()}.
open_stream(Bucket, AwsRegion, ObjectName, ContentSize, ExtraHeaders) ->
  BucketUrl = ?BUCKET_URL(Bucket),
  case erlaws3_utils:http_open(BucketUrl, 443) of
    {ok, ConnPid} ->
      erlaws3_lib:manual_stream_upload(ConnPid, BucketUrl, ObjectName, AwsRegion, ContentSize, ExtraHeaders);
    E -> E
  end.

%%====================================================================
%% @doc Manually upload Bytes to Stream
%% upload_to_stream(ConnPid, <<1,2,3,4>>)
%%====================================================================
-spec upload_to_stream(pid(), binary()) -> ok.
upload_to_stream(ConnPid, Bytes) ->
  hackney:send_body(ConnPid, Bytes).

%%====================================================================
%% @doc Close the stream
%% close_stream(ConnPid)
%%====================================================================
-spec close_stream(pid()) -> {ok, etag()} | {error, any()}.
close_stream(ConnPid) ->
  R1 = hackney:start_response(ConnPid),
  R2 = erlaws3_utils:http_response(ConnPid, R1),
  erlaws3_utils:http_close(ConnPid),
  case R2 of
    {ok, #{status_code := 200, headers := Resp}} ->
      {<<"ETag">>, Etag} = lists:keyfind(<<"ETag">>, 1, Resp),
      {ok, Etag};
    {_, Error} ->
      {error, Error}
  end.

%% Utility function to spawn multiple process for uploading parts
upload_parts(BucketUrl, ObjectName, AwsRegion, UploadId, File, PartCount, PartSize, LastSize) ->
  Timeout = application:get_env(erlaws3, part_upload_timeout, 60000),

  Caller = self(),

  %% open file
  {ok, Fid} = file:open(File, [read]),

  %% spawn process per part upload
  Pids = [ {PartNumber, spawn_link(fun() ->

    %% read bytes
    Offset = (PartNumber - 1) * PartSize,

    ContentSize = if PartNumber == PartCount ->
      LastSize;
    true ->
      PartSize
    end,

    %% upload bytes
    Result = erlaws3_lib:upload_part(BucketUrl, ObjectName, AwsRegion, UploadId, PartNumber, Fid, Offset, ContentSize),

    %% send result to caller
    Caller ! {self(), Result}

  end)} || PartNumber <- lists:seq(1, PartCount)],

  %% receive results
  Parts = [ receive {Pid, R} -> {PartNumber, R}
            after Timeout -> {PartNumber, {error, timeout}}
            end || {PartNumber, Pid} <- Pids ],

  %% close file
  file:close(Fid),

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
    LastSize = FileSize rem PartSize2,
    {MaxParts, PartSize2, LastSize};
  true ->
    PartCount = ceil(FileSize/PartSize),
    LastSize = FileSize rem PartSize,
    {PartCount, PartSize, LastSize}
  end.

