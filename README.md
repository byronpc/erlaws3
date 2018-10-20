# erlaws3: AWS s3 multipart upload tool for erlang #

An AWS s3 multipart upload tool utilizing AWS Signature v4 with automatic retry mechanism in case of single or part upload failure.

## Config ##

Make sure to define your access/secret keys in the AWS config

```erlang
{erlaws3, [

  %% AWS keys
  {access_key, <<>>},
  {secret_key, <<>>},

  {default_bucket, <<>>},
  {default_region, <<>>},

  %% if file_size > max_parts * part_size
  %% part_size = file_size / max_parts
  %% AWS part size minimum is 5242880 and maximum is 5GB
  %% AWS parts maximum is 10000

  {max_retry, 3}, % maximum upload retry per part
  {max_parts, 10000}, % maximum number of parts per upload
  {part_size, 5242880}, % multipart split size

  {chunk_size, 1048576}, % file stream chunk size in memory

  %% Make sure to define a reasonable upload timeout
  %% based on the part_size and max_retry parameters

  {part_upload_timeout, 60000} % multipart upload timeout
]}
```

## Usage ##

If file size is less than 5MB, file will be uploaded via regular PUT operation

```erlang
1> erlaws3:upload(<<"bucket">>, <<"region">>, <<"/aws_file_path">>, <<"/input_file_path">>)
{ok,<<"\"upload_etag\"">>}

2> erlaws3:delete(<<"bucket">>, <<"region">>, <<"/aws_file_path">>)
{ok, true}
```

You can also manually upload chunks to AWS

```erlang
1> {ok, Pid} = erlaws3:open_stream(<<"bucket">>, <<"region">>, <<"/aws_file_path">>, 10).
{ok,#Ref<0.2819056575.3481534465.64411>}
2> erlaws3:upload_to_stream(Pid, <<"abc">>).
ok
3> erlaws3:upload_to_stream(Pid, <<"def">>).
ok
4> erlaws3:upload_to_stream(Pid, <<"ghi">>).
ok
5> erlaws3:upload_to_stream(Pid, <<"j">>).
ok
6> erlaws3:close_stream(Pid).
{ok,<<"\"79b7c0e419a42d3cf53b8d6e959c5259\"">>}
```