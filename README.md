# erlaws3: AWS S3 multipart upload tool for Erlang #

An AWS S3 multipart upload tool utilizing AWS Signature v4.

## Config ##

Make sure to define your access/secret keys in the AWS config

```
{erlaws3, [

  %% AWS keys
  {access_key, ""},
  {secret_key, ""},

  %% Make sure to define a reasonable upload timeout
  %% based on the chunk_size and max_retry parameters

  {max_retry, 3}, % multipart chunk upload retry
  {chunk_size, 5242880}, % multipart chunk split size (5MB-5GB)
  {chunk_upload_timeout, 60000} % multipart chunk upload timeout
]}
```

## Usage ##

If file size is less than 5MB, file will be uploaded via regular PUT operation

```
1> erlaws3:upload("bucket", "region", "/sample_file", "file_path")
{ok,<<"\"upload_etag\"">>}
```