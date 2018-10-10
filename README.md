# erlaws3: AWS S3 multipart upload tool for Erlang #

An AWS S3 multipart upload tool utilizing AWS Signature v4.

## Config ##

Make sure to define your access/secret keys in the AWS config

```
{erlaws3, [

  %% AWS keys
  {access_key, ""},
  {secret_key, ""},

  %% if file_size > max_parts * part_size
  %% part_size = file_size / max_parts
  %% AWS part size minimum is 5242880 and maximum is 5GB
  %% AWS parts maximum is 10000

  {max_retry, 3}, % maximum upload retry per part
  {max_parts, 10000}, % maximum number of parts per upload
  {part_size, 5242880}, % multipart split size

  %% Make sure to define a reasonable upload timeout
  %% based on the part_size and max_retry parameters

  {part_upload_timeout, 60000} % multipart upload timeout
]}
```

## Usage ##

If file size is less than 5MB, file will be uploaded via regular PUT operation

```
1> erlaws3:upload("bucket", "region", "/sample_file", "file_path")
{ok,<<"\"upload_etag\"">>}
```