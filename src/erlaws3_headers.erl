%%%-------------------------------------------------------------------
%% @doc Header Generator for AWS Signature Version 4 (Unsigned Payload)
%% @end
%%%-------------------------------------------------------------------
-module(erlaws3_headers).
-export([generate/6]).
-define(DELIM, "----------------------------------------------------------------").
-define(PAYLOAD_HASH, "UNSIGNED-PAYLOAD").

%%====================================================================
%% Header Generator
%%====================================================================
generate(Host, HttpVerb, CanonicalUri, CanonicalQueryString, AwsRegion, Scope) ->
  AccessKey = application:get_env(erlaws3, access_key, ""),
  Credential = string:join([AccessKey, get_date(), AwsRegion, Scope, "aws4_request"], "/"),
  Timestamp = get_timestamp(),
  Headers = [{"host", Host}, {"x-amz-date", Timestamp}],
  SignedHeaders = string:join([ H || {H,_} <- Headers], ";"),
  Authorization = "AWS4-HMAC-SHA256 " ++
    "Credential=" ++ Credential ++ ", " ++
    "SignedHeaders=" ++ SignedHeaders ++ ", " ++
    "Signature=" ++ generate_signature(HttpVerb, CanonicalUri, CanonicalQueryString, Headers, AwsRegion, Scope, Timestamp),
  logger:debug("~nAuthorization: ~s~n", [Authorization]),
  [{"Authorization", Authorization},
  {"x-amz-content-sha256", ?PAYLOAD_HASH},
  {"x-amz-date", Timestamp}].

%%====================================================================
%% Signature
%%====================================================================
generate_signature(HttpVerb, CanonicalUri, CanonicalQueryString, Headers, AwsRegion, Scope, Timestamp) ->
  CanonicalRequest = generate_canonical_request(HttpVerb, CanonicalUri, CanonicalQueryString, Headers),
  StringToSign = generate_string_to_sign(Timestamp, AwsRegion, Scope, CanonicalRequest),
  SigningKey = generate_signing_key(AwsRegion, Scope),
  SignatureBin = crypto:hmac(sha256, SigningKey, StringToSign),
  Signature = sha256_to_hex(SignatureBin),
  logger:debug("~nSignature: ~s~n", [Signature]),
  Signature.

%%====================================================================
%% Canonical Request
%%====================================================================
generate_canonical_request(HttpVerb, CanonicalUri, CanonicalQueryString, Headers) ->
  Headers1 = lists:sort(Headers),
  CanonicalHeaders = string:join([ string:to_lower(N) ++ ":" ++ string:strip(V) || {N, V} <- Headers1], "\n"),
  SignedHeaders = string:join([ string:to_lower(N) || {N, _V} <- Headers1], ";"),
  CanonicalRequest = string:join([
    HttpVerb,
    CanonicalUri,
    CanonicalQueryString,
    CanonicalHeaders ++ "\n",
    SignedHeaders,
    ?PAYLOAD_HASH], "\n"),
  Hash = sha256_to_hex(crypto:hash(sha256, CanonicalRequest)),
  logger:debug("~nCanonical Request~n~s~n~s~n~s~n~nHash: ~s~n",
    [?DELIM, CanonicalRequest, ?DELIM, Hash]),
  Hash.

%%====================================================================
%% String To Sign
%%====================================================================
generate_string_to_sign(Timestamp, AwsRegion, Scope, CanonicalRequest) ->
  StringToSign = string:join([
    "AWS4-HMAC-SHA256",
    Timestamp,
    string:join([get_date(), AwsRegion, Scope, "aws4_request"], "/"),
    CanonicalRequest], "\n"),
  logger:debug("~nString To Sign~n~s~n~s~n~s~n", [?DELIM, StringToSign, ?DELIM]),
  StringToSign.

%%====================================================================
%% Signing Key
%%====================================================================
generate_signing_key(AwsRegion, Scope) ->
  SecretKey = application:get_env(erlaws3, secret_key, ""),
  DateKey = crypto:hmac(sha256, "AWS4" ++ SecretKey, get_date()),
  RegionKey = crypto:hmac(sha256, DateKey, AwsRegion),
  ScopeKey = crypto:hmac(sha256, RegionKey, Scope),
  SigningKey = crypto:hmac(sha256, ScopeKey, "aws4_request"),
  logger:debug("~nDateKey: ~s~nRegionKey: ~s~nScopeKey: ~s~nSigningKey: ~s~n", [
    sha256_to_hex(DateKey),
    sha256_to_hex(RegionKey),
    sha256_to_hex(ScopeKey),
    sha256_to_hex(SigningKey)
  ]),
  SigningKey.

%%====================================================================
%% Utilities
%%====================================================================
get_date() ->
  {Y,M,D} = date(),
  lists:flatten(io_lib:format("~4..0w~2..0w~2..0w", [Y,M,D])).
  % "20150830".

get_timestamp() ->
  {{Y,M,D},{H,Mi,S}}   = calendar:universal_time(),
  lists:flatten(io_lib:format("~4..0w~2..0w~2..0wT~2..0w~2..0w~2..0wZ", [Y,M,D,H,Mi,S])).
  % "20150830T123600Z".

sha256_to_hex(<<Bin:256/big-unsigned-integer>>) ->
  lists:flatten(io_lib:format("~64.16.0b", [Bin])).
