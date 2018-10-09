%%%-------------------------------------------------------------------
%% @doc Header Generator for AWS Signature Version 4 (Unsigned Payload)
%% @author Byron Wang <byronpc1@gmail.com>
%% @end
%%%-------------------------------------------------------------------
-module(erlaws3_headers).
-export([generate/6]).
-define(DELIM, "----------------------------------------------------------------").
-define(PAYLOAD_HASH, "UNSIGNED-PAYLOAD").

%%====================================================================
%% @doc Header Generator
%%====================================================================
generate(Host, HttpVerb, CanonicalUri, CanonicalQueryString, AwsRegion, Scope) ->
  Date = erlaws3_utils:get_date(),
  Timestamp = erlaws3_utils:get_timestamp(),
  AccessKey = application:get_env(erlaws3, access_key, ""),
  Credential = string:join([AccessKey, Date, AwsRegion, Scope, "aws4_request"], "/"),
  Headers = [{"host", Host}, {"x-amz-date", Timestamp}],
  SignedHeaders = string:join([ H || {H,_} <- Headers], ";"),
  Signature = generate_signature(HttpVerb, CanonicalUri, CanonicalQueryString, Headers, AwsRegion, Scope, Date, Timestamp),
  Authorization = lists:flatten(["AWS4-HMAC-SHA256 ",
    "Credential=", Credential, ", ",
    "SignedHeaders=", SignedHeaders, ", ",
    "Signature=", Signature]),
  [
    {"Authorization", Authorization},
    {"x-amz-content-sha256", ?PAYLOAD_HASH},
    {"x-amz-date", Timestamp}
  ].

%%====================================================================
%% @doc Signature
%%====================================================================
generate_signature(HttpVerb, CanonicalUri, CanonicalQueryString, Headers, AwsRegion, Scope, Date, Timestamp) ->
  CanonicalRequest = generate_canonical_request(HttpVerb, CanonicalUri, CanonicalQueryString, Headers),
  StringToSign = generate_string_to_sign(Date, Timestamp, AwsRegion, Scope, CanonicalRequest),
  SigningKey = generate_signing_key(Date, AwsRegion, Scope),
  erlaws3_utils:sha256_to_hex(crypto:hmac(sha256, SigningKey, StringToSign)).

%%====================================================================
%% @doc Canonical Request
%%====================================================================
generate_canonical_request(HttpVerb, CanonicalUri, CanonicalQueryString, Headers) ->
  Headers1 = lists:sort(Headers),
  CanonicalHeaders = lists:flatten([[string:to_lower(N), ":", string:strip(V), "\n"] || {N, V} <- Headers1]),
  SignedHeaders = string:join([ string:to_lower(N) || {N, _V} <- Headers1], ";"),
  CanonicalRequest = string:join([
    HttpVerb, CanonicalUri,
    CanonicalQueryString,
    CanonicalHeaders,
    SignedHeaders,
    ?PAYLOAD_HASH], "\n"),
  erlaws3_utils:sha256_to_hex(crypto:hash(sha256, CanonicalRequest)).

%%====================================================================
%% @doc String To Sign
%%====================================================================
generate_string_to_sign(Date, Timestamp, AwsRegion, Scope, CanonicalRequest) ->
  string:join([
    "AWS4-HMAC-SHA256",
    Timestamp,
    string:join([Date, AwsRegion, Scope, "aws4_request"], "/"),
    CanonicalRequest], "\n").

%%====================================================================
%% @doc Signing Key
%%====================================================================
generate_signing_key(Date, AwsRegion, Scope) ->
  SecretKey = application:get_env(erlaws3, secret_key, ""),
  DateKey = crypto:hmac(sha256, "AWS4" ++ SecretKey, Date),
  RegionKey = crypto:hmac(sha256, DateKey, AwsRegion),
  ScopeKey = crypto:hmac(sha256, RegionKey, Scope),
  crypto:hmac(sha256, ScopeKey, "aws4_request").