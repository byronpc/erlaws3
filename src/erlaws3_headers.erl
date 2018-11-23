%%%-------------------------------------------------------------------
%% @doc Header Generator for AWS Signature Version 4 (Unsigned Payload)
%% @author Byron Wang <byronpc1@gmail.com>
%% @end
%%%-------------------------------------------------------------------
-module(erlaws3_headers).

-export([
        generate/6,
        generate/7
        ]).
-define(PAYLOAD_HASH, <<"UNSIGNED-PAYLOAD">>).

-define(X_AMZ_SECURITY_TOKEN, <<"x-amz-security-token">>).
-define(X_AMZ_DATE, <<"x-amz-date">>).
-define(X_AMZ_CONTENT_SHA256, <<"x-amz-content-sha256">>).
-define(X_AMZ_PREFIX_MATCH, <<"x-amz-", _/binary>>).

-define(HOST, <<"host">>).

%%====================================================================
%% @doc Header Generator
%%====================================================================
generate(Host, HttpVerb, CanonicalUri, CanonicalQueryString, AwsRegion, Scope) ->
  generate(Host, HttpVerb, CanonicalUri, CanonicalQueryString, AwsRegion, Scope, []).

generate(Host, HttpVerb, CanonicalUri, CanonicalQueryString, AwsRegion, Scope, ExtraHeaders) ->
  Now = calendar:universal_time(),
  Date = erlaws3_utils:get_date(Now),
  Timestamp = erlaws3_utils:get_timestamp(Now),
  BaseHeaders = [ {?HOST, Host},
                  {?X_AMZ_CONTENT_SHA256, ?PAYLOAD_HASH},
                  {?X_AMZ_DATE, Timestamp}] ++ 
                  calc_token_header(application:get_env(erlaws3, token, <<>>)),
  SignedHeaders = lists:sort([ H || {?X_AMZ_PREFIX_MATCH, _} = H <- ExtraHeaders] ++ BaseHeaders),
  AuthorizationHeader = calc_authorization_header( application:get_env(erlaws3, access_key, <<>>), SignedHeaders, HttpVerb, 
                          CanonicalUri, CanonicalQueryString, AwsRegion, Scope, Date, Timestamp),  
  AuthorizationHeader ++ BaseHeaders.

calc_token_header(<<>>) -> [];
calc_token_header(AwsToken) ->
  [{?X_AMZ_SECURITY_TOKEN, AwsToken}].

calc_authorization_header(<<>>, _, _, _, _, _, _, _, _) -> [];
calc_authorization_header(AccessKey, SignedHeaders, HttpVerb, CanonicalUri, CanonicalQueryString, AwsRegion, Scope, Date, Timestamp) ->
  Credential = <<AccessKey/binary, "/", Date/binary, "/", AwsRegion/binary, "/", Scope/binary, "/aws4_request">>,
  SignedHeadersValue = calc_signed_headers_value(SignedHeaders),
  Signature = generate_signature(HttpVerb, CanonicalUri, CanonicalQueryString, SignedHeaders, AwsRegion, Scope, Date, Timestamp),
  [{<<"Authorization">>, <<"AWS4-HMAC-SHA256 ",
    "Credential=", Credential/binary, ", ",
    "SignedHeaders=", SignedHeadersValue/binary, ", ",
    "Signature=", Signature/binary>>}].

calc_signed_headers_value(SignedHeaders) ->
  calc_signed_headers_value_(SignedHeaders, <<>>).

calc_signed_headers_value_([{H,_}], <<>>) when is_binary(H) -> 
  <<H/binary>>;
calc_signed_headers_value_([{H,_}|R], Buf) when is_binary(H) ->
  calc_signed_headers_value_(R, <<Buf/binary, ";", H/binary>>);
calc_signed_headers_value_(_, Buf) -> 
  Buf.

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
  <<_:1/binary, CanonicalHeaders/binary>> =
    lists:foldl(fun({N, V}, Acc) ->
      <<Acc/binary, "\n",
      (string:lowercase(N))/binary, ":", (string:trim(V))/binary>>
    end, <<>>, Headers1),
  <<_:1/binary, SignedHeaders/binary>> =
    lists:foldl(fun({N, _V}, Acc) ->
      <<Acc/binary, ";", (string:lowercase(N))/binary>>
    end, <<>>, Headers1),
  CanonicalRequest = <<HttpVerb/binary, "\n",
                      CanonicalUri/binary, "\n",
                      CanonicalQueryString/binary, "\n",
                      CanonicalHeaders/binary, "\n\n",
                      SignedHeaders/binary, "\n",
                      (?PAYLOAD_HASH)/binary>>,

  erlaws3_utils:sha256_to_hex(crypto:hash(sha256, CanonicalRequest)).

%%====================================================================
%% @doc String To Sign
%%====================================================================
generate_string_to_sign(Date, Timestamp, AwsRegion, Scope, CanonicalRequest) ->
  <<"AWS4-HMAC-SHA256\n",
    Timestamp/binary, "\n",
    Date/binary, "/", AwsRegion/binary, "/", Scope/binary, "/aws4_request\n",
    CanonicalRequest/binary>>.

%%====================================================================
%% @doc Signing Key
%%====================================================================
generate_signing_key(Date, AwsRegion, Scope) ->
  SecretKey = application:get_env(erlaws3, secret_key, <<>>),
  DateKey = crypto:hmac(sha256, <<"AWS4", SecretKey/binary>>, Date),
  RegionKey = crypto:hmac(sha256, DateKey, AwsRegion),
  ScopeKey = crypto:hmac(sha256, RegionKey, Scope),
  crypto:hmac(sha256, ScopeKey, <<"aws4_request">>).

-ifdef(TEST).



-include_lib("eunit/include/eunit.hrl").


-endif.
