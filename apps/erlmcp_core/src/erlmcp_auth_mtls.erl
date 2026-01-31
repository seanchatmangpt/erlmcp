%%%-------------------------------------------------------------------
%%% @doc erlmcp_auth_mtls - Simplified mTLS stub for CT tests
%%%
%%% This is a stub implementation to allow compilation to proceed
%%% while CT suite initialization issues are being fixed.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_auth_mtls).

%% API exports
-export([
    validate/2,
    extract_certificate_from_socket/1,
    check_revocation/3
]).

%% Types
-type cert_der() :: binary().
-type cert_chain() :: [cert_der()].
-type mtls_config() :: map().
-type cert_info() :: map().
-type validation_result() :: {ok, binary()} | {error, term()}.

-export_type([cert_der/0, cert_chain/0, mtls_config/0, cert_info/0, validation_result/0]).

%% @doc Validate mTLS certificate (stub)
validate(_CertInfo, _Config) ->
    {error, mtls_stub_not_fully_implemented}.

%% @doc Extract certificate from socket (stub)
extract_certificate_from_socket(_Socket) ->
    {error, mtls_stub_not_fully_implemented}.

%% @doc Check revocation (stub)
check_revocation(_CertDer, _CertChain, _Config) ->
    {error, mtls_stub_not_fully_implemented}.

%% @doc Extract certificate DER (stub)
extract_certificate_der(_CertInfo, _Config) ->
    {error, mtls_stub_not_fully_implemented}.

%% @doc Validate certificate (stub)
validate_certificate(_CertDer, _ValidatorConfig) ->
    {error, mtls_stub_not_fully_implemented}.
