%% @doc Enterprise Identity Integration Example
%% Demonstrates integration with various identity providers
-module(identity_integration_example).

-export([main/0, setup_identity_providers/0, test_authentication/1,
         test_authorization/2, test_user_management/2]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Main Functions
%%====================================================================

-spec main() -> ok.
main() ->
    %% Start the enterprise integrations
    ok = start_integrations(),

    %% Setup identity providers
    ok = setup_identity_providers(),

    %% Test authentication
    case test_authentication(okta) of
        {ok, UserInfo} ->
            ?LOG_INFO("Authentication successful: ~p", [UserInfo]),

            %% Test authorization
            case test_authorization(UserInfo#{<<"id">> => <<"user123">>}, <<"resource1">>) of
                {ok, true} ->
                    ?LOG_INFO("Authorization successful"),

                    %% Test user management
                    test_user_management(okta, #{
                        <<"id">> => <<"newuser123">>,
                        <<"username">> => <<"testuser">>,
                        <<"email">> => <<"test@example.com">>,
                        <<"groups">> => [<<"developers">>, <<"users">>]
                    });
                {ok, false} ->
                    ?LOG_WARNING("Authorization failed");
                {error, Reason} ->
                    ?LOG_ERROR("Authorization error: ~p", [Reason])
            end;
        {error, Reason} ->
            ?LOG_ERROR("Authentication failed: ~p", [Reason])
    end.

-spec start_integrations() -> ok.
start_integrations() ->
    %% Start the enterprise integrations application
    case application:ensure_all_started(erlmcp_enterprise_integrations) of
        {ok, _} -> ok;
        {error, Reason} -> throw({failed_to_start_integrations, Reason})
    end.

-spec setup_identity_providers() -> ok.
setup_identity_providers() ->
    %% Configure and start identity provider adapters
    %% Okta Configuration
    okta_config = #{
        provider => okta,
        endpoint => "https://your-company.okta.com",
        api_key => <<"your-api-key">>,
        timeout => 30000
    },

    case erlmcp_identity_adapter:configure(okta, okta_config) of
        ok -> ?LOG_INFO("Okta adapter configured");
        {error, Reason} -> ?LOG_ERROR("Failed to configure Okta: ~p", [Reason])
    end,

    %% Azure AD Configuration
    azure_config = #{
        provider => azure,
        endpoint => "https://login.microsoftonline.com/your-tenant-id/oauth2/v2.0",
        client_id => <<"your-client-id">>,
        client_secret => <<"your-client-secret">>,
        timeout => 30000
    },

    case erlmcp_identity_adapter:configure(azure, azure_config) of
        ok -> ?LOG_INFO("Azure AD adapter configured");
        {error, Reason} -> ?LOG_ERROR("Failed to configure Azure AD: ~p", [Reason])
    end,

    %% ADFS Configuration
    adfs_config = #{
        provider => adfs,
        endpoint => "https://your-adfs/adfs",
        username => <<"your-username">>,
        password => <<"your-password">>,
        timeout => 30000
    },

    case erlmcp_identity_adapter:configure(adfs, adfs_config) of
        ok -> ?LOG_INFO("ADFS adapter configured");
        {error, Reason} -> ?LOG_ERROR("Failed to configure ADFS: ~p", [Reason])
    end,

    ok.

-spec test_authentication(provider()) -> {ok, map()} | {error, term()}.
test_authentication(Provider) ->
    %% Test user authentication with the specified provider
    Credentials = #{
        username => <<"testuser">>,
        password => <<"testpass">>
    },

    Context = #{
        client_id => <<"erlmcp-client">>,
        redirect_uri => <<"https://erlmcp.example.com/callback">>,
        state => generate_uuid()
    },

    case erlmcp_identity_adapter:authenticate(Credentials, Context) of
        {ok, UserInfo} ->
            %% Store user session
            case erlmcp_session_manager:create_session(UserInfo) of
                {ok, SessionId} ->
                    %% Store session ID in user info
                    UpdatedUserInfo = UserInfo#{<<"session_id">> => SessionId},
                    {ok, UpdatedUserInfo};
                {error, SessionReason} ->
                    {error, {session_creation_failed, SessionReason}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec test_authorization(map(), binary()) -> {ok, boolean()} | {error, term()}.
test_authorization(UserInfo, Resource) ->
    %% Test user authorization for a specific resource
    UserId = maps:get(<<"id">>, UserInfo),

    Context = #{
        required_permissions => [<<"read">>, <<"write">>],
        resource => Resource,
        app_id => <<"erlmcp">>
    },

    case erlmcp_identity_adapter:authorize(UserId, Resource, Context) of
        {ok, Authorized} ->
            case Authorized of
                true ->
                    %% Log authorization success
                    erlmcp_audit_logger:log(
                        <<"authorization">>,
                        #{user_id => UserId, resource => Resource, action => <<"authorized">>}
                    ),
                    {ok, true};
                false ->
                    %% Log authorization failure
                    erlmcp_audit_logger:log(
                        <<"authorization">>,
                        #{user_id => UserId, resource => Resource, action => <<"denied">>}
                    ),
                    {ok, false}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec test_user_management(provider(), map()) -> ok | {error, term()}.
test_user_management(Provider, UserInfo) ->
    %% Test user provisioning and deprovisioning
    Username = maps:get(<<"username">>, UserInfo),

    %% Provision the user
    case erlmcp_identity_adapter:provision_user(UserInfo, #{}) of
        {ok, UserId} ->
            ?LOG_INFO("User provisioned: ~p", [UserId]),

            %% Get user information
            case erlmcp_identity_adapter:get_user_info(UserId) of
                {ok, RetrievedInfo} ->
                    ?LOG_INFO("Retrieved user info: ~p", [RetrievedInfo]),

                    %% Add user to groups
                    Groups = maps:get(<<"groups">>, UserInfo),
                    lists:foreach(fun(Group) ->
                        case erlmcp_identity_adapter:add_user_to_group(UserId, Group) of
                            ok -> ?LOG_INFO("User added to group: ~p", [Group]);
                            {error, Reason} -> ?LOG_ERROR("Failed to add to group: ~p", [Reason])
                        end
                    end, Groups),

                    Deprovision = proplists:get_value(deprovision, UserInfo, true),
                    if Deprovision ->
                        %% Deprovision the user
                        case erlmcp_identity_adapter:deprovision_user(UserId) of
                            ok ->
                                ?LOG_INFO("User deprovisioned: ~p", [UserId]);
                            {error, Reason} ->
                                ?LOG_ERROR("Failed to deprovision user: ~p", [Reason])
                        end;
                    true ->
                        ok
                    end;
                {error, Reason} ->
                    {error, {failed_to_get_user_info, Reason}}
            end;
        {error, Reason} ->
            {error, {failed_to_provision_user, Reason}}
    end.

%%====================================================================
%% Helper Functions
%%====================================================================

generate_uuid() ->
    %% Generate a unique identifier for the request
    binary_to_list(erlang:ref_to_list(make_ref())).

%%====================================================================
%% Utility Functions
%%====================================================================

%% Example of adding a user to a group
add_user_to_group(UserId, Group) ->
    case erlmcp_identity_adapter:configure(okta, #{}) of
        ok ->
            %% This would be implemented in the identity adapter
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% Example of handling SSO flow
handle_sso_flow(Provider, AuthCode) ->
    %% Exchange authorization code for tokens
    TokenEndpoint = get_token_endpoint(Provider),
    ClientCredentials = get_client_credentials(Provider),

    TokenRequest = #{
        grant_type => <<"authorization_code">>,
        code => AuthCode,
        redirect_uri => <<"https://erlmcp.example.com/callback">>,
        client_id => maps:get(client_id, ClientCredentials),
        client_secret => maps:get(client_secret, ClientCredentials)
    },

    %% Token exchange would happen here
    case httpc:request(post, {TokenEndpoint, [], "application/x-www-form-urlencoded",
                              uri_string:compose_query(maps:to_list(TokenRequest))},
                      [{timeout, 30000}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            %% Parse and store tokens
            {ok, ResponseBody};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {token_exchange_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

get_token_endpoint(okta) ->
    "https://your-company.okta.com/oauth2/default/v1/token";
get_token_endpoint(azure) ->
    "https://login.microsoftonline.com/your-tenant-id/oauth2/v2.0/token";
get_token_endpoint(adfs) ->
    "https://your-adfs/adfs/oauth2/token".

get_client_credentials(okta) ->
    #{
        client_id => <<"okta-client-id">>,
        client_secret => <<"okta-client-secret">>
    };
get_client_credentials(azure) ->
    #{
        client_id => <<"azure-client-id">>,
        client_secret => <<"azure-client-secret">>
    };
get_client_credentials(adfs) ->
    #{
        client_id => <<"adfs-client-id">>,
        client_secret => <<"adfs-client-secret">>
    }.