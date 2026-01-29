%%%-------------------------------------------------------------------
%% @doc GCP IAM Simulator
%%
%% Simulates Google Cloud IAM for testing:
%% - Service account operations
%% - IAM policy management
%% - Permission checking
%% - Role bindings
%%
%% @end
%%%-------------------------------------------------------------------
-module(gcp_iam_sim).
-behaviour(gen_server).

-include("gcp_simulator.hrl").

%% API
-export([
    start_link/0,
    stop/0
]).

%% Service Account Operations
-export([
    create_service_account/3,
    create_service_account/4,
    get_service_account/1,
    list_service_accounts/1,
    delete_service_account/1,
    disable_service_account/1,
    enable_service_account/1
]).

%% IAM Policy Operations
-export([
    get_iam_policy/2,
    set_iam_policy/3,
    add_binding/4,
    remove_binding/3,
    test_iam_permissions/3
]).

%% Permission Checking
-export([
    check_permission/3,
    get_role_permissions/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    service_accounts = #{} :: #{binary() => #gcp_service_account{}},
    policies = #{} :: #{{atom(), binary()} => #gcp_iam_policy{}},  %% {resource_type, resource_id}
    role_permissions = #{} :: #{role() => [permission()]}
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?SERVER).

%% @doc Create a service account.
-spec create_service_account(project_id(), binary(), binary()) ->
    {ok, #gcp_service_account{}} | {error, #gcp_error{}}.
create_service_account(ProjectId, AccountId, DisplayName) ->
    create_service_account(ProjectId, AccountId, DisplayName, #{}).

-spec create_service_account(project_id(), binary(), binary(), map()) ->
    {ok, #gcp_service_account{}} | {error, #gcp_error{}}.
create_service_account(ProjectId, AccountId, DisplayName, Opts) ->
    gen_server:call(?SERVER, {create_service_account, ProjectId, AccountId,
                              DisplayName, Opts}).

%% @doc Get service account.
-spec get_service_account(binary()) ->
    {ok, #gcp_service_account{}} | {error, #gcp_error{}}.
get_service_account(Email) ->
    gen_server:call(?SERVER, {get_service_account, Email}).

%% @doc List service accounts.
-spec list_service_accounts(project_id()) -> {ok, [#gcp_service_account{}]}.
list_service_accounts(ProjectId) ->
    gen_server:call(?SERVER, {list_service_accounts, ProjectId}).

%% @doc Delete service account.
-spec delete_service_account(binary()) -> ok | {error, #gcp_error{}}.
delete_service_account(Email) ->
    gen_server:call(?SERVER, {delete_service_account, Email}).

%% @doc Disable service account.
-spec disable_service_account(binary()) -> ok | {error, #gcp_error{}}.
disable_service_account(Email) ->
    gen_server:call(?SERVER, {disable_service_account, Email}).

%% @doc Enable service account.
-spec enable_service_account(binary()) -> ok | {error, #gcp_error{}}.
enable_service_account(Email) ->
    gen_server:call(?SERVER, {enable_service_account, Email}).

%% @doc Get IAM policy for a resource.
-spec get_iam_policy(atom(), binary()) ->
    {ok, #gcp_iam_policy{}} | {error, #gcp_error{}}.
get_iam_policy(ResourceType, ResourceId) ->
    gen_server:call(?SERVER, {get_iam_policy, ResourceType, ResourceId}).

%% @doc Set IAM policy for a resource.
-spec set_iam_policy(atom(), binary(), #gcp_iam_policy{}) ->
    {ok, #gcp_iam_policy{}} | {error, #gcp_error{}}.
set_iam_policy(ResourceType, ResourceId, Policy) ->
    gen_server:call(?SERVER, {set_iam_policy, ResourceType, ResourceId, Policy}).

%% @doc Add an IAM binding.
-spec add_binding(atom(), binary(), role(), [principal()]) ->
    {ok, #gcp_iam_policy{}} | {error, #gcp_error{}}.
add_binding(ResourceType, ResourceId, Role, Members) ->
    gen_server:call(?SERVER, {add_binding, ResourceType, ResourceId, Role, Members}).

%% @doc Remove an IAM binding.
-spec remove_binding(atom(), binary(), role()) ->
    {ok, #gcp_iam_policy{}} | {error, #gcp_error{}}.
remove_binding(ResourceType, ResourceId, Role) ->
    gen_server:call(?SERVER, {remove_binding, ResourceType, ResourceId, Role}).

%% @doc Test IAM permissions.
-spec test_iam_permissions(atom(), binary(), [permission()]) ->
    {ok, [permission()]}.
test_iam_permissions(ResourceType, ResourceId, Permissions) ->
    gen_server:call(?SERVER, {test_iam_permissions, ResourceType, ResourceId,
                              Permissions}).

%% @doc Check if a principal has a permission.
-spec check_permission(principal(), atom(), binary()) -> boolean().
check_permission(Principal, ResourceType, ResourceId) ->
    gen_server:call(?SERVER, {check_permission, Principal, ResourceType, ResourceId}).

%% @doc Get permissions for a role.
-spec get_role_permissions(role()) -> {ok, [permission()]} | {error, not_found}.
get_role_permissions(Role) ->
    gen_server:call(?SERVER, {get_role_permissions, Role}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Initialize with default role permissions
    DefaultRoles = default_role_permissions(),
    {ok, #state{role_permissions = DefaultRoles}}.

handle_call({create_service_account, ProjectId, AccountId, DisplayName, Opts},
            _From, State) ->
    Email = <<AccountId/binary, "@", ProjectId/binary, ".iam.gserviceaccount.com">>,
    case maps:is_key(Email, State#state.service_accounts) of
        true ->
            Error = #gcp_error{
                code = ?GCP_CONFLICT,
                message = <<"Service account already exists">>,
                status = <<"ALREADY_EXISTS">>
            },
            {reply, {error, Error}, State};
        false ->
            Now = erlang:system_time(millisecond),
            SA = #gcp_service_account{
                email = Email,
                project_id = ProjectId,
                unique_id = generate_unique_id(),
                display_name = DisplayName,
                description = maps:get(description, Opts, <<>>),
                disabled = false,
                created_at = Now
            },
            NewSAs = maps:put(Email, SA, State#state.service_accounts),
            {reply, {ok, SA}, State#state{service_accounts = NewSAs}}
    end;

handle_call({get_service_account, Email}, _From, State) ->
    case maps:get(Email, State#state.service_accounts, undefined) of
        undefined ->
            {reply, {error, not_found_error(<<"Service account">>)}, State};
        SA ->
            {reply, {ok, SA}, State}
    end;

handle_call({list_service_accounts, ProjectId}, _From, State) ->
    SAs = maps:fold(
        fun(_Email, SA, Acc) ->
            case SA#gcp_service_account.project_id of
                ProjectId -> [SA | Acc];
                _ -> Acc
            end
        end,
        [],
        State#state.service_accounts
    ),
    {reply, {ok, SAs}, State};

handle_call({delete_service_account, Email}, _From, State) ->
    case maps:get(Email, State#state.service_accounts, undefined) of
        undefined ->
            {reply, {error, not_found_error(<<"Service account">>)}, State};
        _SA ->
            NewSAs = maps:remove(Email, State#state.service_accounts),
            {reply, ok, State#state{service_accounts = NewSAs}}
    end;

handle_call({disable_service_account, Email}, _From, State) ->
    case maps:get(Email, State#state.service_accounts, undefined) of
        undefined ->
            {reply, {error, not_found_error(<<"Service account">>)}, State};
        SA ->
            UpdatedSA = SA#gcp_service_account{disabled = true},
            NewSAs = maps:put(Email, UpdatedSA, State#state.service_accounts),
            {reply, ok, State#state{service_accounts = NewSAs}}
    end;

handle_call({enable_service_account, Email}, _From, State) ->
    case maps:get(Email, State#state.service_accounts, undefined) of
        undefined ->
            {reply, {error, not_found_error(<<"Service account">>)}, State};
        SA ->
            UpdatedSA = SA#gcp_service_account{disabled = false},
            NewSAs = maps:put(Email, UpdatedSA, State#state.service_accounts),
            {reply, ok, State#state{service_accounts = NewSAs}}
    end;

handle_call({get_iam_policy, ResourceType, ResourceId}, _From, State) ->
    Key = {ResourceType, ResourceId},
    Policy = maps:get(Key, State#state.policies, #gcp_iam_policy{
        version = 1,
        bindings = [],
        etag = generate_etag()
    }),
    {reply, {ok, Policy}, State};

handle_call({set_iam_policy, ResourceType, ResourceId, Policy}, _From, State) ->
    Key = {ResourceType, ResourceId},
    UpdatedPolicy = Policy#gcp_iam_policy{etag = generate_etag()},
    NewPolicies = maps:put(Key, UpdatedPolicy, State#state.policies),
    {reply, {ok, UpdatedPolicy}, State#state{policies = NewPolicies}};

handle_call({add_binding, ResourceType, ResourceId, Role, Members}, _From, State) ->
    Key = {ResourceType, ResourceId},
    Policy = maps:get(Key, State#state.policies, #gcp_iam_policy{
        version = 1,
        bindings = [],
        etag = generate_etag()
    }),
    NewBinding = #gcp_iam_binding{role = Role, members = Members},
    %% Check if binding for this role exists
    UpdatedBindings = case lists:keyfind(Role, #gcp_iam_binding.role, Policy#gcp_iam_policy.bindings) of
        false ->
            [NewBinding | Policy#gcp_iam_policy.bindings];
        ExistingBinding ->
            %% Merge members
            MergedMembers = lists:usort(ExistingBinding#gcp_iam_binding.members ++ Members),
            UpdatedBinding = ExistingBinding#gcp_iam_binding{members = MergedMembers},
            lists:keyreplace(Role, #gcp_iam_binding.role, Policy#gcp_iam_policy.bindings, UpdatedBinding)
    end,
    UpdatedPolicy = Policy#gcp_iam_policy{
        bindings = UpdatedBindings,
        etag = generate_etag()
    },
    NewPolicies = maps:put(Key, UpdatedPolicy, State#state.policies),
    {reply, {ok, UpdatedPolicy}, State#state{policies = NewPolicies}};

handle_call({remove_binding, ResourceType, ResourceId, Role}, _From, State) ->
    Key = {ResourceType, ResourceId},
    case maps:get(Key, State#state.policies, undefined) of
        undefined ->
            {reply, {error, not_found_error(<<"IAM policy">>)}, State};
        Policy ->
            UpdatedBindings = lists:keydelete(Role, #gcp_iam_binding.role,
                                               Policy#gcp_iam_policy.bindings),
            UpdatedPolicy = Policy#gcp_iam_policy{
                bindings = UpdatedBindings,
                etag = generate_etag()
            },
            NewPolicies = maps:put(Key, UpdatedPolicy, State#state.policies),
            {reply, {ok, UpdatedPolicy}, State#state{policies = NewPolicies}}
    end;

handle_call({test_iam_permissions, ResourceType, ResourceId, Permissions}, _From, State) ->
    Key = {ResourceType, ResourceId},
    Policy = maps:get(Key, State#state.policies, #gcp_iam_policy{bindings = []}),

    %% Get all permissions granted by the policy
    GrantedPermissions = lists:foldl(
        fun(Binding, Acc) ->
            RolePerms = maps:get(Binding#gcp_iam_binding.role,
                                 State#state.role_permissions, []),
            lists:usort(Acc ++ RolePerms)
        end,
        [],
        Policy#gcp_iam_policy.bindings
    ),

    %% Filter requested permissions to those granted
    AllowedPermissions = [P || P <- Permissions, lists:member(P, GrantedPermissions)],
    {reply, {ok, AllowedPermissions}, State};

handle_call({check_permission, Principal, ResourceType, ResourceId}, _From, State) ->
    Key = {ResourceType, ResourceId},
    Policy = maps:get(Key, State#state.policies, #gcp_iam_policy{bindings = []}),

    %% Check if principal is in any binding
    HasPermission = lists:any(
        fun(Binding) ->
            lists:member(Principal, Binding#gcp_iam_binding.members)
        end,
        Policy#gcp_iam_policy.bindings
    ),
    {reply, HasPermission, State};

handle_call({get_role_permissions, Role}, _From, State) ->
    case maps:get(Role, State#state.role_permissions, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Permissions ->
            {reply, {ok, Permissions}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

generate_unique_id() ->
    integer_to_binary(erlang:unique_integer([positive])).

generate_etag() ->
    base64:encode(crypto:strong_rand_bytes(12)).

not_found_error(Resource) ->
    #gcp_error{
        code = ?GCP_NOT_FOUND,
        message = <<Resource/binary, " not found">>,
        status = <<"NOT_FOUND">>
    }.

%% @doc Default role permissions for common GCP roles.
default_role_permissions() ->
    #{
        %% Storage roles
        <<"roles/storage.admin">> => [
            <<"storage.buckets.create">>,
            <<"storage.buckets.delete">>,
            <<"storage.buckets.get">>,
            <<"storage.buckets.list">>,
            <<"storage.buckets.update">>,
            <<"storage.objects.create">>,
            <<"storage.objects.delete">>,
            <<"storage.objects.get">>,
            <<"storage.objects.list">>,
            <<"storage.objects.update">>
        ],
        <<"roles/storage.objectViewer">> => [
            <<"storage.objects.get">>,
            <<"storage.objects.list">>
        ],
        <<"roles/storage.objectCreator">> => [
            <<"storage.objects.create">>
        ],

        %% Pub/Sub roles
        <<"roles/pubsub.admin">> => [
            <<"pubsub.topics.create">>,
            <<"pubsub.topics.delete">>,
            <<"pubsub.topics.get">>,
            <<"pubsub.topics.list">>,
            <<"pubsub.topics.publish">>,
            <<"pubsub.subscriptions.create">>,
            <<"pubsub.subscriptions.delete">>,
            <<"pubsub.subscriptions.get">>,
            <<"pubsub.subscriptions.list">>,
            <<"pubsub.subscriptions.consume">>
        ],
        <<"roles/pubsub.publisher">> => [
            <<"pubsub.topics.publish">>
        ],
        <<"roles/pubsub.subscriber">> => [
            <<"pubsub.subscriptions.consume">>,
            <<"pubsub.subscriptions.get">>
        ],

        %% Compute roles
        <<"roles/compute.admin">> => [
            <<"compute.instances.create">>,
            <<"compute.instances.delete">>,
            <<"compute.instances.get">>,
            <<"compute.instances.list">>,
            <<"compute.instances.start">>,
            <<"compute.instances.stop">>,
            <<"compute.instances.reset">>
        ],
        <<"roles/compute.viewer">> => [
            <<"compute.instances.get">>,
            <<"compute.instances.list">>
        ],

        %% Owner/Editor/Viewer
        <<"roles/owner">> => [
            <<"*">>  %% All permissions
        ],
        <<"roles/editor">> => [
            <<"*.create">>,
            <<"*.delete">>,
            <<"*.get">>,
            <<"*.list">>,
            <<"*.update">>
        ],
        <<"roles/viewer">> => [
            <<"*.get">>,
            <<"*.list">>
        ]
    }.
