%%%-------------------------------------------------------------------
%% @doc GCP Compute Engine Simulator
%%
%% Simulates Google Compute Engine for testing:
%% - Instance operations (create, get, list, delete, start, stop)
%% - Machine types
%% - Network interfaces
%% - Instance lifecycle simulation
%%
%% @end
%%%-------------------------------------------------------------------
-module(gcp_compute_sim).
-behaviour(gen_server).

-include("gcp_simulator.hrl").

%% API
-export([
    start_link/0,
    stop/0
]).

%% Instance Operations
-export([
    create_instance/4,
    create_instance/5,
    get_instance/3,
    list_instances/2,
    list_instances/1,
    delete_instance/3,
    start_instance/3,
    stop_instance/3,
    reset_instance/3
]).

%% Instance Metadata
-export([
    set_metadata/4,
    set_labels/4,
    set_tags/4
]).

%% Machine Types
-export([
    list_machine_types/2,
    get_machine_type/3
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    instances = #{} :: #{{project_id(), zone(), binary()} => #gcp_instance{}},
    pending_operations = #{} :: #{binary() => map()}
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?SERVER).

%% @doc Create a new instance.
-spec create_instance(project_id(), zone(), binary(), machine_type()) ->
    {ok, #gcp_instance{}} | {error, #gcp_error{}}.
create_instance(ProjectId, Zone, Name, MachineType) ->
    create_instance(ProjectId, Zone, Name, MachineType, #{}).

-spec create_instance(project_id(), zone(), binary(), machine_type(), map()) ->
    {ok, #gcp_instance{}} | {error, #gcp_error{}}.
create_instance(ProjectId, Zone, Name, MachineType, Opts) ->
    gen_server:call(?SERVER, {create_instance, ProjectId, Zone, Name,
                              MachineType, Opts}).

%% @doc Get instance details.
-spec get_instance(project_id(), zone(), binary()) ->
    {ok, #gcp_instance{}} | {error, #gcp_error{}}.
get_instance(ProjectId, Zone, Name) ->
    gen_server:call(?SERVER, {get_instance, ProjectId, Zone, Name}).

%% @doc List instances in a zone.
-spec list_instances(project_id(), zone()) -> {ok, [#gcp_instance{}]}.
list_instances(ProjectId, Zone) ->
    gen_server:call(?SERVER, {list_instances, ProjectId, Zone}).

%% @doc List all instances in a project.
-spec list_instances(project_id()) -> {ok, [#gcp_instance{}]}.
list_instances(ProjectId) ->
    gen_server:call(?SERVER, {list_all_instances, ProjectId}).

%% @doc Delete an instance.
-spec delete_instance(project_id(), zone(), binary()) -> ok | {error, #gcp_error{}}.
delete_instance(ProjectId, Zone, Name) ->
    gen_server:call(?SERVER, {delete_instance, ProjectId, Zone, Name}).

%% @doc Start an instance.
-spec start_instance(project_id(), zone(), binary()) ->
    {ok, #gcp_instance{}} | {error, #gcp_error{}}.
start_instance(ProjectId, Zone, Name) ->
    gen_server:call(?SERVER, {start_instance, ProjectId, Zone, Name}).

%% @doc Stop an instance.
-spec stop_instance(project_id(), zone(), binary()) ->
    {ok, #gcp_instance{}} | {error, #gcp_error{}}.
stop_instance(ProjectId, Zone, Name) ->
    gen_server:call(?SERVER, {stop_instance, ProjectId, Zone, Name}).

%% @doc Reset an instance.
-spec reset_instance(project_id(), zone(), binary()) ->
    {ok, #gcp_instance{}} | {error, #gcp_error{}}.
reset_instance(ProjectId, Zone, Name) ->
    gen_server:call(?SERVER, {reset_instance, ProjectId, Zone, Name}).

%% @doc Set instance metadata.
-spec set_metadata(project_id(), zone(), binary(), map()) ->
    {ok, #gcp_instance{}} | {error, #gcp_error{}}.
set_metadata(ProjectId, Zone, Name, Metadata) ->
    gen_server:call(?SERVER, {set_metadata, ProjectId, Zone, Name, Metadata}).

%% @doc Set instance labels.
-spec set_labels(project_id(), zone(), binary(), map()) ->
    {ok, #gcp_instance{}} | {error, #gcp_error{}}.
set_labels(ProjectId, Zone, Name, Labels) ->
    gen_server:call(?SERVER, {set_labels, ProjectId, Zone, Name, Labels}).

%% @doc Set instance tags.
-spec set_tags(project_id(), zone(), binary(), [binary()]) ->
    {ok, #gcp_instance{}} | {error, #gcp_error{}}.
set_tags(ProjectId, Zone, Name, Tags) ->
    gen_server:call(?SERVER, {set_tags, ProjectId, Zone, Name, Tags}).

%% @doc List machine types in a zone.
-spec list_machine_types(project_id(), zone()) -> {ok, [map()]}.
list_machine_types(_ProjectId, Zone) ->
    gen_server:call(?SERVER, {list_machine_types, Zone}).

%% @doc Get machine type details.
-spec get_machine_type(project_id(), zone(), binary()) ->
    {ok, map()} | {error, #gcp_error{}}.
get_machine_type(_ProjectId, Zone, MachineType) ->
    gen_server:call(?SERVER, {get_machine_type, Zone, MachineType}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Start timer for simulating instance state transitions
    erlang:send_after(1000, self(), check_pending_operations),
    {ok, #state{}}.

handle_call({create_instance, ProjectId, Zone, Name, MachineType, Opts},
            _From, State) ->
    Key = {ProjectId, Zone, Name},
    case maps:is_key(Key, State#state.instances) of
        true ->
            Error = #gcp_error{
                code = ?GCP_CONFLICT,
                message = <<"Instance already exists">>,
                status = <<"ALREADY_EXISTS">>
            },
            {reply, {error, Error}, State};
        false ->
            Now = erlang:system_time(millisecond),

            %% Create default network interface
            DefaultNetwork = #gcp_network_interface{
                network = <<"default">>,
                network_ip = generate_internal_ip()
            },

            Instance = #gcp_instance{
                name = Name,
                project_id = ProjectId,
                zone = Zone,
                machine_type = MachineType,
                status = provisioning,
                network_interfaces = [DefaultNetwork],
                labels = maps:get(labels, Opts, #{}),
                metadata = maps:get(metadata, Opts, #{}),
                tags = maps:get(tags, Opts, []),
                service_accounts = maps:get(service_accounts, Opts, []),
                created_at = Now
            },

            NewInstances = maps:put(Key, Instance, State#state.instances),

            %% Schedule transition to running
            OpId = generate_operation_id(),
            Op = #{
                type => create,
                key => Key,
                target_status => running,
                complete_at => Now + 5000  %% 5 seconds
            },
            NewOps = maps:put(OpId, Op, State#state.pending_operations),

            {reply, {ok, Instance}, State#state{
                instances = NewInstances,
                pending_operations = NewOps
            }}
    end;

handle_call({get_instance, ProjectId, Zone, Name}, _From, State) ->
    Key = {ProjectId, Zone, Name},
    case maps:get(Key, State#state.instances, undefined) of
        undefined ->
            {reply, {error, not_found_error(<<"Instance">>)}, State};
        Instance ->
            {reply, {ok, Instance}, State}
    end;

handle_call({list_instances, ProjectId, Zone}, _From, State) ->
    Instances = maps:fold(
        fun({P, Z, _N}, Instance, Acc) ->
            case P =:= ProjectId andalso Z =:= Zone of
                true -> [Instance | Acc];
                false -> Acc
            end
        end,
        [],
        State#state.instances
    ),
    {reply, {ok, Instances}, State};

handle_call({list_all_instances, ProjectId}, _From, State) ->
    Instances = maps:fold(
        fun({P, _Z, _N}, Instance, Acc) ->
            case P =:= ProjectId of
                true -> [Instance | Acc];
                false -> Acc
            end
        end,
        [],
        State#state.instances
    ),
    {reply, {ok, Instances}, State};

handle_call({delete_instance, ProjectId, Zone, Name}, _From, State) ->
    Key = {ProjectId, Zone, Name},
    case maps:get(Key, State#state.instances, undefined) of
        undefined ->
            {reply, {error, not_found_error(<<"Instance">>)}, State};
        Instance ->
            %% Mark as terminated
            UpdatedInstance = Instance#gcp_instance{status = terminated},
            NewInstances = maps:put(Key, UpdatedInstance, State#state.instances),

            %% Schedule actual removal
            Now = erlang:system_time(millisecond),
            OpId = generate_operation_id(),
            Op = #{
                type => delete,
                key => Key,
                complete_at => Now + 2000
            },
            NewOps = maps:put(OpId, Op, State#state.pending_operations),

            {reply, ok, State#state{
                instances = NewInstances,
                pending_operations = NewOps
            }}
    end;

handle_call({start_instance, ProjectId, Zone, Name}, _From, State) ->
    Key = {ProjectId, Zone, Name},
    case maps:get(Key, State#state.instances, undefined) of
        undefined ->
            {reply, {error, not_found_error(<<"Instance">>)}, State};
        Instance ->
            case Instance#gcp_instance.status of
                stopped ->
                    Now = erlang:system_time(millisecond),
                    UpdatedInstance = Instance#gcp_instance{
                        status = staging,
                        last_start_time = Now
                    },
                    NewInstances = maps:put(Key, UpdatedInstance, State#state.instances),

                    %% Schedule transition to running
                    OpId = generate_operation_id(),
                    Op = #{
                        type => start,
                        key => Key,
                        target_status => running,
                        complete_at => Now + 3000
                    },
                    NewOps = maps:put(OpId, Op, State#state.pending_operations),

                    {reply, {ok, UpdatedInstance}, State#state{
                        instances = NewInstances,
                        pending_operations = NewOps
                    }};
                running ->
                    {reply, {ok, Instance}, State};
                Status ->
                    Error = #gcp_error{
                        code = ?GCP_BAD_REQUEST,
                        message = <<"Cannot start instance in status: ",
                                    (atom_to_binary(Status))/binary>>,
                        status = <<"INVALID_STATE">>
                    },
                    {reply, {error, Error}, State}
            end
    end;

handle_call({stop_instance, ProjectId, Zone, Name}, _From, State) ->
    Key = {ProjectId, Zone, Name},
    case maps:get(Key, State#state.instances, undefined) of
        undefined ->
            {reply, {error, not_found_error(<<"Instance">>)}, State};
        Instance ->
            case Instance#gcp_instance.status of
                running ->
                    Now = erlang:system_time(millisecond),
                    UpdatedInstance = Instance#gcp_instance{
                        status = stopping,
                        last_stop_time = Now
                    },
                    NewInstances = maps:put(Key, UpdatedInstance, State#state.instances),

                    %% Schedule transition to stopped
                    OpId = generate_operation_id(),
                    Op = #{
                        type => stop,
                        key => Key,
                        target_status => stopped,
                        complete_at => Now + 3000
                    },
                    NewOps = maps:put(OpId, Op, State#state.pending_operations),

                    {reply, {ok, UpdatedInstance}, State#state{
                        instances = NewInstances,
                        pending_operations = NewOps
                    }};
                stopped ->
                    {reply, {ok, Instance}, State};
                Status ->
                    Error = #gcp_error{
                        code = ?GCP_BAD_REQUEST,
                        message = <<"Cannot stop instance in status: ",
                                    (atom_to_binary(Status))/binary>>,
                        status = <<"INVALID_STATE">>
                    },
                    {reply, {error, Error}, State}
            end
    end;

handle_call({reset_instance, ProjectId, Zone, Name}, _From, State) ->
    Key = {ProjectId, Zone, Name},
    case maps:get(Key, State#state.instances, undefined) of
        undefined ->
            {reply, {error, not_found_error(<<"Instance">>)}, State};
        Instance ->
            case Instance#gcp_instance.status of
                running ->
                    Now = erlang:system_time(millisecond),
                    UpdatedInstance = Instance#gcp_instance{
                        status = staging,
                        last_start_time = Now
                    },
                    NewInstances = maps:put(Key, UpdatedInstance, State#state.instances),

                    %% Schedule transition back to running
                    OpId = generate_operation_id(),
                    Op = #{
                        type => reset,
                        key => Key,
                        target_status => running,
                        complete_at => Now + 2000
                    },
                    NewOps = maps:put(OpId, Op, State#state.pending_operations),

                    {reply, {ok, UpdatedInstance}, State#state{
                        instances = NewInstances,
                        pending_operations = NewOps
                    }};
                Status ->
                    Error = #gcp_error{
                        code = ?GCP_BAD_REQUEST,
                        message = <<"Cannot reset instance in status: ",
                                    (atom_to_binary(Status))/binary>>,
                        status = <<"INVALID_STATE">>
                    },
                    {reply, {error, Error}, State}
            end
    end;

handle_call({set_metadata, ProjectId, Zone, Name, Metadata}, _From, State) ->
    update_instance_field(ProjectId, Zone, Name, metadata, Metadata, State);

handle_call({set_labels, ProjectId, Zone, Name, Labels}, _From, State) ->
    update_instance_field(ProjectId, Zone, Name, labels, Labels, State);

handle_call({set_tags, ProjectId, Zone, Name, Tags}, _From, State) ->
    update_instance_field(ProjectId, Zone, Name, tags, Tags, State);

handle_call({list_machine_types, Zone}, _From, State) ->
    MachineTypes = default_machine_types(Zone),
    {reply, {ok, MachineTypes}, State};

handle_call({get_machine_type, Zone, MachineType}, _From, State) ->
    MachineTypes = default_machine_types(Zone),
    case lists:keyfind(MachineType, 1, [{M, M} || M <- MachineTypes]) of
        false ->
            {reply, {error, not_found_error(<<"Machine type">>)}, State};
        {_, MT} ->
            {reply, {ok, MT}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check_pending_operations, State) ->
    Now = erlang:system_time(millisecond),

    {CompletedOps, RemainingOps} = maps:fold(
        fun(OpId, Op, {Completed, Remaining}) ->
            case maps:get(complete_at, Op) =< Now of
                true -> {[{OpId, Op} | Completed], Remaining};
                false -> {Completed, maps:put(OpId, Op, Remaining)}
            end
        end,
        {[], #{}},
        State#state.pending_operations
    ),

    NewInstances = lists:foldl(
        fun({_OpId, Op}, Instances) ->
            Key = maps:get(key, Op),
            case maps:get(type, Op) of
                delete ->
                    maps:remove(Key, Instances);
                _ ->
                    case maps:get(Key, Instances, undefined) of
                        undefined ->
                            Instances;
                        Instance ->
                            TargetStatus = maps:get(target_status, Op),
                            maps:put(Key, Instance#gcp_instance{status = TargetStatus},
                                     Instances)
                    end
            end
        end,
        State#state.instances,
        CompletedOps
    ),

    erlang:send_after(1000, self(), check_pending_operations),
    {noreply, State#state{
        instances = NewInstances,
        pending_operations = RemainingOps
    }};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

generate_operation_id() ->
    base64:encode(crypto:strong_rand_bytes(12)).

generate_internal_ip() ->
    A = rand:uniform(254),
    B = rand:uniform(254),
    iolist_to_binary(io_lib:format("10.128.~B.~B", [A, B])).

not_found_error(Resource) ->
    #gcp_error{
        code = ?GCP_NOT_FOUND,
        message = <<Resource/binary, " not found">>,
        status = <<"NOT_FOUND">>
    }.

update_instance_field(ProjectId, Zone, Name, Field, Value, State) ->
    Key = {ProjectId, Zone, Name},
    case maps:get(Key, State#state.instances, undefined) of
        undefined ->
            {reply, {error, not_found_error(<<"Instance">>)}, State};
        Instance ->
            UpdatedInstance = case Field of
                metadata -> Instance#gcp_instance{metadata = Value};
                labels -> Instance#gcp_instance{labels = Value};
                tags -> Instance#gcp_instance{tags = Value}
            end,
            NewInstances = maps:put(Key, UpdatedInstance, State#state.instances),
            {reply, {ok, UpdatedInstance}, State#state{instances = NewInstances}}
    end.

default_machine_types(_Zone) ->
    [
        #{name => <<"e2-micro">>, vcpus => 2, memory_mb => 1024},
        #{name => <<"e2-small">>, vcpus => 2, memory_mb => 2048},
        #{name => <<"e2-medium">>, vcpus => 2, memory_mb => 4096},
        #{name => <<"e2-standard-2">>, vcpus => 2, memory_mb => 8192},
        #{name => <<"e2-standard-4">>, vcpus => 4, memory_mb => 16384},
        #{name => <<"e2-standard-8">>, vcpus => 8, memory_mb => 32768},
        #{name => <<"n1-standard-1">>, vcpus => 1, memory_mb => 3840},
        #{name => <<"n1-standard-2">>, vcpus => 2, memory_mb => 7680},
        #{name => <<"n1-standard-4">>, vcpus => 4, memory_mb => 15360},
        #{name => <<"n1-standard-8">>, vcpus => 8, memory_mb => 30720},
        #{name => <<"n2-standard-2">>, vcpus => 2, memory_mb => 8192},
        #{name => <<"n2-standard-4">>, vcpus => 4, memory_mb => 16384},
        #{name => <<"n2-standard-8">>, vcpus => 8, memory_mb => 32768}
    ].
