%%%-------------------------------------------------------------------
%% @doc GCP Cloud Storage Simulator
%%
%% Simulates Google Cloud Storage for testing:
%% - Bucket operations (create, get, list, delete)
%% - Object operations (upload, download, copy, delete)
%% - ACL and IAM operations
%% - Versioning support
%%
%% @end
%%%-------------------------------------------------------------------
-module(gcp_storage_sim).
-behaviour(gen_server).

-include("gcp_simulator.hrl").

%% API
-export([
    start_link/0,
    stop/0
]).

%% Bucket Operations
-export([
    create_bucket/2,
    create_bucket/3,
    get_bucket/1,
    list_buckets/1,
    delete_bucket/1,
    update_bucket/2
]).

%% Object Operations
-export([
    upload_object/4,
    upload_object/5,
    download_object/2,
    get_object_metadata/2,
    list_objects/1,
    list_objects/2,
    delete_object/2,
    copy_object/4
]).

%% Versioning
-export([
    list_object_versions/2,
    get_object_version/3
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    buckets = #{} :: #{binary() => #gcp_bucket{}},
    objects = #{} :: #{{binary(), binary()} => #gcp_object{}},
    versions = #{} :: #{{binary(), binary()} => [#gcp_object{}]},
    generation_counter = 1 :: pos_integer()
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?SERVER).

%% @doc Create a new bucket.
-spec create_bucket(project_id(), binary()) ->
    {ok, #gcp_bucket{}} | {error, #gcp_error{}}.
create_bucket(ProjectId, BucketName) ->
    create_bucket(ProjectId, BucketName, #{}).

-spec create_bucket(project_id(), binary(), map()) ->
    {ok, #gcp_bucket{}} | {error, #gcp_error{}}.
create_bucket(ProjectId, BucketName, Opts) ->
    gen_server:call(?SERVER, {create_bucket, ProjectId, BucketName, Opts}).

%% @doc Get bucket metadata.
-spec get_bucket(binary()) ->
    {ok, #gcp_bucket{}} | {error, #gcp_error{}}.
get_bucket(BucketName) ->
    gen_server:call(?SERVER, {get_bucket, BucketName}).

%% @doc List buckets in a project.
-spec list_buckets(project_id()) -> {ok, [#gcp_bucket{}]}.
list_buckets(ProjectId) ->
    gen_server:call(?SERVER, {list_buckets, ProjectId}).

%% @doc Delete a bucket.
-spec delete_bucket(binary()) -> ok | {error, #gcp_error{}}.
delete_bucket(BucketName) ->
    gen_server:call(?SERVER, {delete_bucket, BucketName}).

%% @doc Update bucket metadata.
-spec update_bucket(binary(), map()) ->
    {ok, #gcp_bucket{}} | {error, #gcp_error{}}.
update_bucket(BucketName, Updates) ->
    gen_server:call(?SERVER, {update_bucket, BucketName, Updates}).

%% @doc Upload an object.
-spec upload_object(binary(), binary(), binary(), binary()) ->
    {ok, #gcp_object{}} | {error, #gcp_error{}}.
upload_object(BucketName, ObjectName, ContentType, Data) ->
    upload_object(BucketName, ObjectName, ContentType, Data, #{}).

-spec upload_object(binary(), binary(), binary(), binary(), map()) ->
    {ok, #gcp_object{}} | {error, #gcp_error{}}.
upload_object(BucketName, ObjectName, ContentType, Data, Metadata) ->
    gen_server:call(?SERVER, {upload_object, BucketName, ObjectName,
                              ContentType, Data, Metadata}).

%% @doc Download an object.
-spec download_object(binary(), binary()) ->
    {ok, binary()} | {error, #gcp_error{}}.
download_object(BucketName, ObjectName) ->
    gen_server:call(?SERVER, {download_object, BucketName, ObjectName}).

%% @doc Get object metadata.
-spec get_object_metadata(binary(), binary()) ->
    {ok, #gcp_object{}} | {error, #gcp_error{}}.
get_object_metadata(BucketName, ObjectName) ->
    gen_server:call(?SERVER, {get_object_metadata, BucketName, ObjectName}).

%% @doc List objects in a bucket.
-spec list_objects(binary()) -> {ok, [#gcp_object{}]} | {error, #gcp_error{}}.
list_objects(BucketName) ->
    list_objects(BucketName, #{}).

-spec list_objects(binary(), map()) ->
    {ok, [#gcp_object{}]} | {error, #gcp_error{}}.
list_objects(BucketName, Opts) ->
    gen_server:call(?SERVER, {list_objects, BucketName, Opts}).

%% @doc Delete an object.
-spec delete_object(binary(), binary()) -> ok | {error, #gcp_error{}}.
delete_object(BucketName, ObjectName) ->
    gen_server:call(?SERVER, {delete_object, BucketName, ObjectName}).

%% @doc Copy an object.
-spec copy_object(binary(), binary(), binary(), binary()) ->
    {ok, #gcp_object{}} | {error, #gcp_error{}}.
copy_object(SrcBucket, SrcObject, DstBucket, DstObject) ->
    gen_server:call(?SERVER, {copy_object, SrcBucket, SrcObject,
                              DstBucket, DstObject}).

%% @doc List object versions.
-spec list_object_versions(binary(), binary()) ->
    {ok, [#gcp_object{}]} | {error, #gcp_error{}}.
list_object_versions(BucketName, ObjectName) ->
    gen_server:call(?SERVER, {list_object_versions, BucketName, ObjectName}).

%% @doc Get a specific object version.
-spec get_object_version(binary(), binary(), pos_integer()) ->
    {ok, #gcp_object{}} | {error, #gcp_error{}}.
get_object_version(BucketName, ObjectName, Generation) ->
    gen_server:call(?SERVER, {get_object_version, BucketName, ObjectName,
                              Generation}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    {ok, #state{}}.

handle_call({create_bucket, ProjectId, BucketName, Opts}, _From, State) ->
    case maps:is_key(BucketName, State#state.buckets) of
        true ->
            Error = #gcp_error{
                code = ?GCP_CONFLICT,
                message = <<"Bucket already exists">>,
                status = <<"ALREADY_EXISTS">>
            },
            {reply, {error, Error}, State};
        false ->
            Now = erlang:system_time(millisecond),
            Bucket = #gcp_bucket{
                name = BucketName,
                project_id = ProjectId,
                location = maps:get(location, Opts, <<"US">>),
                storage_class = maps:get(storage_class, Opts, standard),
                versioning_enabled = maps:get(versioning, Opts, false),
                labels = maps:get(labels, Opts, #{}),
                created_at = Now,
                updated_at = Now,
                etag = generate_etag()
            },
            NewBuckets = maps:put(BucketName, Bucket, State#state.buckets),
            {reply, {ok, Bucket}, State#state{buckets = NewBuckets}}
    end;

handle_call({get_bucket, BucketName}, _From, State) ->
    case maps:get(BucketName, State#state.buckets, undefined) of
        undefined ->
            {reply, {error, not_found_error(<<"Bucket">>)}, State};
        Bucket ->
            {reply, {ok, Bucket}, State}
    end;

handle_call({list_buckets, ProjectId}, _From, State) ->
    Buckets = maps:fold(
        fun(_Name, Bucket, Acc) ->
            case Bucket#gcp_bucket.project_id of
                ProjectId -> [Bucket | Acc];
                _ -> Acc
            end
        end,
        [],
        State#state.buckets
    ),
    {reply, {ok, Buckets}, State};

handle_call({delete_bucket, BucketName}, _From, State) ->
    case maps:get(BucketName, State#state.buckets, undefined) of
        undefined ->
            {reply, {error, not_found_error(<<"Bucket">>)}, State};
        _Bucket ->
            %% Check if bucket is empty
            HasObjects = maps:fold(
                fun({Bucket, _}, _, Acc) ->
                    Acc orelse (Bucket =:= BucketName)
                end,
                false,
                State#state.objects
            ),
            case HasObjects of
                true ->
                    Error = #gcp_error{
                        code = ?GCP_CONFLICT,
                        message = <<"Bucket is not empty">>,
                        status = <<"FAILED_PRECONDITION">>
                    },
                    {reply, {error, Error}, State};
                false ->
                    NewBuckets = maps:remove(BucketName, State#state.buckets),
                    {reply, ok, State#state{buckets = NewBuckets}}
            end
    end;

handle_call({update_bucket, BucketName, Updates}, _From, State) ->
    case maps:get(BucketName, State#state.buckets, undefined) of
        undefined ->
            {reply, {error, not_found_error(<<"Bucket">>)}, State};
        Bucket ->
            Now = erlang:system_time(millisecond),
            UpdatedBucket = apply_bucket_updates(Bucket, Updates, Now),
            NewBuckets = maps:put(BucketName, UpdatedBucket, State#state.buckets),
            {reply, {ok, UpdatedBucket}, State#state{buckets = NewBuckets}}
    end;

handle_call({upload_object, BucketName, ObjectName, ContentType, Data, Metadata},
            _From, State) ->
    case maps:get(BucketName, State#state.buckets, undefined) of
        undefined ->
            {reply, {error, not_found_error(<<"Bucket">>)}, State};
        Bucket ->
            Now = erlang:system_time(millisecond),
            Generation = State#state.generation_counter,

            Object = #gcp_object{
                bucket = BucketName,
                name = ObjectName,
                generation = Generation,
                metageneration = 1,
                content_type = ContentType,
                size = byte_size(Data),
                md5_hash = base64:encode(crypto:hash(md5, Data)),
                crc32c = compute_crc32c(Data),
                data = Data,
                metadata = Metadata,
                created_at = Now,
                updated_at = Now,
                etag = generate_etag()
            },

            Key = {BucketName, ObjectName},
            NewObjects = maps:put(Key, Object, State#state.objects),

            %% Handle versioning
            NewVersions = case Bucket#gcp_bucket.versioning_enabled of
                true ->
                    OldVersions = maps:get(Key, State#state.versions, []),
                    maps:put(Key, [Object | OldVersions], State#state.versions);
                false ->
                    State#state.versions
            end,

            NewState = State#state{
                objects = NewObjects,
                versions = NewVersions,
                generation_counter = Generation + 1
            },
            {reply, {ok, Object}, NewState}
    end;

handle_call({download_object, BucketName, ObjectName}, _From, State) ->
    Key = {BucketName, ObjectName},
    case maps:get(Key, State#state.objects, undefined) of
        undefined ->
            {reply, {error, not_found_error(<<"Object">>)}, State};
        Object ->
            {reply, {ok, Object#gcp_object.data}, State}
    end;

handle_call({get_object_metadata, BucketName, ObjectName}, _From, State) ->
    Key = {BucketName, ObjectName},
    case maps:get(Key, State#state.objects, undefined) of
        undefined ->
            {reply, {error, not_found_error(<<"Object">>)}, State};
        Object ->
            %% Return object without data
            {reply, {ok, Object#gcp_object{data = <<>>}}, State}
    end;

handle_call({list_objects, BucketName, Opts}, _From, State) ->
    case maps:get(BucketName, State#state.buckets, undefined) of
        undefined ->
            {reply, {error, not_found_error(<<"Bucket">>)}, State};
        _Bucket ->
            Prefix = maps:get(prefix, Opts, <<>>),
            Objects = maps:fold(
                fun({Bucket, Name}, Obj, Acc) ->
                    Matches = Bucket =:= BucketName andalso has_prefix(Name, Prefix),
                    case Matches of
                        true -> [Obj#gcp_object{data = <<>>} | Acc];
                        false -> Acc
                    end
                end,
                [],
                State#state.objects
            ),
            {reply, {ok, Objects}, State}
    end;

handle_call({delete_object, BucketName, ObjectName}, _From, State) ->
    Key = {BucketName, ObjectName},
    case maps:get(Key, State#state.objects, undefined) of
        undefined ->
            {reply, {error, not_found_error(<<"Object">>)}, State};
        _Object ->
            NewObjects = maps:remove(Key, State#state.objects),
            {reply, ok, State#state{objects = NewObjects}}
    end;

handle_call({copy_object, SrcBucket, SrcObject, DstBucket, DstObject}, _From, State) ->
    SrcKey = {SrcBucket, SrcObject},
    case maps:get(SrcKey, State#state.objects, undefined) of
        undefined ->
            {reply, {error, not_found_error(<<"Source object">>)}, State};
        SrcObj ->
            case maps:is_key(DstBucket, State#state.buckets) of
                false ->
                    {reply, {error, not_found_error(<<"Destination bucket">>)}, State};
                true ->
                    Now = erlang:system_time(millisecond),
                    Generation = State#state.generation_counter,
                    DstObj = SrcObj#gcp_object{
                        bucket = DstBucket,
                        name = DstObject,
                        generation = Generation,
                        metageneration = 1,
                        created_at = Now,
                        updated_at = Now,
                        etag = generate_etag()
                    },
                    DstKey = {DstBucket, DstObject},
                    NewObjects = maps:put(DstKey, DstObj, State#state.objects),
                    NewState = State#state{
                        objects = NewObjects,
                        generation_counter = Generation + 1
                    },
                    {reply, {ok, DstObj}, NewState}
            end
    end;

handle_call({list_object_versions, BucketName, ObjectName}, _From, State) ->
    Key = {BucketName, ObjectName},
    case maps:get(Key, State#state.versions, undefined) of
        undefined ->
            %% Check if object exists at all
            case maps:get(Key, State#state.objects, undefined) of
                undefined ->
                    {reply, {error, not_found_error(<<"Object">>)}, State};
                Object ->
                    {reply, {ok, [Object#gcp_object{data = <<>>}]}, State}
            end;
        Versions ->
            {reply, {ok, [V#gcp_object{data = <<>>} || V <- Versions]}, State}
    end;

handle_call({get_object_version, BucketName, ObjectName, Generation}, _From, State) ->
    Key = {BucketName, ObjectName},
    Versions = maps:get(Key, State#state.versions, []),
    case lists:keyfind(Generation, #gcp_object.generation, Versions) of
        false ->
            {reply, {error, not_found_error(<<"Object version">>)}, State};
        Object ->
            {reply, {ok, Object}, State}
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

generate_etag() ->
    base64:encode(crypto:strong_rand_bytes(12)).

not_found_error(Resource) ->
    #gcp_error{
        code = ?GCP_NOT_FOUND,
        message = <<Resource/binary, " not found">>,
        status = <<"NOT_FOUND">>
    }.

compute_crc32c(Data) ->
    %% Simplified CRC32C (in production, use erlang-crc32c NIF)
    Crc = erlang:crc32(Data),
    base64:encode(<<Crc:32/big>>).

%% @doc Check if a binary has a given prefix.
has_prefix(_Name, <<>>) ->
    true;  %% Empty prefix matches everything
has_prefix(Name, Prefix) ->
    PrefixLen = byte_size(Prefix),
    case Name of
        <<Prefix:PrefixLen/binary, _/binary>> -> true;
        _ -> false
    end.

apply_bucket_updates(Bucket, Updates, Now) ->
    Bucket1 = case maps:get(versioning, Updates, undefined) of
        undefined -> Bucket;
        V -> Bucket#gcp_bucket{versioning_enabled = V}
    end,
    Bucket2 = case maps:get(labels, Updates, undefined) of
        undefined -> Bucket1;
        L -> Bucket1#gcp_bucket{labels = L}
    end,
    Bucket3 = case maps:get(storage_class, Updates, undefined) of
        undefined -> Bucket2;
        S -> Bucket2#gcp_bucket{storage_class = S}
    end,
    Bucket3#gcp_bucket{
        updated_at = Now,
        etag = generate_etag()
    }.
