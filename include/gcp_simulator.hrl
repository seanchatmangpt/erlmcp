%%%-------------------------------------------------------------------
%% @doc GCP Simulator Header - Types and Records
%%
%% Simulates Google Cloud Platform services for testing MCP+ governance.
%%
%% @end
%%%-------------------------------------------------------------------

-ifndef(GCP_SIMULATOR_HRL).
-define(GCP_SIMULATOR_HRL, true).

%%====================================================================
%% Common Types
%%====================================================================

-type project_id() :: binary().
-type region() :: binary().
-type zone() :: binary().
-type resource_id() :: binary().
-type timestamp_ms() :: non_neg_integer().
-type etag() :: binary().

%%====================================================================
%% IAM Types
%%====================================================================

-type principal() :: binary().  %% user:email, serviceAccount:email, group:email
-type role() :: binary().       %% roles/storage.admin, roles/pubsub.publisher
-type permission() :: binary(). %% storage.objects.get, pubsub.topics.publish

-record(gcp_iam_binding, {
    role :: role(),
    members :: [principal()],
    condition :: map() | undefined
}).

-record(gcp_iam_policy, {
    version = 1 :: pos_integer(),
    bindings = [] :: [#gcp_iam_binding{}],
    etag :: etag()
}).

-record(gcp_service_account, {
    email :: binary(),
    project_id :: project_id(),
    unique_id :: binary(),
    display_name :: binary(),
    description :: binary(),
    disabled = false :: boolean(),
    created_at :: timestamp_ms()
}).

%%====================================================================
%% Cloud Storage Types
%%====================================================================

-type storage_class() :: standard | nearline | coldline | archive.
-type acl_entity() :: binary().  %% user-email, group-email, allUsers, allAuthenticatedUsers

-record(gcp_bucket, {
    name :: binary(),
    project_id :: project_id(),
    location :: region(),
    storage_class = standard :: storage_class(),
    versioning_enabled = false :: boolean(),
    lifecycle_rules = [] :: [map()],
    labels = #{} :: map(),
    iam_policy :: #gcp_iam_policy{} | undefined,
    created_at :: timestamp_ms(),
    updated_at :: timestamp_ms(),
    etag :: etag()
}).

-record(gcp_object, {
    bucket :: binary(),
    name :: binary(),
    generation :: pos_integer(),
    metageneration :: pos_integer(),
    content_type :: binary(),
    size :: non_neg_integer(),
    md5_hash :: binary(),
    crc32c :: binary(),
    data :: binary(),
    metadata = #{} :: map(),
    acl = [] :: [{acl_entity(), binary()}],
    created_at :: timestamp_ms(),
    updated_at :: timestamp_ms(),
    etag :: etag()
}).

%%====================================================================
%% Pub/Sub Types
%%====================================================================

-type ack_id() :: binary().
-type subscription_type() :: pull | push.

-record(gcp_topic, {
    name :: binary(),              %% projects/{project}/topics/{topic}
    project_id :: project_id(),
    labels = #{} :: map(),
    message_retention_duration :: non_neg_integer() | undefined,
    schema_settings :: map() | undefined,
    created_at :: timestamp_ms()
}).

-record(gcp_subscription, {
    name :: binary(),              %% projects/{project}/subscriptions/{sub}
    project_id :: project_id(),
    topic :: binary(),
    type = pull :: subscription_type(),
    push_config :: map() | undefined,
    ack_deadline_seconds = 10 :: pos_integer(),
    message_retention_duration = 604800 :: pos_integer(),  %% 7 days
    retain_acked_messages = false :: boolean(),
    labels = #{} :: map(),
    filter :: binary() | undefined,
    dead_letter_policy :: map() | undefined,
    retry_policy :: map() | undefined,
    created_at :: timestamp_ms()
}).

-record(gcp_pubsub_message, {
    message_id :: binary(),
    data :: binary(),
    attributes = #{} :: map(),
    publish_time :: timestamp_ms(),
    ordering_key :: binary() | undefined
}).

-record(gcp_received_message, {
    ack_id :: ack_id(),
    message :: #gcp_pubsub_message{},
    delivery_attempt :: pos_integer()
}).

%%====================================================================
%% Compute Engine Types
%%====================================================================

-type machine_type() :: binary().  %% e2-micro, n1-standard-1, etc.
-type instance_status() :: provisioning | staging | running |
                           stopping | stopped | suspending |
                           suspended | terminated.

-record(gcp_disk, {
    name :: binary(),
    project_id :: project_id(),
    zone :: zone(),
    size_gb :: pos_integer(),
    type :: binary(),  %% pd-standard, pd-ssd, pd-balanced
    source_image :: binary() | undefined,
    status :: binary(),
    created_at :: timestamp_ms()
}).

-record(gcp_network_interface, {
    network :: binary(),
    subnetwork :: binary() | undefined,
    network_ip :: binary() | undefined,
    access_configs = [] :: [map()]
}).

-record(gcp_instance, {
    name :: binary(),
    project_id :: project_id(),
    zone :: zone(),
    machine_type :: machine_type(),
    status = provisioning :: instance_status(),
    disks = [] :: [#gcp_disk{}],
    network_interfaces = [] :: [#gcp_network_interface{}],
    service_accounts = [] :: [map()],
    labels = #{} :: map(),
    metadata = #{} :: map(),
    tags = [] :: [binary()],
    created_at :: timestamp_ms(),
    last_start_time :: timestamp_ms() | undefined,
    last_stop_time :: timestamp_ms() | undefined
}).

%%====================================================================
%% Cloud Functions Types
%%====================================================================

-type function_runtime() :: binary().  %% nodejs20, python311, go121, etc.
-type function_status() :: deploying | active | offline | delete_requested.

-record(gcp_cloud_function, {
    name :: binary(),              %% projects/{project}/locations/{region}/functions/{name}
    project_id :: project_id(),
    region :: region(),
    runtime :: function_runtime(),
    entry_point :: binary(),
    source_code :: binary(),       %% Simulated source
    trigger :: map(),
    environment_variables = #{} :: map(),
    available_memory_mb = 256 :: pos_integer(),
    timeout_seconds = 60 :: pos_integer(),
    max_instances :: pos_integer() | undefined,
    min_instances = 0 :: non_neg_integer(),
    status = deploying :: function_status(),
    service_account :: binary() | undefined,
    labels = #{} :: map(),
    created_at :: timestamp_ms(),
    updated_at :: timestamp_ms()
}).

%%====================================================================
%% Cloud Logging Types
%%====================================================================

-type log_severity() :: default | debug | info | notice |
                        warning | error | critical | alert | emergency.

-record(gcp_log_entry, {
    log_name :: binary(),          %% projects/{project}/logs/{log_id}
    resource :: map(),             %% {type, labels}
    timestamp :: timestamp_ms(),
    receive_timestamp :: timestamp_ms(),
    severity = default :: log_severity(),
    insert_id :: binary(),
    labels = #{} :: map(),
    text_payload :: binary() | undefined,
    json_payload :: map() | undefined,
    proto_payload :: binary() | undefined,
    trace :: binary() | undefined,
    span_id :: binary() | undefined
}).

%%====================================================================
%% BigQuery Types
%%====================================================================

-type bq_field_type() :: string | bytes | integer | float |
                         boolean | timestamp | date | time |
                         datetime | record | numeric | geography.

-record(gcp_bq_field, {
    name :: binary(),
    type :: bq_field_type(),
    mode = nullable :: nullable | required | repeated,
    description :: binary() | undefined,
    fields = [] :: [#gcp_bq_field{}]  %% For RECORD type
}).

-record(gcp_bq_table, {
    project_id :: project_id(),
    dataset_id :: binary(),
    table_id :: binary(),
    schema = [] :: [#gcp_bq_field{}],
    description :: binary() | undefined,
    labels = #{} :: map(),
    num_rows = 0 :: non_neg_integer(),
    num_bytes = 0 :: non_neg_integer(),
    created_at :: timestamp_ms(),
    modified_at :: timestamp_ms()
}).

-record(gcp_bq_dataset, {
    project_id :: project_id(),
    dataset_id :: binary(),
    location :: region(),
    description :: binary() | undefined,
    labels = #{} :: map(),
    default_table_expiration_ms :: non_neg_integer() | undefined,
    created_at :: timestamp_ms(),
    modified_at :: timestamp_ms()
}).

%%====================================================================
%% Error Types
%%====================================================================

-record(gcp_error, {
    code :: pos_integer(),         %% HTTP status code
    message :: binary(),
    status :: binary(),            %% NOT_FOUND, PERMISSION_DENIED, etc.
    details = [] :: [map()]
}).

%% Standard GCP error codes
-define(GCP_OK, 200).
-define(GCP_CREATED, 201).
-define(GCP_NO_CONTENT, 204).
-define(GCP_BAD_REQUEST, 400).
-define(GCP_UNAUTHORIZED, 401).
-define(GCP_FORBIDDEN, 403).
-define(GCP_NOT_FOUND, 404).
-define(GCP_CONFLICT, 409).
-define(GCP_PRECONDITION_FAILED, 412).
-define(GCP_TOO_MANY_REQUESTS, 429).
-define(GCP_INTERNAL_ERROR, 500).
-define(GCP_SERVICE_UNAVAILABLE, 503).

-endif. %% GCP_SIMULATOR_HRL
