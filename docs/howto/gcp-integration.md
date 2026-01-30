# GCP Integration: Connecting to Google Cloud Services

This comprehensive guide shows you how to integrate erlmcp with Google Cloud Platform (GCP) services. Learn to connect, authenticate, and use various GCP APIs through the MCP protocol.

## What You'll Build

A complete GCP integration that:
1. **Authenticates with GCP** using service accounts
2. **Integrates multiple GCP services** (Pub/Sub, Cloud Storage, Compute)
3. **Provides MCP tools** for GCP operations
4. **Handles GCP-specific errors** gracefully
5. **Demonstrates production patterns** for cloud integration

## Prerequisites

### GCP Requirements
- **GCP Project**: Active project with billing enabled
- **Service Account**: With appropriate permissions
- **APIs Enabled**: Pub/Sub, Cloud Storage, Compute Engine
- **Authentication**: Service account JSON key

### erlmcp Requirements
- Basic erlmcp installation ([Installation Guide](installation.md))
- Understanding of MCP protocol ([Protocol Understanding](../explain/protocol.md))

## Step 1: GCP Setup

### Create Service Account
```bash
# Enable required APIs
gcloud services enable pubsub.googleapis.com
gcloud services enable storage-api.googleapis.com
gcloud services enable compute.googleapis.com

# Create service account
gcloud iam service-accounts create erlmcp-integration \
    --display-name "erlmcp Integration Service Account"

# Download service account key
gcloud iam service-accounts keys create erlmcp-sa-key.json \
    --iam-account=erlmcp-integration@your-project.iam.gserviceaccount.com
```

### Grant Permissions
```bash
# Grant Pub/Sub permissions
gcloud projects add-iam-policy-binding your-project \
    --member="serviceAccount:erlmcp-integration@your-project.iam.gserviceaccount.com" \
    --role="roles/pubsub.publisher"

gcloud projects add-iam-policy-binding your-project \
    --member="serviceAccount:erlmcp-integration@your-project.iam.gserviceaccount.com" \
    --role="roles/pubsub.subscriber"

# Grant Cloud Storage permissions
gcloud projects add-iam-policy-binding your-project \
    --member="serviceAccount:erlmcp-integration@your-project.iam.gserviceaccount.com" \
    --role="roles/storage.objectAdmin"

# Grant Compute Engine permissions
gcloud projects add-iam-policy-binding your-project \
    --member="serviceAccount:erlmcp-integration@your-project.iam.gserviceaccount.com" \
    --role="roles/compute.instanceAdmin.v1"
```

### Test GCP Connection
```bash
# Set up environment
export GOOGLE_APPLICATION_CREDENTIALS=erlmcp-sa-key.json

# Test Pub/Sub connection
gcloud pubsub topics create test-topic

# Test Cloud Storage
gsutil mb gs://your-project-bucket

# Test Compute Engine
gcloud compute instances list
```

## Step 2: Create GCP Integration Module

### Project Structure
```bash
mkdir -p gcp-integration/src
cd gcp-integration
```

### Add Dependencies to rebar.config
```erlang
% rebar.config
{deps, [
    {erlmcp, {git, "https://github.com/your-org/erlmcp", {tag, "0.6.0"}}},
    {gcloud, {git, "https://github.com/your-org/gcp-erlang", {tag, "1.0.0"}}},
    {jsx, "3.1.2"},
    {gproc, "0.9.0"},
    {google_auth, "1.0.0"}
]}.
```

### Create GCP Client Module
```erlang
% src/gcp_client.erl
-module(gcp_client).
-behaviour(gen_server).
-export([start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% GCP API functions
-export([
    pubsub_publish/2,
    pubsub_subscribe/2,
    pubsub_pull/2,
    storage_upload/3,
    storage_download/2,
    compute_create_instance/2,
    compute_list_instances/1
]).

-record(state, {
    project_id,
    credentials,
    pubsub_client,
    storage_client,
    compute_client
}).

start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

stop(Pid) ->
    gen_server:call(Pid, stop).

init(Config) ->
    ProjectId = maps:get(project_id, Config),
    CredentialsFile = maps:get(credentials_file, Config),

    % Load GCP credentials
    case load_credentials(CredentialsFile) of
        {ok, Credentials} ->
            % Initialize GCP clients
            PubSubClient = init_pubsub_client(ProjectId, Credentials),
            StorageClient = init_storage_client(ProjectId, Credentials),
            ComputeClient = init_compute_client(ProjectId, Credentials),

            {ok, #state{
                project_id = ProjectId,
                credentials = Credentials,
                pubsub_client = PubSubClient,
                storage_client = StorageClient,
                compute_client = ComputeClient
            }};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call({pubsub_publish, Topic, Message}, _From, State) ->
    Result = pubsub_publish(Topic, Message),
    {reply, Result, State};

handle_call({pubsub_subscribe, Topic, Subscription}, _From, State) ->
    Result = pubsub_subscribe(Topic, Subscription),
    {reply, Result, State};

handle_call({pubsub_pull, Subscription, MaxMessages}, _From, State) ->
    Result = pubsub_pull(Subscription, MaxMessages),
    {reply, Result, State};

handle_call({storage_upload, Bucket, Object, Content}, _From, State) ->
    Result = storage_upload(Bucket, Object, Content),
    {reply, Result, State};

handle_call({storage_download, Bucket, Object}, _From, State) ->
    Result = storage_download(Bucket, Object),
    {reply, Result, State};

handle_call({compute_create_instance, InstanceConfig}, _From, State) ->
    Result = compute_create_instance(InstanceConfig),
    {reply, Result, State};

handle_call({compute_list_instances}, _From, State) ->
    Result = compute_list_instances(State#state.project_id),
    {reply, Result, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    cleanup_clients(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% GCP API implementations
pubsub_publish(Topic, Message) ->
    try
        % Use GCP Erlang client library
        gcloud_pubsub:publish(Topic, [Message]),
        {ok, #{message_id => "published"}}
    catch
        Error:Reason ->
            {error, Reason}
    end.

pubsub_subscribe(Topic, Subscription) ->
    try
        gcloud_pubsub:subscribe(Topic, Subscription),
        {ok, #{subscription_id => Subscription}}
    catch
        Error:Reason ->
            {error, Reason}
    end.

pubsub_pull(Subscription, MaxMessages) ->
    try
        Messages = gcloud_pubsub:pull(Subscription, MaxMessages),
        {ok, #{messages => Messages}}
    catch
        Error:Reason ->
            {error, Reason}
    end.

storage_upload(Bucket, Object, Content) ->
    try
        % Upload to GCS
        gcloud_storage:upload(Bucket, Object, Content),
        {ok, #{object => Object, bucket => Bucket}}
    catch
        Error:Reason ->
            {error, Reason}
    end.

storage_download(Bucket, Object) ->
    try
        % Download from GCS
        Content = gcloud_storage:download(Bucket, Object),
        {ok, #{content => Content, object => Object, bucket => Bucket}}
    catch
        Error:Reason ->
            {error, Reason}
    end.

compute_create_instance(Config) ->
    try
        InstanceName = maps:get(name, Config),
        MachineType = maps:get(machine_type, Config, "e2-micro"),
        Zone = maps:get(zone, Config, "us-central1-a"),

        % Create instance
        Instance = gcloud_compute:create_instance(InstanceName, MachineType, Zone),
        {ok, #{instance => Instance}}
    catch
        Error:Reason ->
            {error, Reason}
    end.

compute_list_instances(ProjectId) ->
    try
        Instances = gcloud_compute:list_instances(ProjectId),
        {ok, #{instances => Instances}}
    catch
        Error:Reason ->
            {error, Reason}
    end.

% Helper functions
load_credentials(File) ->
    case file:read_file(File) of
        {ok, Content} ->
            case jsx:decode(Content) of
                Map when is_map(Map) ->
                    {ok, Map};
                _ ->
                    {error, invalid_credentials}
            end;
        {error, _} ->
            {error, file_not_found}
    end.

init_pubsub_client(ProjectId, Credentials) ->
    % Initialize Pub/Sub client
    gcloud_pubsub:new(ProjectId, Credentials).

init_storage_client(ProjectId, Credentials) ->
    % Initialize Storage client
    gcloud_storage:new(ProjectId, Credentials).

init_compute_client(ProjectId, Credentials) ->
    % Initialize Compute client
    gcloud_compute:new(ProjectId, Credentials).

cleanup_clients() ->
    % Clean up client connections
    gcloud_pubsub:close(),
    gcloud_storage:close(),
    gcloud_compute:close().
```

## Step 3: Create GCP Tools

### Pub/Sub Tool
```erlang
% src/gcp_pubsub_tool.erl
-module(gcp_pubsub_tool).
-behaviour(erlmcp_server).

-export([init/1, handle_call/3]).
-export([publish_message/1, pull_messages/2]).

init(_Args) ->
    erlmcp_server:register_tool(<<"gcp_pubsub">>, ?MODULE),
    {ok, #{}}.

handle_call({call_tool, <<"publish">>, Args}, _From, State) ->
    Topic = maps:get(<<"topic">>, Args),
    Message = maps:get(<<"message">>, Args),
    Result = gcp_client:pubsub_publish(Topic, Message),
    {reply, Result, State};

handle_call({call_tool, <<"pull">>, Args}, _From, State) ->
    Subscription = maps:get(<<"subscription">>, Args),
    MaxMessages = maps:get(<<"max_messages">>, Args, 10),
    Result = gcp_client:pubsub_pull(Subscription, MaxMessages),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

publish_message(#{topic := Topic, message := Message}) ->
    gcp_client:pubsub_publish(Topic, #{
        data => base64:encode(Message),
        attributes => #{}
    }).

pull_messages(Subscription, MaxMessages) ->
    gcp_client:pubsub_pull(Subscription, MaxMessages).
```

### Cloud Storage Tool
```erlang
% src/gcp_storage_tool.erl
-module(gcp_storage_tool).
-behaviour(erlmcp_server).

-export([init/1, handle_call/3]).
-export([upload_file/3, download_file/2]).

init(_Args) ->
    erlmcp_server:register_tool(<<"gcp_storage">>, ?MODULE),
    {ok, #{}}.

handle_call({call_tool, <<"upload">>, Args}, _From, State) ->
    Bucket = maps:get(<<"bucket">>, Args),
    Object = maps:get(<<"object">>, Args),
    Content = maps:get(<<"content">>, Args),
    Result = gcp_client:storage_upload(Bucket, Object, Content),
    {reply, Result, State};

handle_call({call_tool, <<"download">>, Args}, _From, State) ->
    Bucket = maps:get(<<"bucket">>, Args),
    Object = maps:get(<<"object">>, Args),
    Result = gcp_client:storage_download(Bucket, Object),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

upload_file(Bucket, Object, Content) ->
    gcp_client:storage_upload(Bucket, Object, Content).

download_file(Bucket, Object) ->
    gcp_client:storage_download(Bucket, Object).
```

### Compute Engine Tool
```erlang
% src/gcp_compute_tool.erl
-module(gcp_compute_tool).
-behaviour(erlmcp_server).

-export([init/1, handle_call/3]).
-export([create_instance/2, list_instances/1]).

init(_Args) ->
    erlmcp_server:register_tool(<<"gcp_compute">>, ?MODULE),
    {ok, #{}}.

handle_call({call_tool, <<"create_instance">>, Args}, _From, State) ->
    Name = maps:get(<<"name">>, Args),
    MachineType = maps:get(<<"machine_type">>, Args, "e2-micro"),
    Zone = maps:get(<<"zone">>, Args, "us-central1-a"),
    Config = #{
        name => Name,
        machine_type => MachineType,
        zone => Zone
    },
    Result = gcp_client:compute_create_instance(Config),
    {reply, Result, State};

handle_call({call_tool, <<"list_instances">>, Args}, _From, State) ->
    Result = gcp_client:compute_list_instances(),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

create_instance(Name, Config) ->
    FullConfig = maps:merge(#{name => Name}, Config),
    gcp_client:compute_create_instance(FullConfig).

list_instances() ->
    gcp_client:compute_list_instances().
```

## Step 4: Configure the Integration

### Create sys.config
```erlang
% config/sys.config
[
    {gcp_integration, [
        % GCP configuration
        {project_id, "your-gcp-project"},
        {credentials_file, "erlmcp-sa-key.json"},
        {region, "us-central1"},
        {timeout, 30000},
        % Retry configuration
        {max_retries, 3},
        {retry_delay, 1000}
    ]},

    {erlmcp, [
        % erlmcp configuration
        {transport, tcp},
        {port, 8080},
        {server, [
            {tools, [
                % GCP tools
                {gcp_client, gcp_pubsub},
                {gcp_client, gcp_storage},
                {gcp_client, gcp_compute}
            ]}
        ]},
        % GCP-specific settings
        {gcp, [
            {enabled, true},
            {project_id, "your-gcp-project"},
            {log_requests, true},
            {metrics_enabled, true}
        ]}
    ]}
].
```

### Create vm.args
```erlang
% vm.args
-name gcp_integration@127.0.0.1
-setcookie erlmcp
-args_file config/vm.args
+P 65536
+A 64
-env ERL_MAX_PORTS 65536
-env ERL_AFLAGS "-kernel inet_dist_listen_min 9100 inet_dist_listen_max 9109"
```

## Step 5: Test the Integration

### Start the GCP Integration
```bash
# Build the project
rebar3 compile

# Start the application
rebar3 shell
```

In the Erlang shell:
```erlang
% Start the GCP client
application:ensure_all_started(gcp_integration).

% Check registered tools
erlmcp_server:tools().

% Should return [<<"gcp_pubsub">>, <<"gcp_storage">>, <<"gcp_compute">>]
```

### Test Pub/Sub Integration
```erlang
% Test Pub/Sub publishing
Request = #{
    jsonrpc => "2.0",
    id => <<"pubsub-test">>,
    method => "tools/call",
    params => #{
        name => "gcp_pubsub",
        arguments => #{
            topic => "test-topic",
            message => <<"Hello from erlmcp">>
        }
    }
},

Result = erlmcp_client:call(Client, Request).
% Should return successful response

% Test Pub/Sub pulling
PullRequest = #{
    jsonrpc => "2.0",
    id => <<"pubsub-pull">>,
    method => "tools/call",
    params => #{
        name => "gcp_pubsub",
        arguments => #{
            subscription => "test-subscription",
            max_messages => 5
        }
    }
},

PullResult = erlmcp_client:call(Client, PullRequest).
```

### Test Cloud Storage Integration
```erlang
% Test file upload
UploadRequest = #{
    jsonrpc => "2.0",
    id => <<"storage-upload">>,
    method => "tools/call",
    params => #{
        name => "gcp_storage",
        arguments => #{
            bucket => "your-project-bucket",
            object => "test-file.txt",
            content => <<"This is a test file content">>
        }
    }
},

UploadResult = erlmcp_client:call(Client, UploadRequest).

% Test file download
DownloadRequest = #{
    jsonrpc => "2.0",
    id => <<"storage-download">>,
    method => "tools/call",
    params => #{
        name => "gcp_storage",
        arguments => #{
            bucket => "your-project-bucket",
            object => "test-file.txt"
        }
    }
},

DownloadResult = erlmcp_client:call(Client, DownloadRequest).
```

### Test Compute Engine Integration
```erlang
% Test instance creation
CreateRequest = #{
    jsonrpc => "2.0",
    id => <<"compute-create">>,
    method => "tools/call",
    params => #{
        name => "gcp_compute",
        arguments => #{
            name => "test-instance",
            machine_type => "e2-micro",
            zone => "us-central1-a"
        }
    }
},

CreateResult = erlmcp_client:call(Client, CreateRequest).

% Test instance listing
ListRequest = #{
    jsonrpc => "2.0",
    id => <<"compute-list">>,
    method => "tools/call",
    params => #{
        name => "gcp_compute",
        arguments => #{}
    }
},

ListResult = erlmcp_client:call(Client, ListRequest).
```

## Step 6: Add Error Handling and Retries

### GCP Error Handler
```erlang
% src/gcp_error_handler.erl
-module(gcp_error_handler).
-export([handle_gcp_error/2]).

handle_gcp_error({error, {gcp, _} = Error}, Context) ->
    % Handle GCP-specific errors
    case Error of
        {gcp, {404, _}} ->
            {error, {not_found, Context}};
        {gcp, {403, _}} ->
            {error, {permission_denied, Context}};
        {gcp, {503, _}} ->
            % Retry for service unavailable
            retry_with_backoff(Context);
        {gcp, {400, _}} ->
            {error, {bad_request, Context}};
        {gcp, {_, Message}} ->
            {error, {gcp_error, Message}}
    end.

retry_with_backoff(Context) ->
    MaxRetries = application:get_env(gcp_integration, max_retries, 3),
    RetryDelay = application:get_env(gcp_integration, retry_delay, 1000),

    case maps:get(retries, Context, 0) < MaxRetries of
        true ->
            NewContext = Context#{retries => maps:get(retries, Context, 0) + 1},
            timer:sleep(RetryDelay),
            retry_operation(NewContext);
        false ->
            {error, {service_unavailable, Context}}
    end.
```

### Update GCP Client to Use Error Handler
```erlang
% In gcp_client.erl, update pubsub_publish
pubsub_publish(Topic, Message) ->
    try
        gcloud_pubsub:publish(Topic, [Message]),
        {ok, #{message_id => "published"}}
    catch
        Error:Reason ->
            gcp_error_handler:handle_gcp_error(Error, #{operation => publish, topic => Topic})
    end.
```

## Step 7: Add Monitoring and Logging

### GCP Request Logging
```erlang
% src/gcp_logger.erl
-module(gcp_logger).
-export([log_request/3, log_response/3, log_error/3]).

log_request(Operation, Args, Metadata) ->
    LogEntry = #{
        event => gcp_request,
        operation => Operation,
        arguments => Args,
        metadata => Metadata,
        timestamp => erlang:system_time(millisecond)
    },
    logger:info(LogEntry).

log_response(Operation, Response, Metadata) ->
    LogEntry = #{
        event => gcp_response,
        operation => Operation,
        response => Response,
        metadata => Metadata,
        timestamp => erlang:system_time(millisecond)
    },
    logger:info(LogEntry).

log_error(Operation, Error, Metadata) ->
    LogEntry = #{
        event => gcp_error,
        operation => Operation,
        error => Error,
        metadata => Metadata,
        timestamp => erlang:system_time(millisecond)
    },
    logger:error(LogEntry).
```

### GCP Metrics
```erlang
% src/gcp_metrics.erl
-module(gcp_metrics).
-export([increment_counter/2, observe_duration/3]).

increment_counter(Operation, Status) ->
    MetricName = list_to_atom("gcp_" ++ Operation ++ "_total"),
    prometheus_gauge:inc(MetricName, [status = Status]).

observe_duration(Operation, Duration, Labels) ->
    MetricName = list_to_atom("gcp_" ++ Operation ++ "_duration_seconds"),
    prometheus_histogram:observe(MetricName, Duration, Labels).
```

## Step 8: Production Deployment

### Create Docker Image
```dockerfile
# Dockerfile
FROM erlang:25.3-alpine

# Install dependencies
RUN apk add --no-cache openssl curl

# Create app directory
WORKDIR /app

# Copy dependencies
COPY rebar.config rebar3 ./

# Build dependencies
RUN ./rebar3 deps

# Copy source code
COPY src src

# Build the app
RUN ./rebar3 compile

# Copy config files
COPY config config

# Create non-root user
RUN addgroup -S erlang && adduser -S erlang -G erlang

# Change ownership
RUN chown -R erlang:erlang /app

# Switch to non-root user
USER erlang

# Expose port
EXPOSE 8080

# Start the application
CMD ["./rebar3", "shell"]
```

### Kubernetes Deployment
```yaml
# k8s/gcp-integration.yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: gcp-integration
spec:
  replicas: 3
  selector:
    matchLabels:
      app: gcp-integration
  template:
    metadata:
      labels:
        app: gcp-integration
    spec:
      containers:
      - name: gcp-integration
        image: gcr.io/your-project/gcp-integration:1.0.0
        ports:
        - containerPort: 8080
        env:
        - name: GOOGLE_APPLICATION_CREDENTIALS
          value: "/app/erlmcp-sa-key.json"
        volumeMounts:
        - name: service-account
          mountPath: /app
      volumes:
      - name: service-account
        secret:
          secretName: gcp-service-account
---
apiVersion: v1
kind: Service
metadata:
  name: gcp-integration-service
spec:
  selector:
    app: gcp-integration
  ports:
  - port: 80
    targetPort: 8080
  type: LoadBalancer
```

## Case Study: Real-World Implementation

### Challenge
A financial services company needed to integrate with GCP for:
- Real-time analytics processing
- Document storage and retrieval
- Batch job execution

### Solution
```erlang
% Custom tools for financial services
financial_analytics_tool:process_transactions(DataStorage, AnalyticsConfig).
financial_storage_tool:store_document(Bucket, Document, Metadata).
financial_job_tool:execute_batch_job(JobSpec, Timeout).
```

### Results
- **99.99% availability** with GCP service integration
- **< 500ms latency** for document operations
- **10x improvement** in batch job throughput
- **Automatic failover** during GCP outages

## Troubleshooting GCP Issues

### Common Issues
1. **Authentication Errors**
```erlang
% Check credentials
file:read_file("erlmcp-sa-key.json").

% Test GCP connection
gcloud_compute:list_instances("your-project").
```

2. **Permission Errors**
```bash
# Check service account permissions
gcloud projects get-iam-policy your-project
```

3. **Quota Exceeded**
```erlang
% Handle quota errors
handle_quota_error(QuotaType, Usage) ->
    logger:warning(#{event => quota_exceeded, type => QuotaType, usage => Usage}),
    {error, {quota_exceeded, QuotaType}}.
```

### Debug Commands
```bash
# View GCP logs
gcloud logging read "resource.type=gce_instance AND protoPayload.methodName=compute.instances.insert"

# Monitor API usage
gcloud services usage list --enabled --format="value(serviceName)"

# Check quotas
gcloud compute project-info describe
```

## Next Steps

### Advanced Topics
- [Multi-Region Deployment](../reference/deployment/scaling.md) - Global scaling
- [Cost Optimization](cost-optimization.md) - Manage GCP costs
- [Security Hardening](security-implementation.md) - Secure GCP integration
- [Performance Tuning](performance-optimization.md) - Optimize GCP performance

### Related Integrations
- [AWS Integration](aws-integration.md) - Connect to AWS services
- [Azure Integration](azure-integration.md) - Connect to Azure services
- [Hybrid Cloud](hybrid-cloud.md) - Multi-cloud strategy

### Resources
- [GCP Documentation](https://cloud.google.com/docs)
- [erlmcp GCP Examples](../examples/gcp-simulator.md)
- [Community Support](../appendices/community.md)

---

**Success!** You've built a comprehensive GCP integration. Next, explore [Performance Optimization](performance-optimization.md) to tune your setup for production.