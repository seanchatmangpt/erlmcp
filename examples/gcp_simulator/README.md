# GCP Simulator MCP Server

A comprehensive Google Cloud Platform (GCP) simulator implemented as an MCP (Model Context Protocol) server using erlmcp. This simulator provides mock implementations of common GCP services for testing, development, and educational purposes.

## Features

### Supported GCP Services

1. **Compute Engine** - Virtual machine instances
   - Create, list, get, start, stop, delete instances
   - Mock IP addresses and instance metadata

2. **Cloud Storage** - Object storage
   - Create buckets
   - Upload, download, list, delete objects
   - Multi-region support

3. **Cloud Functions** - Serverless functions
   - Deploy, list, invoke, delete functions
   - Support for multiple runtimes (Python, Node.js, etc.)

4. **Cloud SQL** - Managed database instances
   - Create, list, delete database instances
   - Support for PostgreSQL and MySQL

5. **Cloud Pub/Sub** - Messaging service
   - Create topics and subscriptions
   - Publish messages

6. **IAM** - Identity and Access Management
   - Create and list service accounts
   - Generate service account emails

## Getting Started

### Prerequisites

- Erlang/OTP 25+
- rebar3
- erlmcp installed and configured

### Running the Server

```bash
# From the erlmcp root directory
rebar3 compile

# Start the GCP simulator
rebar3 shell
> gcp_simulator_server:start().
```

The server will start in stdio mode and be ready to accept MCP commands.

### Available Tools

#### Compute Engine

- `gcp_compute_create_instance` - Create a new VM instance
  ```json
  {
    "name": "web-server-1",
    "machine_type": "e2-micro",
    "zone": "us-central1-a"
  }
  ```

- `gcp_compute_list_instances` - List all instances
- `gcp_compute_get_instance` - Get instance details
- `gcp_compute_start_instance` - Start a stopped instance
- `gcp_compute_stop_instance` - Stop a running instance
- `gcp_compute_delete_instance` - Delete an instance

#### Cloud Storage

- `gcp_storage_create_bucket` - Create a new bucket
  ```json
  {
    "name": "my-app-assets",
    "location": "US"
  }
  ```

- `gcp_storage_list_buckets` - List all buckets
- `gcp_storage_upload_object` - Upload an object to a bucket
  ```json
  {
    "bucket": "my-app-assets",
    "object_name": "index.html",
    "content": "<html>Hello World</html>"
  }
  ```

- `gcp_storage_list_objects` - List objects in a bucket
- `gcp_storage_download_object` - Download an object
- `gcp_storage_delete_object` - Delete an object

#### Cloud Functions

- `gcp_functions_deploy` - Deploy a new function
  ```json
  {
    "name": "process-upload",
    "runtime": "python39",
    "entry_point": "main"
  }
  ```

- `gcp_functions_list` - List all functions
- `gcp_functions_invoke` - Invoke a function
  ```json
  {
    "name": "process-upload",
    "data": "{\"file\": \"data.csv\"}"
  }
  ```

- `gcp_functions_delete` - Delete a function

#### Cloud SQL

- `gcp_sql_create_instance` - Create a database instance
  ```json
  {
    "name": "app-database",
    "database_version": "POSTGRES_14",
    "tier": "db-f1-micro"
  }
  ```

- `gcp_sql_list_instances` - List all database instances
- `gcp_sql_delete_instance` - Delete a database instance

#### Cloud Pub/Sub

- `gcp_pubsub_create_topic` - Create a topic
  ```json
  {
    "name": "order-events"
  }
  ```

- `gcp_pubsub_list_topics` - List all topics
- `gcp_pubsub_publish` - Publish a message to a topic
  ```json
  {
    "topic": "order-events",
    "message": "{\"order_id\": 12345, \"status\": \"completed\"}"
  }
  ```

- `gcp_pubsub_create_subscription` - Create a subscription
  ```json
  {
    "name": "order-processor-sub",
    "topic": "order-events"
  }
  ```

#### IAM

- `gcp_iam_create_service_account` - Create a service account
  ```json
  {
    "name": "app-service-account",
    "display_name": "Application Service Account"
  }
  ```

- `gcp_iam_list_service_accounts` - List all service accounts

### Resources

The simulator provides several resources for inspection:

- `gcp://status` - View simulator status and resource counts
- `gcp://help` - View complete help documentation

### Prompts

- `gcp_deploy_web_app` - Generate a deployment plan for a web application
  ```json
  {
    "app_name": "my-web-app"
  }
  ```

## Example Workflow

Here's a complete example of deploying a web application infrastructure:

```erlang
%% 1. Create a compute instance for the web server
gcp_compute_create_instance(<<"web-server">>, <<"n1-standard-1">>, <<"us-central1-a">>).

%% 2. Create a storage bucket for static assets
gcp_storage_create_bucket(<<"my-app-static">>, <<"US">>).

%% 3. Upload static files
gcp_storage_upload_object(<<"my-app-static">>, <<"index.html">>, <<"<html>...</html>">>).

%% 4. Create a database instance
gcp_sql_create_instance(<<"app-db">>, <<"POSTGRES_14">>, <<"db-n1-standard-1">>).

%% 5. Deploy a cloud function for API endpoints
gcp_functions_deploy(<<"api-handler">>, <<"nodejs18">>, <<"handler">>).

%% 6. Create a Pub/Sub topic for events
gcp_pubsub_create_topic(<<"app-events">>).

%% 7. Create a service account
gcp_iam_create_service_account(<<"app-sa">>, <<"App Service Account">>).

%% 8. Check the status
gcp://status
```

## Architecture

The simulator uses ETS (Erlang Term Storage) tables to maintain state:

- `gcp_compute_instances` - VM instances
- `gcp_storage_buckets` - Storage buckets
- `gcp_storage_objects` - Stored objects
- `gcp_cloud_functions` - Deployed functions
- `gcp_cloud_sql` - Database instances
- `gcp_pubsub_topics` - Pub/Sub topics
- `gcp_pubsub_subscriptions` - Subscriptions
- `gcp_iam_service_accounts` - Service accounts

All operations are in-memory and non-persistent. State is reset when the server is restarted.

## Testing

The simulator includes comprehensive EUnit tests:

```bash
# Run all GCP simulator tests
rebar3 eunit --module=gcp_simulator_tests

# Run specific test
rebar3 eunit --module=gcp_simulator_tests --test=compute_create_instance_test_
```

## Use Cases

1. **Development** - Test GCP-dependent applications without real GCP credentials
2. **CI/CD** - Run integration tests in CI pipelines without external dependencies
3. **Education** - Learn GCP APIs and services in a safe environment
4. **Prototyping** - Quickly prototype GCP architectures before deployment
5. **Cost Savings** - Avoid GCP charges during development and testing

## Limitations

- All data is stored in-memory (not persistent)
- No authentication or authorization checks
- Simplified resource models (not 100% API-compatible with real GCP)
- No network calls to actual GCP services
- No quota or billing simulation
- Limited to simulated operations (no actual compute, storage, etc.)

## Future Enhancements

Potential additions for future versions:

- [ ] More GCP services (Cloud Run, GKE, BigQuery, etc.)
- [ ] Persistent state storage (optional)
- [ ] IAM policy simulation
- [ ] VPC and networking simulation
- [ ] Billing and quota tracking
- [ ] More realistic error conditions
- [ ] Resource relationships and dependencies
- [ ] Import/export state for testing scenarios

## Contributing

Contributions are welcome! Please ensure:

1. All new features include EUnit tests
2. Code follows erlmcp OTP patterns
3. Documentation is updated
4. Quality gates pass (compile, tests, dialyzer)

## License

Same license as erlmcp parent project.

## Support

For issues or questions:
- Check the main erlmcp documentation
- Review test files for usage examples
- Use the `gcp://help` resource for command reference
