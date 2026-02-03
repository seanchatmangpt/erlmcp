%% @doc Enterprise Integration Test Suite
%% Comprehensive integration tests for all enterprise adapters
-module(erlmcp_enterprise_integration_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([
    %% Identity provider tests
    test_okta_integration/1,
    test_azure_ad_integration/1,
    test_adfs_integration/1,

    %% Monitoring system tests
    test_splunk_integration/1,
    test_datadog_integration/1,
    test_new_relic_integration/1,

    %% Logging platform tests
    test_elasticsearch_integration/1,
    test_graylog_integration/1,
    test_sumo_logic_integration/1,

    %% Business intelligence tests
    test_tableau_integration/1,
    test_power_bi_integration/1,

    %% Service bus tests
    test_kafka_integration/1,
    test_esb_integration/1,

    %% Data warehouse tests
    test_snowflake_integration/1,
    test_bigquery_integration/1,

    %% DevOps tests
    test_jenkins_integration/1,
    test_gitlab_ci_integration/1,

    %% API gateway tests
    test_kong_integration/1,

    %% Cloud platform tests
    test_aws_integration/1,,

    %% Security system tests
    test_siem_integration/1,

    %% Configuration management tests
    test_ansible_integration/1,

    %% Container orchestration tests
    test_kubernetes_integration/1,

    %% Integration orchestration tests
    test_enterprise_bus/1,,

    %% Performance tests
    test_concurrent_connections/1,,

    %% Security tests
    test_authentication/1,,

    %% Compliance tests
    test_audit_logging/1]).

-include_lib("common_test/include/ct.hrl").

%%====================================================================
%% Test suite setup
%%====================================================================

-spec all() -> [test_name()].
all() ->
    [
        %% Identity providers
        test_okta_integration,
        test_azure_ad_integration,
        test_adfs_integration,

        %% Monitoring systems
        test_splunk_integration,
        test_datadog_integration,
        test_new_relic_integration,

        %% Logging platforms
        test_elasticsearch_integration,
        test_graylog_integration,
        test_sumo_logic_integration,

        %% Business intelligence
        test_tableau_integration,
        test_power_bi_integration,

        %% Service buses
        test_kafka_integration,
        test_esb_integration,

        %% Data warehouses
        test_snowflake_integration,
        test_bigquery_integration,

        %% DevOps tools
        test_jenkins_integration,
        test_gitlab_ci_integration,

        %% API gateways
        test_kong_integration,
        test_apigee_integration,

        %% Cloud platforms
        test_aws_integration,
        test_azure_integration,
        test_gcp_integration,

        %% Security systems
        test_siem_integration,

        %% Configuration management
        test_ansible_integration,
        test_puppet_integration,

        %% Container orchestration
        test_kubernetes_integration,
        test_swarm_integration,

        %% Integration orchestration
        test_enterprise_bus,
        test_enterprise_registry,
        test_enterprise_metrics,
        test_enterprise_health,

        %% Performance tests
        test_concurrent_connections,
        test_load_balancing,
        test_failover,

        %% Security tests
        test_authentication,
        test_authorization,
        test_data_encryption,

        %% Compliance tests
        test_audit_logging,
        test_data_retention,
        test_regulatory_compliance
    ].

-spec init_per_suite(Config :: config()) -> config().
init_per_suite(Config) ->
    %% Start enterprise application
    {ok, _Apps} = application:ensure_all_started(erlmcp_enterprise),

    %% Set up test environment
    Config1 = [{test_env, enterprise} | Config],

    %% Initialize test data
    TestData = init_test_data(),
    Config2 = [{test_data, TestData} | Config1],

    %% Set up monitoring for tests
    ct:pal("Starting enterprise integration test suite"),

    Config2.

-spec end_per_suite(Config :: config()) -> term().
end_per_suite(Config) ->
    %% Stop enterprise application
    application:stop(erlmcp_enterprise),

    %% Clean up test data
    cleanup_test_data(proplists:get_value(test_data, Config)),

    ct:pal("Enterprise integration test suite completed"),
    ok.

%%====================================================================
%% Identity provider tests
%%====================================================================

-spec test_okta_integration(Config :: config()) -> term().
test_okta_integration(Config) ->
    ct:pal("Testing Okta integration"),

    %% Get test configuration
    TestConfig = get_test_config(okta, Config),

    %% Start Okta adapter
    {ok, _Pid} = erlmcp_identity_adapter:start_link(TestConfig),

    %% Test authentication
    TestToken = generate_test_token(),
    case erlmcp_identity_adapter:authenticate(TestToken) of
        {ok, User} ->
            ct:pal("Okta authentication successful: ~p", [User]);
        {error, Reason} ->
            ct:fail("Okta authentication failed: ~p", [Reason])
    end,

    %% Test user lookup
    UserId = <<"test_user">>,
    case erlmcp_identity_adapter:get_user(UserId) of
        {ok, UserInfo} ->
            ct:pal("Okta user lookup successful: ~p", [UserInfo]);
        {error, Reason} ->
            ct:fail("Okta user lookup failed: ~p", [Reason])
    end,

    %% Test group lookup
    case erlmcp_identity_adapter:get_groups(UserId) of
        {ok, Groups} ->
            ct:pal("Okta groups lookup successful: ~p", [Groups]);
        {error, Reason} ->
            ct:fail("Okta groups lookup failed: ~p", [Reason])
    end,

    %% Stop adapter
    erlmcp_identity_adapter:stop(okta_adapter),

    ct:pal("Okta integration test completed").

-spec test_azure_ad_integration(Config :: config()) -> term().
test_azure_ad_integration(Config) ->
    ct:pal("Testing Azure AD integration"),

    %% Get test configuration
    TestConfig = get_test_config(azure_ad, Config),

    %% Start Azure AD adapter
    {ok, _Pid} = erlmcp_identity_adapter:start_link(TestConfig),

    %% Test authentication
    TestToken = generate_test_token(),
    case erlmcp_identity_adapter:authenticate(TestToken) of
        {ok, User} ->
            ct:pal("Azure AD authentication successful: ~p", [User]);
        {error, Reason} ->
            ct:fail("Azure AD authentication failed: ~p", [Reason])
    end,

    %% Test applications lookup
    case erlmcp_identity_adapter:get_applications() of
        {ok, Apps} ->
            ct:pal("Azure AD applications lookup successful: ~p", [Apps]);
        {error, Reason} ->
            ct:fail("Azure AD applications lookup failed: ~p", [Reason])
    end,

    %% Stop adapter
    erlmcp_identity_adapter:stop(azure_ad_adapter),

    ct:pal("Azure AD integration test completed").

-spec test_adfs_integration(Config :: config()) -> term().
test_adfs_integration(Config) ->
    ct:pal("Testing ADFS integration"),

    %% Get test configuration
    TestConfig = get_test_config(adfs, Config),

    %% Start ADFS adapter
    {ok, _Pid} = erlmcp_identity_adapter:start_link(TestConfig),

    %% Test authentication
    TestToken = generate_test_token(),
    case erlmcp_identity_adapter:authenticate(TestToken) of
        {ok, User} ->
            ct:pal("ADFS authentication successful: ~p", [User]);
        {error, Reason} ->
            ct:fail("ADFS authentication failed: ~p", [Reason])
    end,

    %% Stop adapter
    erlmcp_identity_adapter:stop(adfs_adapter),

    ct:pal("ADFS integration test completed").

%%====================================================================
%% Monitoring system tests
%%====================================================================

-spec test_splunk_integration(Config :: config()) -> term().
test_splunk_integration(Config) ->
    ct:pal("Testing Splunk integration"),

    %% Get test configuration
    TestConfig = get_test_config(splunk, Config),

    %% Start Splunk adapter
    {ok, _Pid} = erlmcp_monitoring_adapter:start_link(TestConfig),

    %% Test metric sending
    TestMetric = #{
        name => "test_metric",
        value => 42,
        type => "gauge",
        tags => #{
            env => "test",
            service => "erlmcp"
        }
    },
    case erlmcp_monitoring_adapter:send_metric(TestMetric) of
        ok ->
            ct:pal("Splunk metric send successful");
        {error, Reason} ->
            ct:fail("Splunk metric send failed: ~p", [Reason])
    end,

    %% Test log sending
    TestLog = #{
        message => "Test log message",
        level => "info",
        timestamp => erlang:system_time(millisecond),
        service => "erlmcp"
    },
    case erlmcp_monitoring_adapter:send_log(TestLog) of
        ok ->
            ct:pal("Splunk log send successful");
        {error, Reason} ->
            ct:fail("Splunk log send failed: ~p", [Reason])
    end,

    %% Stop adapter
    erlmcp_monitoring_adapter:stop(splunk_adapter),

    ct:pal("Splunk integration test completed").

-spec test_datadog_integration(Config :: config()) -> term().
test_datadog_integration(Config) ->
    ct:pal("Testing Datadog integration"),

    %% Get test configuration
    TestConfig = get_test_config(datadog, Config),

    %% Start Datadog adapter
    {ok, _Pid} = erlmcp_monitoring_adapter:start_link(TestConfig),

    %% Test metric sending
    TestMetric = #{
        name => "test_metric",
        value => 42,
        type => "gauge",
        tags => #{
            env => "test",
            service => "erlmcp"
        }
    },
    case erlmcp_monitoring_adapter:send_metric(TestMetric) of
        ok ->
            ct:pal("Datadog metric send successful");
        {error, Reason} ->
            ct:fail("Datadog metric send failed: ~p", [Reason])
    end,

    %% Stop adapter
    erlmcp_monitoring_adapter:stop(datadog_adapter),

    ct:pal("Datadog integration test completed").

-spec test_new_relic_integration(Config :: config()) -> term().
test_new_relic_integration(Config) ->
    ct:pal("Testing New Relic integration"),

    %% Get test configuration
    TestConfig = get_test_config(new_relic, Config),

    %% Start New Relic adapter
    {ok, _Pid} = erlmcp_monitoring_adapter:start_link(TestConfig),

    %% Test metric sending
    TestMetric = #{
        name => "test_metric",
        value => 42,
        type => "gauge",
        tags => #{
            env => "test",
            service => "erlmcp"
        }
    },
    case erlmcp_monitoring_adapter:send_metric(TestMetric) of
        ok ->
            ct:pal("New Relic metric send successful");
        {error, Reason} ->
            ct:fail("New Relic metric send failed: ~p", [Reason])
    end,

    %% Stop adapter
    erlmcp_monitoring_adapter:stop(new_relic_adapter),

    ct:pal("New Relic integration test completed").

%%====================================================================
%% Logging platform tests
%%====================================================================

-spec test_elasticsearch_integration(Config :: config()) -> term().
test_elasticsearch_integration(Config) ->
    ct:pal("Testing Elasticsearch integration"),

    %% Get test configuration
    TestConfig = get_test_config(elasticsearch, Config),

    %% Start Elasticsearch adapter
    {ok, _Pid} = erlmcp_logging_adapter:start_link(TestConfig),

    %% Test log event
    TestEvent = #{
        message => "Test log event",
        level => "info",
        timestamp => erlang:system_time(millisecond),
        service => "erlmcp",
        env => "test"
    },
    case erlmcp_logging_adapter:log_event(TestEvent) of
        ok ->
            ct:pal("Elasticsearch log event successful");
        {error, Reason} ->
            ct:fail("Elasticsearch log event failed: ~p", [Reason])
    end,

    %% Test index creation
    TestIndexConfig = #{
        name => "test-index",
        mappings => #{
            properties => #{
                message => #{type => "text"},
                timestamp => #{type => "date"}
            }
        }
    },
    case erlmcp_logging_adapter:create_index(TestIndexConfig) of
        ok ->
            ct:pal("Elasticsearch index creation successful");
        {error, Reason} ->
            ct:fail("Elasticsearch index creation failed: ~p", [Reason])
    end,

    %% Stop adapter
    erlmcp_logging_adapter:stop(elasticsearch_adapter),

    ct:pal("Elasticsearch integration test completed").

-spec test_graylog_integration(Config :: config()) -> term().
test_graylog_integration(Config) ->
    ct:pal("Testing Graylog integration"),

    %% Get test configuration
    TestConfig = get_test_config(graylog, Config),

    %% Start Graylog adapter
    {ok, _Pid} = erlmcp_logging_adapter:start_link(TestConfig),

    %% Test log event
    TestEvent = #{
        message => "Test log event",
        level => "info",
        timestamp => erlang:system_time(millisecond),
        service => "erlmcp"
    },
    case erlmcp_logging_adapter:log_event(TestEvent) of
        ok ->
            ct:pal("Graylog log event successful");
        {error, Reason} ->
            ct:fail("Graylog log event failed: ~p", [Reason])
    end,

    %% Stop adapter
    erlmcp_logging_adapter:stop(graylog_adapter),

    ct:pal("Graylog integration test completed").

-spec test_sumo_logic_integration(Config :: config()) -> term().
test_sumo_logic_integration(Config) ->
    ct:pal("Testing Sumo Logic integration"),

    %% Get test configuration
    TestConfig = get_test_config(sumo_logic, Config),

    %% Start Sumo Logic adapter
    {ok, _Pid} = erlmcp_logging_adapter:start_link(TestConfig),

    %% Test log event
    TestEvent = #{
        message => "Test log event",
        level => "info",
        timestamp => erlang:system_time(millisecond),
        service => "erlmcp"
    },
    case erlmcp_logging_adapter:log_event(TestEvent) of
        ok ->
            ct:pal("Sumo Logic log event successful");
        {error, Reason} ->
            ct:fail("Sumo Logic log event failed: ~p", [Reason])
    end,

    %% Stop adapter
    erlmcp_logging_adapter:stop(sumo_logic_adapter),

    ct:pal("Sumo Logic integration test completed").

%%====================================================================
%% Business intelligence tests
%%====================================================================

-spec test_tableau_integration(Config :: config()) -> term().
test_tableau_integration(Config) ->
    ct:pal("Testing Tableau integration"),

    %% Get test configuration
    TestConfig = get_test_config(tableau, Config),

    %% Start Tableau adapter
    {ok, _Pid} = erlmcp_bizintel_adapter:start_link(TestConfig),

    %% Test workbook lookup
    TestWorkbookId = <<"test-workbook">>,
    case erlmcp_bizintel_adapter:get_workbook(TestWorkbookId) of
        {ok, Workbook} ->
            ct:pal("Tableau workbook lookup successful: ~p", [Workbook]);
        {error, Reason} ->
            ct:fail("Tableau workbook lookup failed: ~p", [Reason])
    end,

    %% Test data extraction
    TestExtractConfig = #{
        workbook_id => TestWorkbookId,
        format => "csv"
    },
    case erlmcp_bizintel_adapter:extract_data(TestExtractConfig) of
        {ok, Data} ->
            ct:pal("Tableau data extraction successful");
        {error, Reason} ->
            ct:fail("Tableau data extraction failed: ~p", [Reason])
    end,

    %% Stop adapter
    erlmcp_bizintel_adapter:stop(tableau_adapter),

    ct:pal("Tableau integration test completed").

-spec test_power_bi_integration(Config :: config()) -> term().
test_power_bi_integration(Config) ->
    ct:pal("Testing Power BI integration"),

    %% Get test configuration
    TestConfig = get_test_config(power_bi, Config),

    %% Start Power BI adapter
    {ok, _Pid} = erlmcp_bizintel_adapter:start_link(TestConfig),

    %% Test report lookup
    TestReportId = <<"test-report">>,
    case erlmcp_bizintel_adapter:get_workbook(TestReportId) of
        {ok, Report} ->
            ct:pal("Power BI report lookup successful: ~p", [Report]);
        {error, Reason} ->
            ct:fail("Power BI report lookup failed: ~p", [Reason])
    end,

    %% Test query execution
    TestQueryConfig = #{
        report_id => TestReportId,
        query => "SELECT * FROM FactSales"
    },
    case erlmcp_bizintel_adapter:extract_data(TestQueryConfig) of
        {ok, Data} ->
            ct:pal("Power BI query execution successful");
        {error, Reason} ->
            ct:fail("Power BI query execution failed: ~p", [Reason])
    end,

    %% Stop adapter
    erlmcp_bizintel_adapter:stop(power_bi_adapter),

    ct:pal("Power BI integration test completed").

%%====================================================================
%% Service bus tests
%%====================================================================

-spec test_kafka_integration(Config :: config()) -> term().
test_kafka_integration(Config) ->
    ct:pal("Testing Kafka integration"),

    %% Get test configuration
    TestConfig = get_test_config(kafka, Config),

    %% Start Kafka adapter
    {ok, _Pid} = erlmcp_servicebus_adapter:start_link(TestConfig),

    %% Test message sending
    TestMessage = #{
        key => <<"test-key">>,
        value => <<"test-value">>,
        headers => #{
            source => "erlmcp"
        }
    },
    case erlmcp_servicebus_adapter:send_message(TestMessage) of
        ok ->
            ct:pal("Kafka message send successful");
        {error, Reason} ->
            ct:fail("Kafka message send failed: ~p", [Reason])
    end,

    %% Test topic publishing
    TestTopic = <<"test-topic">>,
    case erlmcp_servicebus_adapter:publish_message(TestTopic, TestMessage) of
        ok ->
            ct:pal("Kafka topic publish successful");
        {error, Reason} ->
            ct:fail("Kafka topic publish failed: ~p", [Reason])
    end,

    %% Stop adapter
    erlmcp_servicebus_adapter:stop(kafka_adapter),

    ct:pal("Kafka integration test completed").

-spec test_esb_integration(Config :: config()) -> term().
test_esb_integration(Config) ->
    ct:pal("Testing ESB integration"),

    %% Get test configuration
    TestConfig = get_test_config(esb, Config),

    %% Start ESB adapter
    {ok, _Pid} = erlmcp_servicebus_adapter:start_link(TestConfig),

    %% Test message sending
    TestMessage = #{
        payload => <<"test-payload">>,
        headers => #{
            source => "erlmcp",
            destination => "test-service"
        }
    },
    case erlmcp_servicebus_adapter:send_message(TestMessage) of
        ok ->
            ct:pal("ESB message send successful");
        {error, Reason} ->
            ct:fail("ESB message send failed: ~p", [Reason])
    end,

    %% Stop adapter
    erlmcp_servicebus_adapter:stop(esb_adapter),

    ct:pal("ESB integration test completed").

%%====================================================================
%% Data warehouse tests
%%====================================================================

-spec test_snowflake_integration(Config :: config()) -> term().
test_snowflake_integration(Config) ->
    ct:pal("Testing Snowflake integration"),

    %% Get test configuration
    TestConfig = get_test_config(snowflake, Config),

    %% Start Snowflake adapter
    {ok, _Pid} = erlmcp_data_adapter:start_link(TestConfig),

    %% Test query execution
    TestQuery = #{
        sql => "SELECT 1 as test_column",
        database => "TEST_DB",
        schema => "TEST_SCHEMA"
    },
    case erlmcp_data_adapter:execute_query(TestQuery) of
        {ok, Results} ->
            ct:pal("Snowflake query execution successful: ~p", [Results]);
        {error, Reason} ->
            ct:fail("Snowflake query execution failed: ~p", [Reason])
    end,

    %% Test table creation
    TestTableConfig = #{
        name => "test_table",
        columns => [
            {id, "INTEGER PRIMARY KEY"},
            {name, "VARCHAR(100)"},
            {created_at, "TIMESTAMP"}
        ]
    },
    case erlmcp_data_adapter:create_table(TestTableConfig) of
        ok ->
            ct:pal("Snowflake table creation successful");
        {error, Reason} ->
            ct:fail("Snowflake table creation failed: ~p", [Reason])
    end,

    %% Stop adapter
    erlmcp_data_adapter:stop(snowflake_adapter),

    ct:pal("Snowflake integration test completed").

-spec test_bigquery_integration(Config :: config()) -> term().
test_bigquery_integration(Config) ->
    ct:pal("Testing BigQuery integration"),

    %% Get test configuration
    TestConfig = get_test_config(bigquery, Config),

    %% Start BigQuery adapter
    {ok, _Pid} = erlmcp_data_adapter:start_link(TestConfig),

    %% Test query execution
    TestQuery = #{
        sql => "SELECT 1 as test_column",
        dataset => "TEST_DATASET"
    },
    case erlmcp_data_adapter:execute_query(TestQuery) of
        {ok, Results} ->
            ct:pal("BigQuery query execution successful: ~p", [Results]);
        {error, Reason} ->
            ct:fail("BigQuery query execution failed: ~p", [Reason])
    end,

    %% Stop adapter
    erlmcp_data_adapter:stop(bigquery_adapter),

    ct:pal("BigQuery integration test completed").

%%====================================================================
%% DevOps tests
%%====================================================================

-spec test_jenkins_integration(Config :: config()) -> term().
test_jenkins_integration(Config) ->
    ct:pal("Testing Jenkins integration"),

    %% Get test configuration
    TestConfig = get_test_config(jenkins, Config),

    %% Start Jenkins adapter
    {ok, _Pid} = erlmcp_devops_adapter:start_link(TestConfig),

    %% Test pipeline start
    TestPipelineId = <<"test-pipeline">>,
    TestParameters = #{
        branch => "main",
        environment => "test"
    },
    case erlmcp_devops_adapter:start_pipeline(TestPipelineId, TestParameters) of
        {ok, BuildId} ->
            ct:pal("Jenkins pipeline start successful: ~p", [BuildId]);
        {error, Reason} ->
            ct:fail("Jenkins pipeline start failed: ~p", [Reason])
    end,

    %% Test build status lookup
    case erlmcp_devops_adapter:get_build_status(BuildId) of
        {ok, Status} ->
            ct:pal("Jenkins build status lookup successful: ~p", [Status]);
        {error, Reason} ->
            ct:fail("Jenkins build status lookup failed: ~p", [Reason])
    end,

    %% Stop adapter
    erlmcp_devops_adapter:stop(jenkins_adapter),

    ct:pal("Jenkins integration test completed").

-spec test_gitlab_ci_integration(Config :: config()) -> term().
test_gitlab_ci_integration(Config) ->
    ct:pal("Testing GitLab CI integration"),

    %% Get test configuration
    TestConfig = get_test_config(gitlab_ci, Config),

    %% Start GitLab CI adapter
    {ok, _Pid} = erlmcp_devops_adapter:start_link(TestConfig),

    %% Test pipeline start
    TestPipelineId = <<"test-pipeline">>,
    TestParameters = #{
        ref => "main",
        variables => #{
            ENV => "test"
        }
    },
    case erlmcp_devops_adapter:start_pipeline(TestPipelineId, TestParameters) of
        {ok, BuildId} ->
            ct:pal("GitLab CI pipeline start successful: ~p", [BuildId]);
        {error, Reason} ->
            ct:fail("GitLab CI pipeline start failed: ~p", [Reason])
    end,

    %% Stop adapter
    erlmcp_devops_adapter:stop(gitlab_ci_adapter),

    ct:pal("GitLab CI integration test completed").

%%====================================================================
%% API gateway tests
%%====================================================================

-spec test_kong_integration(Config :: config()) -> term().
test_kong_integration(Config) ->
    ct:pal("Testing Kong integration"),

    %% Get test configuration
    TestConfig = get_test_config(kong, Config),

    %% Start Kong adapter
    {ok, _Pid} = erlmcp_apigw_adapter:start_link(TestConfig),

    %% Test API creation
    TestApiConfig = #{
        name => "test-api",
        uris => ["/test"],
        upstream_url => "http://localhost:8080"
    },
    case erlmcp_apigw_adapter:create_api(TestApiConfig) of
        {ok, ApiId} ->
            ct:pal("Kong API creation successful: ~p", [ApiId]);
        {error, Reason} ->
            ct:fail("Kong API creation failed: ~p", [Reason])
    end,

    %% Test route creation
    TestRouteConfig = #{
        paths => ["/test/*"],
        methods => ["GET", "POST"]
    },
    case erlmcp_apigw_adapter:create_route(ApiId, TestRouteConfig) of
        {ok, RouteId} ->
            ct:pal("Kong route creation successful: ~p", [RouteId]);
        {error, Reason} ->
            ct:fail("Kong route creation failed: ~p", [Reason])
    end,

    %% Stop adapter
    erlmcp_apigw_adapter:stop(kong_adapter),

    ct:pal("Kong integration test completed").

-spec test_apigee_integration(Config :: config()) -> term().
test_apigee_integration(Config) ->
    ct:pal("Testing Apigee integration"),

    %% Get test configuration
    TestConfig = get_test_config(apigee, Config),

    %% Start Apigee adapter
    {ok, _Pid} = erlmcp_apigw_adapter:start_link(TestConfig),

    %% Test API creation
    TestApiConfig = #{
        name => "test-api",
        proxy => "test-proxy",
        basepath => "/test"
    },
    case erlmcp_apigw_adapter:create_api(TestApiConfig) of
        {ok, ApiId} ->
            ct:pal("Apigee API creation successful: ~p", [ApiId]);
        {error, Reason} ->
            ct:fail("Apigee API creation failed: ~p", [Reason])
    end,

    %% Stop adapter
    erlmcp_apigw_adapter:stop(apigee_adapter),

    ct:pal("Apigee integration test completed").

%%====================================================================
%% Cloud platform tests
%%====================================================================

-spec test_aws_integration(Config :: config()) -> term().
test_aws_integration(Config) ->
    ct:pal("Testing AWS integration"),

    %% Get test configuration
    TestConfig = get_test_config(aws, Config),

    %% Start AWS adapter
    {ok, _Pid} = erlmcp_cloud_adapter:start_link(TestConfig),

    %% Test EC2 instance creation
    TestResourceConfig = #{
        type => "ec2",
        image_id => "ami-12345678",
        instance_type => "t2.micro",
        region => "us-east-1"
    },
    case erlmcp_cloud_adapter:create_resource(TestResourceConfig) of
        {ok, ResourceId} ->
            ct:pal("AWS EC2 instance creation successful: ~p", [ResourceId]);
        {error, Reason} ->
            ct:fail("AWS EC2 instance creation failed: ~p", [Reason])
    end,

    %% Test S3 bucket creation
    S3Config = #{
        type => "s3",
        bucket_name => "test-bucket-12345",
        region => "us-east-1"
    },
    case erlmcp_cloud_adapter:create_resource(S3Config) of
        {ok, ResourceId} ->
            ct:pal("AWS S3 bucket creation successful: ~p", [ResourceId]);
        {error, Reason} ->
            ct:fail("AWS S3 bucket creation failed: ~p", [Reason])
    end,

    %% Stop adapter
    erlmcp_cloud_adapter:stop(cloud_adapter),

    ct:pal("AWS integration test completed").

-spec test_azure_integration(Config :: config()) -> term().
test_azure_integration(Config) ->
    ct:pal("Testing Azure integration"),

    %% Get test configuration
    TestConfig = get_test_config(azure, Config),

    %% Start Azure adapter
    {ok, _Pid} = erlmcp_cloud_adapter:start_link(TestConfig),

    %% Test VM creation
    TestResourceConfig = #{
        type => "virtual_machine",
        name => "test-vm",
        location => "East US",
        size => "Standard_B1s"
    },
    case erlmcp_cloud_adapter:create_resource(TestResourceConfig) of
        {ok, ResourceId} ->
            ct:pal("Azure VM creation successful: ~p", [ResourceId]);
        {error, Reason} ->
            ct:fail("Azure VM creation failed: ~p", [Reason])
    end,

    %% Stop adapter
    erlmcp_cloud_adapter:stop(cloud_adapter),

    ct:pal("Azure integration test completed").

-spec test_gcp_integration(Config :: config()) -> term().
test_gcp_integration(Config) ->
    ct:pal("Testing GCP integration"),

    %% Get test configuration
    TestConfig = get_test_config(gcp, Config),

    %% Start GCP adapter
    {ok, _Pid} = erlmcp_cloud_adapter:start_link(TestConfig),

    %% Test VM creation
    TestResourceConfig = #{
        type => "compute_instance",
        name => "test-vm",
        zone => "us-central1-a",
        machine_type => "e2-micro"
    },
    case erlmcp_cloud_adapter:create_resource(TestResourceConfig) of
        {ok, ResourceId} ->
            ct:pal("GCP VM creation successful: ~p", [ResourceId]);
        {error, Reason} ->
            ct:fail("GCP VM creation failed: ~p", [Reason])
    end,

    %% Stop adapter
    erlmcp_cloud_adapter:stop(cloud_adapter),

    ct:pal("GCP integration test completed").

%%====================================================================
%% Security system tests
%%====================================================================

-spec test_siem_integration(Config :: config()) -> term().
test_siem_integration(Config) ->
    ct:pal("Testing SIEM integration"),

    %% Get test configuration
    TestConfig = get_test_config(siem, Config),

    %% Start SIEM adapter
    {ok, _Pid} = erlmcp_security_adapter:start_link(TestConfig),

    %% Test security event
    TestEvent = #{
        timestamp => erlang:system_time(millisecond),
        event_type => "login_attempt",
        severity => "high",
        source_ip => "192.168.1.100",
        username => "test_user",
        result => "failure",
        details => #{
            reason => "invalid_credentials"
        }
    },
    case erlmcp_security_adapter:send_event(TestEvent) of
        ok ->
            ct:pal("SIEM event send successful");
        {error, Reason} ->
            ct:fail("SIEM event send failed: ~p", [Reason])
    end,

    %% Stop adapter
    erlmcp_security_adapter:stop(siem_adapter),

    ct:pal("SIEM integration test completed").

%%====================================================================
%% Configuration management tests
%%====================================================================

-spec test_ansible_integration(Config :: config()) -> term().
test_ansible_integration(Config) ->
    ct:pal("Testing Ansible integration"),

    %% Get test configuration
    TestConfig = get_test_config(ansible, Config),

    %% Start Ansible adapter
    {ok, _Pid} = erlmcp_config_adapter:start_link(TestConfig),

    %% Test playbook execution
    TestPlaybook = #{
        playbook => "test_playbook.yml",
        inventory => "test_inventory",
        extra_vars => #{
            env => "test"
        }
    },
    case erlmcp_config_adapter:run_playbook(TestPlaybook) of
        {ok, JobId} ->
            ct:pal("Ansible playbook execution successful: ~p", [JobId]);
        {error, Reason} ->
            ct:fail("Ansible playbook execution failed: ~p", [Reason])
    end,

    %% Stop adapter
    erlmcp_config_adapter:stop(config_adapter),

    ct:pal("Ansible integration test completed").

-spec test_puppet_integration(Config :: config()) -> term().
test_puppet_integration(Config) ->
    ct:pal("Testing Puppet integration"),

    %% Get test configuration
    TestConfig = get_test_config(puppet, Config),

    %% Start Puppet adapter
    {ok, _Pid} = erlmcp_config_adapter:start_link(TestConfig),

    %% Test catalog application
    TestCatalog = #{
        node => "test-node",
        environment => "production",
      resources => [
          #{
              type => "file",
              title => "/etc/test.conf",
              content => "test content"
          }
      ]
    },
    case erlmcp_config_adapter:apply_catalog(TestCatalog) of
        {ok, Report} ->
            ct:pal("Puppet catalog application successful: ~p", [Report]);
        {error, Reason} ->
            ct:fail("Puppet catalog application failed: ~p", [Reason])
    end,

    %% Stop adapter
    erlmcp_config_adapter:stop(config_adapter),

    ct:pal("Puppet integration test completed").

%%====================================================================
%% Container orchestration tests
%%====================================================================

-spec test_kubernetes_integration(Config :: config()) -> term().
test_kubernetes_integration(Config) ->
    ct:pal("Testing Kubernetes integration"),

    %% Get test configuration
    TestConfig = get_test_config(kubernetes, Config),

    %% Start Kubernetes adapter
    {ok, _Pid} = erlmcp_container_adapter:start_link(TestConfig),

    %% Test pod deployment
    TestPod = #{
        name => "test-pod",
        image => "nginx:latest",
        namespace => "default",
        replicas => 1
    },
    case erlmcp_container_adapter:deploy_pod(TestPod) of
        {ok, PodId} ->
            ct:pal("Kubernetes pod deployment successful: ~p", [PodId]);
        {error, Reason} ->
            ct:fail("Kubernetes pod deployment failed: ~p", [Reason])
    end,

    %% Test service creation
    TestService = #{
        name => "test-service",
        namespace => "default",
        selector => #{
            app => "test-pod"
        },
        ports => [#{port => 80, target_port => 80}]
    },
    case erlmcp_container_adapter:create_service(TestService) of
        {ok, ServiceId} ->
            ct:pal("Kubernetes service creation successful: ~p", [ServiceId]);
        {error, Reason} ->
            ct:fail("Kubernetes service creation failed: ~p", [Reason])
    end,

    %% Stop adapter
    erlmcp_container_adapter:stop(container_adapter),

    ct:pal("Kubernetes integration test completed").

-spec test_swarm_integration(Config :: config()) -> term().
test_swarm_integration(Config) ->
    ct:pal("Testing Docker Swarm integration"),

    %% Get test configuration
    TestConfig = get_test_config(swarm, Config),

    %% Start Swarm adapter
    {ok, _Pid} = erlmcp_container_adapter:start_link(TestConfig),

    %% Test service deployment
    TestService = #{
        name => "test-service",
        image => "nginx:latest",
        replicas => 1,
        ports => [#{published => 80, target => 80}]
    },
    case erlmcp_container_adapter:deploy_service(TestService) of
        {ok, ServiceId} ->
            ct:pal("Docker Swarm service deployment successful: ~p", [ServiceId]);
        {error, Reason} ->
            ct:fail("Docker Swarm service deployment failed: ~p", [Reason])
    end,

    %% Stop adapter
    erlmcp_container_adapter:stop(container_adapter),

    ct:pal("Docker Swarm integration test completed").

%%====================================================================
%% Integration orchestration tests
%%====================================================================

-spec test_enterprise_bus(Config :: config()) -> term().
test_enterprise_bus(Config) ->
    ct:pal("Testing enterprise event bus"),

    %% Start event bus
    erlmcp_enterprise_bus:start_link(),

    %% Test event publishing
    TestEvent = #{
        type => "test_event",
        data => #{
            message => "Hello, Enterprise!",
            timestamp => erlang:system_time(millisecond)
        }
    },
    ok = erlmcp_enterprise_bus:publish(test_event, TestEvent),

    %% Test event subscription
    Self = self(),
    ok = erlmcp_enterprise_bus:subscribe(test_event, Self),

    %% Wait for event
    receive
        {enterprise_event, test_event, ReceivedEvent} ->
            ct:pal("Enterprise bus event received: ~p", [ReceivedEvent]);
        timeout ->
            ct:fail("Enterprise bus timeout")
    after 5000 ->
        ct:fail("Enterprise bus timeout")
    end,

    %% Stop event bus
    erlmcp_enterprise_bus:stop(),

    ct:pal("Enterprise event bus test completed").

-spec test_enterprise_registry(Config :: config()) -> term().
test_enterprise_registry(Config) ->
    ct:pal("Testing enterprise registry"),

    %% Start registry
    erlmcp_enterprise_registry:start_link(),

    %% Test service registration
    TestService = #{
        name => "test-service",
        type => "api",
        version => "1.0.0",
        endpoints => ["/api/test"]
    },
    ok = erlmcp_enterprise_registry:register_service(test_service, TestService),

    %% Test service lookup
    case erlmcp_enterprise_registry:get_service(test_service) of
        {ok, RetrievedService} ->
            ct:pal("Service retrieved successfully: ~p", [RetrievedService]);
        {error, Reason} ->
            ct:fail("Service lookup failed: ~p", [Reason])
    end,

    %% Test service listing
    Services = erlmcp_enterprise_registry:list_services(api),
    ct:pal("API services found: ~p", [Services]),

    %% Stop registry
    erlmcp_enterprise_registry:stop(),

    ct:pal("Enterprise registry test completed").

-spec test_enterprise_metrics(Config :: config()) -> term().
test_enterprise_metrics(Config) ->
    ct:pal("Testing enterprise metrics"),

    %% Start metrics collector
    erlmcp_enterprise_metrics:start_link(),

    %% Test metric recording
    TestMetric = {test_counter, 1},
    ok = erlmcp_enterprise_metrics:record_metric(test_category, TestMetric),

    %% Test metric retrieval
    case erlmcp_enterprise_metrics:get_metrics(test_category) of
        {ok, Metrics} ->
            ct:pal("Metrics retrieved successfully: ~p", [Metrics]);
        {error, Reason} ->
            ct:fail("Metrics retrieval failed: ~p", [Reason])
    end,

    %% Stop metrics collector
    erlmcp_enterprise_metrics:stop(),

    ct:pal("Enterprise metrics test completed").

-spec test_enterprise_health(Config :: config()) -> term().
test_enterprise_health(Config) ->
    ct:pal("Testing enterprise health monitor"),

    %% Start health monitor
    erlmcp_enterprise_health:start_link(),

    %% Test identity health check
    case erlmcp_enterprise_health:check_health(identity) of
        {ok, Health} ->
            ct:pal("Identity health check result: ~p", [Health]);
        {error, Reason} ->
            ct:pal("Identity health check failed: ~p", [Reason])
    end,

    %% Test all health status
    AllHealth = erlmcp_enterprise_health:get_all_health_status(),
    ct:pal("All health status: ~p", [maps:size(AllHealth)]),

    %% Stop health monitor
    erlmcp_enterprise_health:stop(),

    ct:pal("Enterprise health monitor test completed").

%%====================================================================
%% Performance tests
%%====================================================================

-spec test_concurrent_connections(Config :: config()) -> term().
test_concurrent_connections(Config) ->
    ct:pal("Testing concurrent connections"),

    %% Create multiple connections
    Connections = lists:map(fun(I) ->
        TestConfig = get_test_config(kafka, Config),
        {ok, Pid} = erlmcp_servicebus_adapter:start_link(TestConfig),
        Pid
    end, lists:seq(1, 100)),

    %% Test concurrent message sending
    Pids = lists:map(fun(Pid) ->
        spawn_link(fun() ->
            TestMessage = #{
                key => list_to_binary("key_" ++ integer_to_list(erlang:unique_integer())),
                value = <<"test-value">>
            },
            erlmcp_servicebus_adapter:send_message(Pid, TestMessage)
        end)
    end, Connections),

    %% Wait for all operations to complete
    [receive
        {Pid, Result} ->
            case Result of
                ok -> ok;
                {error, Reason} -> ct:fail("Message send failed: ~p", [Reason])
            end
     end || Pid <- Pids],

    %% Cleanup
    [erlmcp_servicebus_adapter:stop(Pid) || Pid <- Connections],

    ct:pal("Concurrent connections test completed").

-spec test_load_balancing(Config :: config()) -> term().
test_load_balancing(Config) ->
    ct:pal("Testing load balancing"),

    %% Create multiple adapters
    Adapters = lists:map(fun(I) ->
        TestConfig = get_test_config(kafka, Config),
        TestConfig1 = TestConfig#{adapter_id => I},
        {ok, Pid} = erlmcp_servicebus_adapter:start_link(TestConfig1),
        Pid
    end, lists:seq(1, 5)),

    %% Test load distribution
    MessagesSent = lists:foldl(fun(Adapter, Acc) ->
        TestMessage = #{
            key => list_to_binary("load_test_" ++ integer_to_list(erlang:unique_integer())),
            value = <<"load-test-value">>
        },
        case erlmcp_servicebus_adapter:send_message(Adapter, TestMessage) of
            ok -> Acc + 1;
            {error, _} -> Acc
        end
    end, 0, Adapters),

    ct:pal("Load balancing test: ~p messages sent", [MessagesSent]),

    %% Cleanup
    [erlmcp_servicebus_adapter:stop(Adapter) || Adapter <- Adapters],

    ct:pal("Load balancing test completed").

-spec test_failover(Config :: config()) -> term().
test_failover(Config) ->
    ct:pal("Testing failover"),

    %% Start primary adapter
    TestConfig = get_test_config(kafka, Config),
    {ok, PrimaryPid} = erlmcp_servicebus_adapter:start_link(TestConfig),

    %% Start backup adapter
    TestConfig2 = TestConfig#{adapter_id => "backup"},
    {ok, BackupPid} = erlmcp_servicebus_adapter:start_link(TestConfig2),

    %% Send message to primary
    TestMessage = #{
        key => <<"failover-test">>,
        value = <<"primary">>
    },
    case erlmcp_servicebus_adapter:send_message(PrimaryPid, TestMessage) of
        ok ->
            ct:pal("Primary message send successful");
        {error, Reason} ->
            ct:fail("Primary message send failed: ~p", [Reason])
    end,

    %% Simulate primary failure (stop it)
    erlmcp_servicebus_adapter:stop(PrimaryPid),

    %% Send message to backup
    TestMessage2 = #{
        key => <<"failover-test">>,
        value = <<"backup">>
    },
    case erlmcp_servicebus_adapter:send_message(BackupPid, TestMessage2) of
        ok ->
            ct:pal("Backup message send successful");
        {error, Reason} ->
            ct:fail("Backup message send failed: ~p", [Reason])
    end,

    %% Cleanup
    erlmcp_servicebus_adapter:stop(BackupPid),

    ct:pal("Failover test completed").

%%====================================================================
%% Security tests
%%====================================================================

-spec test_authentication(Config :: config()) -> term().
test_authentication(Config) ->
    ct:pal("Testing authentication"),

    %% Start identity provider
    TestConfig = get_test_config(okta, Config),
    {ok, _Pid} = erlmcp_identity_adapter:start_link(TestConfig),

    %% Test valid token
    ValidToken = generate_valid_token(),
    case erlmcp_identity_adapter:authenticate(ValidToken) of
        {ok, User} ->
            ct:pal("Authentication successful: ~p", [User]);
        {error, Reason} ->
            ct:fail("Authentication failed: ~p", [Reason])
    end,

    %% Test invalid token
    InvalidToken = generate_invalid_token(),
    case erlmcp_identity_adapter:authenticate(InvalidToken) of
        {error, authentication_failed} ->
            ct:pal("Invalid token correctly rejected");
        {ok, _} ->
            ct:fail("Invalid token accepted")
    end,

    %% Stop adapter
    erlmcp_identity_adapter:stop(okta_adapter),

    ct:pal("Authentication test completed").

-spec test_authorization(Config :: config()) -> term().
test_authorization(Config) ->
    ct:pal("Testing authorization"),

    %% Start security adapter
    TestConfig = get_test_config(siem, Config),
    {ok, _Pid} = erlmcp_security_adapter:start_link(TestConfig),

    %% Test authorized access
    AuthorizedUser = #{
        id => <<"user1">>,
        roles => [<<"admin">>]
    },
    TestResource = #{
        id => <<"resource1">>,
        permissions => [<<"admin">>]
    },
    case erlmcp_security_adapter:check_permission(AuthorizedUser, TestResource) of
        {ok, authorized} ->
            ct:pal("Authorization successful for admin user");
        {error, Reason} ->
            ct:fail("Authorization failed: ~p", [Reason])
    end,

    %% Test unauthorized access
    UnauthorizedUser = #{
        id => <<"user2">>,
        roles => [<<"viewer">>]
    },
    case erlmcp_security_adapter:check_permission(UnauthorizedUser, TestResource) of
        {error, unauthorized} ->
            ct:pal("Unauthorized access correctly rejected");
        {ok, _} ->
            ct:fail("Unauthorized access accepted")
    end,

    %% Stop adapter
    erlmcp_security_adapter:stop(siem_adapter),

    ct:pal("Authorization test completed").

-spec test_data_encryption(Config :: config()) -> term().
test_data_encryption(Config) ->
    ct:pal("Testing data encryption"),

    %% Start security adapter
    TestConfig = get_test_config(security, Config),
    {ok, _Pid} = erlmcp_security_adapter:start_link(TestConfig),

    %% Test encryption
    Plaintext = <<"sensitive-data">>,
    case erlmcp_security_adapter:encrypt(Plaintext) of
        {ok, Ciphertext} ->
            ct:pal("Encryption successful");
        {error, Reason} ->
            ct:fail("Encryption failed: ~p", [Reason])
    end,

    %% Test decryption
    case erlmcp_security_adapter:decrypt(Ciphertext) of
        {ok, Decrypted} when Decrypted =:= Plaintext ->
            ct:pal("Decryption successful");
        {error, Reason} ->
            ct:fail("Decryption failed: ~p", [Reason])
    end;
    {error, Reason} ->
        ct:fail("Cannot decrypt without ciphertext: ~p", [Reason])
    end,

    %% Stop adapter
    erlmcp_security_adapter:stop(security_adapter),

    ct:pal("Data encryption test completed").

%%====================================================================
%% Compliance tests
%%====================================================================

-spec test_audit_logging(Config :: config()) -> term().
test_audit_logging(Config) ->
    ct:pal("Testing audit logging"),

    %% Start audit logger
    erlmcp_enterprise_audit:start_link(),

    %% Test audit events
    TestEvent = #{
        timestamp => erlang:system_time(millisecond),
        user => "test_user",
        action => "login",
        resource => "/api/login",
        result => "success",
        details => #{}
    },
    ok = erlmcp_enterprise_audit:log_event(TestEvent),

    %% Test audit retrieval
    case erlmcp_enterprise_audit:get_events(#{user => "test_user"}) of
        {ok, Events} ->
            ct:pal("Audit events retrieved: ~p", [length(Events)]);
        {error, Reason} ->
            ct:fail("Audit retrieval failed: ~p", [Reason])
    end,

    %% Stop audit logger
    erlmcp_enterprise_audit:stop(),

    ct:pal("Audit logging test completed").

-spec test_data_retention(Config :: config()) -> term().
test_data_retention(Config) ->
    ct:pal("Testing data retention"),

    %% Start retention manager
    erlmcp_retention_manager:start_link(),

    %% Add test data
    TestData = #{
        id => "test_data_1",
        timestamp => erlang:system_time(millisecond),
        data => <<"test data">>
    },
    ok = erlmcp_retention_manager:add_data(test_data, TestData),

    %% Test retention policy
    Policy = #{
        retention_period => 86400000,  % 24 hours
        max_records => 100
    },
    ok = erlmcp_retention_manager:set_policy(test_data, Policy),

    %% Check data expiration
    erlmcp_retention_manager:check_expiration(),

    %% Stop retention manager
    erlmcp_retention_manager:stop(),

    ct:pal("Data retention test completed").

-spec test_regulatory_compliance(Config :: config()) -> term().
test_regulatory_compliance(Config) ->
    ct:pal("Testing regulatory compliance"),

    %% Start compliance monitor
    erlmcp_compliance_monitor:start_link(),

    %% Test GDPR compliance
    GDPRTest = #{
        regulation => "GDPR",
        data_subject => "test_subject",
        purpose => "testing",
        retention_period => 30,
        data_processing => true
    },
    case erlmcp_compliance_monitor:check_gdpr(GDPRTest) of
        {ok, compliant} ->
            ct:pal("GDPR compliance check passed");
        {error, Violations} ->
            ct:pal("GDPR compliance violations: ~p", [Violations])
    end,

    %% Test HIPAA compliance
    HIPAATest = #{
        regulation => "HIPAA",
        phi_data => false,
        encryption => true,
        access_controls => true
    },
    case erlmcp_compliance_monitor:check_hipaa(HIPAATest) of
        {ok, compliant} ->
            ct:pal("HIPAA compliance check passed");
        {error, Violations} ->
            ct:pal("HIPAA compliance violations: ~p", [Violations])
    end,

    %% Test SOX compliance
    SOXTest = #{
        regulation => "SOX",
        financial_controls => true,
        audit_trail => true,
        segregation => true
    },
    case erlmcp_compliance_monitor:check_sox(SOXTest) of
        {ok, compliant} ->
            ct:pal("SOX compliance check passed");
        {error, Violations} ->
            ct:pal("SOX compliance violations: ~p", [Violations])
    end,

    %% Stop compliance monitor
    erlmcp_compliance_monitor:stop(),

    ct:pal("Regulatory compliance test completed").

%%====================================================================
%% Helper functions
%%====================================================================

-spec init_test_data() -> map().
init_test_data() ->
    #{
        okta => #{
            endpoint => "https://test-org.okta.com",
            client_id => "test_client_id",
            client_secret => "test_client_secret"
        },
        azure_ad => #{
            tenant_id => "test_tenant",
            client_id => "test_client_id",
            client_secret => "test_client_secret"
        },
        kafka => #{
            brokers => ["localhost:9092"],
            topic_prefix => "test_erlmcp"
        },
        splunk => #{
            host => "localhost",
            port => 8088,
            token => "test_token"
        }
    }.

-spec get_test_config(atom(), config()) -> map().
get_test_config(Service, Config) ->
    TestData = proplists:get_value(test_data, Config, #{}),
    case maps:find(Service, TestData) of
        {ok, ConfigMap} ->
            ConfigMap#{service => Service};
        error ->
            #{service => Service}
    end.

-spec generate_test_token() -> binary().
generate_test_token() ->
    list_to_binary("test_token_" + integer_to_list(erlang:unique_integer())).

-spec generate_valid_token() -> binary().
generate_valid_token() ->
    <<"valid_token">>.

-spec generate_invalid_token() -> binary().
generate_invalid_token() ->
    <<"invalid_token">>.

-spec cleanup_test_data(map()) -> ok.
cleanup_test_data(_TestData) ->
    %% Clean up any test resources
    ok.