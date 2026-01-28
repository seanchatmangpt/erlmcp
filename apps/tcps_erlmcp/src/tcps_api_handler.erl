%%%-------------------------------------------------------------------
%%% @doc TCPS API Handler - REST API for Diataxis Simulator
%%%
%%% Provides RESTful API endpoints for the TCPS MCP Diataxis
%%% simulator web interface.
%%%
%%% Endpoints:
%%% - GET /api/diataxis/quadrants - Get Diataxis quadrant structure
%%% - GET /api/diataxis/content/:quadrant - Get content for specific quadrant
%%% - GET /api/simulator/status - Get simulator status
%%% - GET /api/simulator/workflow - Get workflow items
%%% - GET /api/simulator/kanban - Get Kanban board state
%%% - GET /api/simulator/metrics - Get current metrics
%%% - GET /api/simulator/andons - Get Andon alerts
%%% - GET /api/simulator/quality-gates - Get quality gate status
%%% - GET /api/mcp/tools - List available MCP tools
%%% - POST /api/mcp/tools/:tool/execute - Execute MCP tool
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcps_api_handler).

-export([init/2, terminate/3]).
-export([allowed_methods/2, content_types_provided/2, content_types_accepted/2]).
-export([to_json/2, from_json/2]).

-include_lib("kernel/include/logger.hrl").

%%%===================================================================
%%% Cowboy REST Handler Callbacks
%%%===================================================================

init(Req, Opts) ->
    Endpoint = maps:get(endpoint, Opts, unknown),
    {cowboy_rest, Req, #{endpoint => Endpoint}}.

allowed_methods(Req, State = #{endpoint := mcp_execute}) ->
    {[<<"POST">>], Req, State};
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"OPTIONS">>], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json}], Req, State}.

%%%===================================================================
%%% Response Handlers
%%%===================================================================

%% GET /api/diataxis/quadrants
to_json(Req, State = #{endpoint := diataxis_quadrants}) ->
    Quadrants = get_diataxis_quadrants(),
    Body = jsx:encode(#{quadrants => Quadrants}),
    {Body, Req, State};

%% GET /api/diataxis/content/:quadrant
to_json(Req, State = #{endpoint := diataxis_content}) ->
    Quadrant = cowboy_req:binding(quadrant, Req),
    Content = get_diataxis_content(Quadrant),
    Body = jsx:encode(#{quadrant => Quadrant, content => Content}),
    {Body, Req, State};

%% GET /api/simulator/status
to_json(Req, State = #{endpoint := simulator_status}) ->
    Status = get_simulator_status(),
    Body = jsx:encode(Status),
    {Body, Req, State};

%% GET /api/simulator/workflow
to_json(Req, State = #{endpoint := workflow}) ->
    Workflows = get_all_workflows(),
    Body = jsx:encode(#{workflows => Workflows}),
    {Body, Req, State};

%% GET /api/simulator/workflow/:id
to_json(Req, State = #{endpoint := workflow_detail}) ->
    WorkflowId = cowboy_req:binding(id, Req),
    Workflow = get_workflow_by_id(WorkflowId),
    Body = jsx:encode(#{workflow => Workflow}),
    {Body, Req, State};

%% GET /api/simulator/kanban
to_json(Req, State = #{endpoint := kanban}) ->
    Kanban = get_kanban_state(),
    Body = jsx:encode(Kanban),
    {Body, Req, State};

%% GET /api/simulator/metrics
to_json(Req, State = #{endpoint := metrics}) ->
    Metrics = get_simulator_metrics(),
    Body = jsx:encode(Metrics),
    {Body, Req, State};

%% GET /api/simulator/andons
to_json(Req, State = #{endpoint := andons}) ->
    Andons = get_andon_alerts(),
    Body = jsx:encode(#{andons => Andons}),
    {Body, Req, State};

%% GET /api/simulator/quality-gates
to_json(Req, State = #{endpoint := quality_gates}) ->
    QualityGates = get_quality_gates(),
    Body = jsx:encode(#{quality_gates => QualityGates}),
    {Body, Req, State};

%% GET /api/mcp/tools
to_json(Req, State = #{endpoint := mcp_tools}) ->
    Tools = get_mcp_tools(),
    Body = jsx:encode(#{tools => Tools}),
    {Body, Req, State};

%% GET /api/health
to_json(Req, State = #{endpoint := health}) ->
    Health = #{
        status => <<"ok">>,
        timestamp => erlang:system_time(millisecond),
        server => <<"TCPS Diataxis Simulator">>,
        version => <<"1.0.0">>
    },
    Body = jsx:encode(Health),
    {Body, Req, State};

to_json(Req, State) ->
    Body = jsx:encode(#{error => <<"Unknown endpoint">>}),
    {Body, Req, State}.

%% POST handlers
from_json(Req, State = #{endpoint := mcp_execute}) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    Data = jsx:decode(Body, [return_maps]),
    Tool = cowboy_req:binding(tool, Req2),

    Result = execute_mcp_tool(Tool, Data),
    ResponseBody = jsx:encode(#{result => Result}),

    Req3 = cowboy_req:set_resp_body(ResponseBody, Req2),
    {true, Req3, State};

from_json(Req, State) ->
    Body = jsx:encode(#{error => <<"Method not allowed">>}),
    Req2 = cowboy_req:set_resp_body(Body, Req),
    {false, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

%%%===================================================================
%%% Internal Functions - Diataxis Content
%%%===================================================================

%% @private Get Diataxis quadrant structure
get_diataxis_quadrants() ->
    [
        #{
            id => <<"tutorial">>,
            name => <<"Tutorial">>,
            description => <<"Learning-oriented: Take the user through a series of steps to complete a project">>,
            color => <<"#3498db">>,
            icon => <<"graduation-cap">>,
            quadrant => <<"learning-doing">>
        },
        #{
            id => <<"howto">>,
            name => <<"How-To Guide">>,
            description => <<"Task-oriented: Guide the user through solving a real-world problem">>,
            color => <<"#e74c3c">>,
            icon => <<"tools">>,
            quadrant => <<"problem-doing">>
        },
        #{
            id => <<"explanation">>,
            name => <<"Explanation">>,
            description => <<"Understanding-oriented: Clarify and illuminate a particular topic">>,
            color => <<"#2ecc71">>,
            icon => <<"lightbulb">>,
            quadrant => <<"learning-thinking">>
        },
        #{
            id => <<"reference">>,
            name => <<"Reference">>,
            description => <<"Information-oriented: Describe the machinery, be accurate and complete">>,
            color => <<"#f39c12">>,
            icon => <<"book">>,
            quadrant => <<"problem-thinking">>
        }
    ].

%% @private Get content for specific Diataxis quadrant
get_diataxis_content(<<"tutorial">>) ->
    #{
        title => <<"TCPS Tutorial: Building Your First Workflow">>,
        sections => [
            #{
                title => <<"Introduction">>,
                content => <<"Welcome to the TCPS tutorial! You'll learn how to create and manage Toyota Code Production System workflows.">>
            },
            #{
                title => <<"Step 1: Create a Work Item">>,
                content => <<"Click 'New Work Item' to create your first item. Give it a title and description.">>,
                interactive => true,
                validation => <<"work_item_created">>
            },
            #{
                title => <<"Step 2: Move Through Kanban">>,
                content => <<"Drag your work item from Backlog → Ready → In Progress.">>,
                interactive => true,
                validation => <<"item_moved_to_in_progress">>
            },
            #{
                title => <<"Step 3: Quality Gates">>,
                content => <<"Watch as quality gates automatically validate your work at each stage.">>,
                interactive => true,
                validation => <<"quality_gate_passed">>
            },
            #{
                title => <<"Step 4: Handle Andon">>,
                content => <<"If quality gates fail, an Andon alert is triggered. Learn to resolve it.">>,
                interactive => true,
                validation => <<"andon_resolved">>
            }
        ]
    };

get_diataxis_content(<<"howto">>) ->
    #{
        title => <<"How-To: Configure TCPS for Your Project">>,
        sections => [
            #{
                title => <<"Setting WIP Limits">>,
                content => <<"Configure Work-In-Progress limits for each Kanban bucket to prevent bottlenecks.">>
            },
            #{
                title => <<"Defining Quality Gates">>,
                content => <<"Set up automated quality checks: test coverage, pass rates, defect thresholds.">>
            },
            #{
                title => <<"Andon Alert Rules">>,
                content => <<"Configure when and how Andon alerts are triggered based on failure types.">>
            },
            #{
                title => <<"MCP Tool Integration">>,
                content => <<"Connect MCP tools to automate workflow steps and quality checks.">>
            }
        ]
    };

get_diataxis_content(<<"explanation">>) ->
    #{
        title => <<"Understanding TCPS: Toyota Principles in Code">>,
        sections => [
            #{
                title => <<"Just-In-Time Development">>,
                content => <<"TCPS applies Toyota's JIT principle: work only on what's needed, when it's needed.">>
            },
            #{
                title => <<"Jidoka (Autonomation)">>,
                content => <<"Automated quality gates embody Jidoka: stop the line when defects are detected.">>
            },
            #{
                title => <<"Andon Cord Philosophy">>,
                content => <<"Any developer can pull the Andon cord to stop production and fix quality issues.">>
            },
            #{
                title => <<"Kaizen (Continuous Improvement)">>,
                content => <<"Metrics and feedback loops drive continuous process improvement.">>
            },
            #{
                title => <<"Visual Management">>,
                content => <<"Kanban boards provide visual workflow state, making bottlenecks immediately visible.">>
            }
        ]
    };

get_diataxis_content(<<"reference">>) ->
    #{
        title => <<"TCPS Reference Documentation">>,
        sections => [
            #{
                title => <<"Kanban Bucket States">>,
                content => <<"Backlog, Ready, In Progress, Review, Done - each with configurable WIP limits.">>
            },
            #{
                title => <<"Quality Gate Metrics">>,
                content => <<"test_pass_rate (≥80%), code_coverage (≥80%), defect_rate (≤1%), first_pass_yield (≥95%).">>
            },
            #{
                title => <<"Andon Severity Levels">>,
                content => <<"critical (stops pipeline), warning (alerts team), info (logged only).">>
            },
            #{
                title => <<"MCP Tool Schema">>,
                content => <<"Tools follow MCP protocol: name, description, inputSchema, execute handler.">>
            },
            #{
                title => <<"Metrics API">>,
                content => <<"GET /api/simulator/metrics returns real-time WIP, quality, Andon, and flow data.">>
            }
        ]
    };

get_diataxis_content(_) ->
    #{
        title => <<"Content Not Found">>,
        sections => []
    }.

%%%===================================================================
%%% Internal Functions - Simulator Data
%%%===================================================================

%% @private Get simulator status
get_simulator_status() ->
    #{
        status => <<"running">>,
        uptime_seconds => get_uptime(),
        mode => <<"interactive">>,
        active_workflows => count_workflows(),
        ws_connections => count_ws_connections()
    }.

%% @private Get all workflows
get_all_workflows() ->
    [
        #{
            id => <<"wf-001">>,
            title => <<"Authentication Module">>,
            status => <<"in_progress">>,
            current_stage => <<"development">>,
            progress => 0.65,
            quality_score => 0.88
        },
        #{
            id => <<"wf-002">>,
            title => <<"Payment Integration">>,
            status => <<"review">>,
            current_stage => <<"code_review">>,
            progress => 0.90,
            quality_score => 0.92
        },
        #{
            id => <<"wf-003">>,
            title => <<"User Dashboard">>,
            status => <<"ready">>,
            current_stage => <<"design">>,
            progress => 0.30,
            quality_score => 0.85
        }
    ].

%% @private Get workflow by ID
get_workflow_by_id(Id) ->
    #{
        id => Id,
        title => <<"Sample Workflow">>,
        status => <<"in_progress">>,
        stages => [
            #{name => <<"requirements">>, completed => true, quality_passed => true},
            #{name => <<"design">>, completed => true, quality_passed => true},
            #{name => <<"development">>, completed => false, quality_passed => false},
            #{name => <<"testing">>, completed => false, quality_passed => false},
            #{name => <<"deployment">>, completed => false, quality_passed => false}
        ],
        metrics => #{
            lead_time_hours => 36,
            cycle_time_hours => 18,
            defects_found => 2,
            defects_fixed => 1
        }
    }.

%% @private Get Kanban board state
get_kanban_state() ->
    #{
        buckets => [
            #{
                name => <<"backlog">>,
                limit => 10,
                items => [
                    #{id => <<"item-1">>, title => <<"Feature X">>, priority => <<"high">>},
                    #{id => <<"item-2">>, title => <<"Bug Fix Y">>, priority => <<"medium">>}
                ]
            },
            #{
                name => <<"ready">>,
                limit => 5,
                items => [
                    #{id => <<"item-3">>, title => <<"Refactor Z">>, priority => <<"low">>}
                ]
            },
            #{
                name => <<"in_progress">>,
                limit => 3,
                items => [
                    #{id => <<"item-4">>, title => <<"Auth Module">>, priority => <<"high">>,
                      assignee => <<"dev-1">>, started_at => 1706275200000}
                ]
            },
            #{
                name => <<"review">>,
                limit => 2,
                items => []
            },
            #{
                name => <<"done">>,
                limit => 999,
                items => [
                    #{id => <<"item-5">>, title => <<"Payment Gateway">>, completed_at => 1706261600000}
                ]
            }
        ],
        wip_status => #{
            total_wip => 4,
            total_capacity => 20,
            utilization => 0.20
        }
    }.

%% @private Get simulator metrics
get_simulator_metrics() ->
    #{
        throughput => #{
            items_per_day => 3.2,
            trend => <<"increasing">>,
            sparkline => [2.8, 3.0, 3.1, 3.2, 3.5]
        },
        lead_time => #{
            average_hours => 42,
            trend => <<"decreasing">>,
            sparkline => [48, 46, 44, 43, 42]
        },
        quality => #{
            pass_rate => 0.92,
            coverage => 0.87,
            defect_rate => 0.015,
            trend => <<"stable">>
        },
        wip => #{
            current => 6,
            limit => 15,
            percentage => 0.40
        }
    }.

%% @private Get Andon alerts
get_andon_alerts() ->
    case ets:info(tcps_simulator_andons) of
        undefined -> [];
        _ ->
            All = ets:tab2list(tcps_simulator_andons),
            [Event || {_Id, Event} <- All]
    end.

%% @private Get quality gates status
get_quality_gates() ->
    [
        #{
            name => <<"Test Pass Rate">>,
            current => 0.92,
            target => 0.80,
            status => <<"pass">>,
            trend => <<"stable">>
        },
        #{
            name => <<"Code Coverage">>,
            current => 0.87,
            target => 0.80,
            status => <<"pass">>,
            trend => <<"increasing">>
        },
        #{
            name => <<"Defect Rate">>,
            current => 0.015,
            target => 0.01,
            status => <<"warning">>,
            trend => <<"increasing">>
        },
        #{
            name => <<"First Pass Yield">>,
            current => 0.96,
            target => 0.95,
            status => <<"pass">>,
            trend => <<"stable">>
        }
    ].

%% @private Get available MCP tools
get_mcp_tools() ->
    [
        #{
            name => <<"tcps.work_order.create">>,
            description => <<"Create a new TCPS work order">>,
            inputSchema => #{
                type => <<"object">>,
                properties => #{
                    title => #{type => <<"string">>},
                    description => #{type => <<"string">>},
                    priority => #{type => <<"string">>, enum => [<<"low">>, <<"medium">>, <<"high">>]}
                }
            }
        },
        #{
            name => <<"tcps.quality_gate.check">>,
            description => <<"Run quality gate validation">>,
            inputSchema => #{
                type => <<"object">>,
                properties => #{
                    workflow_id => #{type => <<"string">>},
                    gates => #{type => <<"array">>, items => #{type => <<"string">>}}
                }
            }
        },
        #{
            name => <<"tcps.andon.trigger">>,
            description => <<"Manually trigger Andon alert">>,
            inputSchema => #{
                type => <<"object">>,
                properties => #{
                    severity => #{type => <<"string">>, enum => [<<"info">>, <<"warning">>, <<"critical">>]},
                    title => #{type => <<"string">>},
                    description => #{type => <<"string">>}
                }
            }
        },
        #{
            name => <<"tcps.metrics.get">>,
            description => <<"Get current TCPS metrics">>,
            inputSchema => #{
                type => <<"object">>,
                properties => #{
                    metric_types => #{type => <<"array">>, items => #{type => <<"string">>}}
                }
            }
        }
    ].

%% @private Execute MCP tool
execute_mcp_tool(Tool, Params) ->
    ?LOG_INFO("Executing MCP tool: ~s with params: ~p", [Tool, Params]),

    %% Mock execution - would integrate with real MCP system
    #{
        tool => Tool,
        params => Params,
        status => <<"success">>,
        output => <<"Tool executed successfully">>,
        execution_time_ms => 42,
        timestamp => erlang:system_time(millisecond)
    }.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% @private Get uptime
get_uptime() ->
    {UpTime, _} = erlang:statistics(wall_clock),
    UpTime div 1000.

%% @private Count workflows
count_workflows() ->
    case ets:info(tcps_simulator_work_items) of
        undefined -> 0;
        _ -> ets:info(tcps_simulator_work_items, size)
    end.

%% @private Count WebSocket connections
count_ws_connections() ->
    case ets:info(tcps_ws_connections) of
        undefined -> 0;
        _ -> ets:info(tcps_ws_connections, size)
    end.
