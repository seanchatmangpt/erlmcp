%%% @doc MCP Projection - "MCP is just a port"
%%%
%%% Maps MCP protocol to Case reality. Every MCP operation is a Case syscall.
%%%
%%% "One kernel, two faces" Architecture:
%%% - Case is the OS kernel
%%% - MCP is a protocol port (like A2A)
%%% - Resources map to Case state (snapshots, artifacts, receipts)
%%% - Tools map to Case tool_call syscalls
%%% - Prompts map to workflow templates
%%%
%%% Resource URI Formats:
%%% - case://<case_id>/snapshot - Case state snapshot
%%% - case://<case_id>/artifacts - Case artifacts list
%%% - case://<case_id>/receipt - Receipt chain head
%%% - case://<case_id>/history - Event history
%%% - chain://block/<height> - Blockchain block
%%% - chain://tx/<id> - Transaction
%%% - identity://<address> - Identity info
%%%
%%% @end
-module(pqc_projection_mcp).

-include("pqchain.hrl").

%% API exports
-export([
    default_net/0,
    read_resource/1,
    list_resources/0,
    get_prompt/1,
    list_prompts/0,
    case_to_mcp_tool_result/1,
    snapshot_to_mcp_resource/1,
    artifacts_to_mcp_resource/1,
    receipt_to_mcp_resource/1,
    history_to_mcp_resource/1,
    parse_resource_uri/1
]).

%%% ============================================================================
%%% Types
%%% ============================================================================

-type resource_uri() :: binary().
-type resource_content() :: #{
    uri := binary(),
    name := binary(),
    mimeType => binary(),
    text => binary(),
    blob => binary()
}.

-type prompt() :: #{
    name := binary(),
    description := binary(),
    arguments => [prompt_argument()]
}.

-type prompt_argument() :: #{
    name := binary(),
    description := binary(),
    required => boolean()
}.

-export_type([resource_uri/0, resource_content/0, prompt/0]).

%%% ============================================================================
%%% API Functions - Workflow Net
%%% ============================================================================

%% @doc Return minimal default workflow net for new Cases
%%
%% This is the "hello world" workflow: single transition that completes.
%%
%% @end
-spec default_net() -> map().
default_net() ->
    #{
        id => <<"default_mcp_workflow">>,
        name => <<"Default MCP Workflow">>,
        transitions => #{
            start => #{
                guard => fun(M) -> maps:get(initial, M, 0) > 0 end,
                effect => fun(M) ->
                    {ok,
                     M#{initial => 0, completed => 1},
                     [{set_variable, status, completed}]}
                end,
                metadata => #{description => <<"Start and complete workflow">>}
            }
        },
        order => [start],
        initial_marking => #{initial => 1, completed => 0},
        metadata => #{
            version => 1,
            description => <<"Minimal workflow for MCP Cases">>
        }
    }.

%%% ============================================================================
%%% API Functions - Resource Reading
%%% ============================================================================

%% @doc Read resource by URI
%%
%% Parses URI and routes to appropriate handler.
%% Returns MCP resource content map.
%%
%% @end
-spec read_resource(resource_uri()) ->
    {ok, resource_content()} | {error, term()}.
read_resource(Uri) when is_binary(Uri) ->
    case parse_resource_uri(Uri) of
        {case_snapshot, CaseId} ->
            read_case_snapshot(CaseId);
        {case_artifacts, CaseId} ->
            read_case_artifacts(CaseId);
        {case_receipt, CaseId} ->
            read_case_receipt(CaseId);
        {case_history, CaseId} ->
            read_case_history(CaseId);
        {chain_block, Height} ->
            read_chain_block(Height);
        {chain_tx, TxId} ->
            read_chain_tx(TxId);
        {identity, Address} ->
            read_identity(Address);
        {error, _Reason} = Error ->
            Error
    end.

%% @doc List all available resource URIs
%%
%% Returns list of resource URIs that MCP clients can read.
%% Dynamically generated based on active Cases.
%%
%% @end
-spec list_resources() -> {ok, [resource_uri()]}.
list_resources() ->
    %% Get all active Case IDs from registry
    CaseIds = case whereis(pqc_case_registry) of
        undefined -> [];
        _ -> pqc_case_registry:list_cases()
    end,

    %% Generate resource URIs for each Case
    CaseResources = lists:flatmap(
        fun(CaseId) ->
            [
                iolist_to_binary([<<"case://">>, CaseId, <<"/snapshot">>]),
                iolist_to_binary([<<"case://">>, CaseId, <<"/artifacts">>]),
                iolist_to_binary([<<"case://">>, CaseId, <<"/receipt">>]),
                iolist_to_binary([<<"case://">>, CaseId, <<"/history">>])
            ]
        end,
        CaseIds
    ),

    %% Add static blockchain resources
    ChainResources = [
        <<"chain://block/latest">>,
        <<"chain://status">>
    ],

    {ok, CaseResources ++ ChainResources}.

%%% ============================================================================
%%% API Functions - Prompts
%%% ============================================================================

%% @doc Get prompt by name
-spec get_prompt(binary()) -> {ok, prompt()} | {error, not_found}.
get_prompt(Name) when is_binary(Name) ->
    Prompts = workflow_prompts(),
    case lists:keyfind(Name, 1, Prompts) of
        {Name, Prompt} -> {ok, Prompt};
        false -> {error, not_found}
    end.

%% @doc List all available prompts
-spec list_prompts() -> {ok, [prompt()]}.
list_prompts() ->
    Prompts = workflow_prompts(),
    {ok, [Prompt || {_Name, Prompt} <- Prompts]}.

%%% ============================================================================
%%% API Functions - MCP Conversions
%%% ============================================================================

%% @doc Convert Case tool result to MCP format
-spec case_to_mcp_tool_result({ok, term()} | {error, term()}) -> map().
case_to_mcp_tool_result({ok, Result}) ->
    #{
        content => [#{
            type => <<"text">>,
            text => format_result(Result)
        }],
        isError => false
    };
case_to_mcp_tool_result({error, Reason}) ->
    #{
        content => [#{
            type => <<"text">>,
            text => iolist_to_binary(io_lib:format("Error: ~p", [Reason]))
        }],
        isError => true
    }.

%% @doc Convert Case snapshot to MCP resource
-spec snapshot_to_mcp_resource(map()) -> resource_content().
snapshot_to_mcp_resource(Snapshot) ->
    #{
        uri => iolist_to_binary([<<"case://">>, maps:get(case_id, Snapshot, <<"unknown">>), <<"/snapshot">>]),
        name => <<"Case Snapshot">>,
        mimeType => <<"application/json">>,
        text => jsx:encode(Snapshot)
    }.

%% @doc Convert Case artifacts to MCP resource
-spec artifacts_to_mcp_resource(list()) -> resource_content().
artifacts_to_mcp_resource(Artifacts) ->
    #{
        uri => <<"case://artifacts">>,
        name => <<"Case Artifacts">>,
        mimeType => <<"application/json">>,
        text => jsx:encode(#{artifacts => Artifacts})
    }.

%% @doc Convert Case receipt to MCP resource
-spec receipt_to_mcp_resource(map()) -> resource_content().
receipt_to_mcp_resource(Receipt) ->
    ReceiptJson = #{
        hash => base64:encode(maps:get(hash, Receipt, <<>>)),
        prev_hash => base64:encode(maps:get(prev_hash, Receipt, <<>>)),
        timestamp => maps:get(timestamp, Receipt, 0),
        signature => format_signature(maps:get(signature, Receipt, undefined))
    },
    #{
        uri => <<"case://receipt">>,
        name => <<"Receipt Chain Head">>,
        mimeType => <<"application/json">>,
        text => jsx:encode(ReceiptJson)
    }.

%% @doc Convert Case history to MCP resource
-spec history_to_mcp_resource(list()) -> resource_content().
history_to_mcp_resource(History) ->
    #{
        uri => <<"case://history">>,
        name => <<"Case Event History">>,
        mimeType => <<"application/json">>,
        text => jsx:encode(#{history => History})
    }.

%% @doc Parse resource URI into structured form
-spec parse_resource_uri(binary()) ->
    {case_snapshot, binary()} |
    {case_artifacts, binary()} |
    {case_receipt, binary()} |
    {case_history, binary()} |
    {chain_block, binary() | integer()} |
    {chain_tx, binary()} |
    {identity, binary()} |
    {error, term()}.
parse_resource_uri(Uri) when is_binary(Uri) ->
    case binary:split(Uri, <<"://">>, [global]) of
        [<<"case">>, Rest] ->
            parse_case_uri(Rest);
        [<<"chain">>, Rest] ->
            parse_chain_uri(Rest);
        [<<"identity">>, Address] ->
            {identity, Address};
        _ ->
            {error, {invalid_uri_scheme, Uri}}
    end.

%%% ============================================================================
%%% Internal Functions - Resource Readers
%%% ============================================================================

%% @private
-spec read_case_snapshot(binary()) -> {ok, resource_content()} | {error, term()}.
read_case_snapshot(CaseId) ->
    case pqc_case_registry:lookup(CaseId) of
        {ok, Pid} ->
            case pqc_case:snapshot(Pid) of
                {ok, Snapshot} ->
                    {ok, snapshot_to_mcp_resource(Snapshot)};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, not_found} ->
            {error, case_not_found}
    end.

%% @private
-spec read_case_artifacts(binary()) -> {ok, resource_content()} | {error, term()}.
read_case_artifacts(CaseId) ->
    case pqc_case_registry:lookup(CaseId) of
        {ok, Pid} ->
            case pqc_case:snapshot(Pid) of
                {ok, Snapshot} ->
                    %% Extract artifacts from snapshot
                    Artifacts = maps:get(artifacts, Snapshot, []),
                    Resource = #{
                        uri => iolist_to_binary([<<"case://">>, CaseId, <<"/artifacts">>]),
                        name => <<"Case Artifacts">>,
                        mimeType => <<"application/json">>,
                        text => jsx:encode(#{artifacts => Artifacts})
                    },
                    {ok, Resource};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, not_found} ->
            {error, case_not_found}
    end.

%% @private
-spec read_case_receipt(binary()) -> {ok, resource_content()} | {error, term()}.
read_case_receipt(CaseId) ->
    case pqc_case_registry:lookup(CaseId) of
        {ok, Pid} ->
            case pqc_case:get_receipt(Pid) of
                {ok, Receipt} ->
                    {ok, receipt_to_mcp_resource(Receipt)};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, not_found} ->
            {error, case_not_found}
    end.

%% @private
-spec read_case_history(binary()) -> {ok, resource_content()} | {error, term()}.
read_case_history(CaseId) ->
    case pqc_case_registry:lookup(CaseId) of
        {ok, Pid} ->
            case pqc_case:snapshot(Pid) of
                {ok, Snapshot} ->
                    %% Extract history length from snapshot
                    HistoryLength = maps:get(history_length, Snapshot, 0),
                    Resource = #{
                        uri => iolist_to_binary([<<"case://">>, CaseId, <<"/history">>]),
                        name => <<"Case History">>,
                        mimeType => <<"application/json">>,
                        text => jsx:encode(#{
                            case_id => CaseId,
                            history_length => HistoryLength,
                            note => <<"Full history available via Case API">>
                        })
                    },
                    {ok, Resource};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, not_found} ->
            {error, case_not_found}
    end.

%% @private
-spec read_chain_block(binary() | integer()) -> {ok, resource_content()} | {error, term()}.
read_chain_block(Height) ->
    %% Placeholder for blockchain query
    HeightBin = if
        is_binary(Height) -> Height;
        is_integer(Height) -> integer_to_binary(Height);
        true -> <<"unknown">>
    end,
    Resource = #{
        uri => iolist_to_binary([<<"chain://block/">>, HeightBin]),
        name => <<"Blockchain Block">>,
        mimeType => <<"application/json">>,
        text => jsx:encode(#{
            height => HeightBin,
            status => <<"not_implemented">>,
            note => <<"Blockchain queries coming soon">>
        })
    },
    {ok, Resource}.

%% @private
-spec read_chain_tx(binary()) -> {ok, resource_content()} | {error, term()}.
read_chain_tx(TxId) ->
    %% Placeholder for transaction query
    Resource = #{
        uri => iolist_to_binary([<<"chain://tx/">>, TxId]),
        name => <<"Transaction">>,
        mimeType => <<"application/json">>,
        text => jsx:encode(#{
            tx_id => TxId,
            status => <<"not_implemented">>
        })
    },
    {ok, Resource}.

%% @private
-spec read_identity(binary()) -> {ok, resource_content()} | {error, term()}.
read_identity(Address) ->
    %% Delegate to pqc_identity
    case catch pqc_identity:lookup(Address) of
        {ok, Identity} ->
            Resource = #{
                uri => iolist_to_binary([<<"identity://">>, Address]),
                name => <<"PQC Identity">>,
                mimeType => <<"application/json">>,
                text => jsx:encode(Identity)
            },
            {ok, Resource};
        {error, not_found} ->
            {error, identity_not_found};
        _ ->
            Resource = #{
                uri => iolist_to_binary([<<"identity://">>, Address]),
                name => <<"PQC Identity">>,
                mimeType => <<"application/json">>,
                text => jsx:encode(#{
                    address => Address,
                    status => <<"not_found">>
                })
            },
            {ok, Resource}
    end.

%%% ============================================================================
%%% Internal Functions - URI Parsing
%%% ============================================================================

%% @private
-spec parse_case_uri(binary()) ->
    {case_snapshot, binary()} |
    {case_artifacts, binary()} |
    {case_receipt, binary()} |
    {case_history, binary()} |
    {error, term()}.
parse_case_uri(Rest) ->
    case binary:split(Rest, <<"/">>, [global]) of
        [CaseId, <<"snapshot">>] -> {case_snapshot, CaseId};
        [CaseId, <<"artifacts">>] -> {case_artifacts, CaseId};
        [CaseId, <<"receipt">>] -> {case_receipt, CaseId};
        [CaseId, <<"history">>] -> {case_history, CaseId};
        _ -> {error, invalid_case_uri}
    end.

%% @private
-spec parse_chain_uri(binary()) ->
    {chain_block, binary() | integer()} |
    {chain_tx, binary()} |
    {error, term()}.
parse_chain_uri(Rest) ->
    case binary:split(Rest, <<"/">>, [global]) of
        [<<"block">>, Height] ->
            try
                {chain_block, binary_to_integer(Height)}
            catch
                _:_ -> {chain_block, Height}
            end;
        [<<"tx">>, TxId] ->
            {chain_tx, TxId};
        [<<"status">>] ->
            {chain_block, <<"latest">>};
        _ ->
            {error, invalid_chain_uri}
    end.

%%% ============================================================================
%%% Internal Functions - Prompts
%%% ============================================================================

%% @private
%% Workflow prompts for MCP clients
-spec workflow_prompts() -> [{binary(), prompt()}].
workflow_prompts() ->
    [
        {<<"approval_workflow">>, #{
            name => <<"approval_workflow">>,
            description => <<"Create an approval workflow with submit, approve, and reject transitions">>,
            arguments => [
                #{
                    name => <<"title">>,
                    description => <<"Workflow title">>,
                    required => true
                },
                #{
                    name => <<"approver">>,
                    description => <<"Approver identity address">>,
                    required => false
                }
            ]
        }},
        {<<"simple_workflow">>, #{
            name => <<"simple_workflow">>,
            description => <<"Create a simple start-complete workflow">>,
            arguments => [
                #{
                    name => <<"name">>,
                    description => <<"Workflow name">>,
                    required => true
                }
            ]
        }},
        {<<"multi_step_workflow">>, #{
            name => <<"multi_step_workflow">>,
            description => <<"Create a multi-step sequential workflow">>,
            arguments => [
                #{
                    name => <<"steps">>,
                    description => <<"List of step names">>,
                    required => true
                }
            ]
        }}
    ].

%%% ============================================================================
%%% Internal Functions - Formatting
%%% ============================================================================

%% @private
-spec format_result(term()) -> binary().
format_result(Result) when is_map(Result) ->
    jsx:encode(Result);
format_result(Result) when is_binary(Result) ->
    Result;
format_result(Result) ->
    iolist_to_binary(io_lib:format("~p", [Result])).

%% @private
-spec format_signature(#pqc_signature{} | undefined) -> map() | null.
format_signature(undefined) ->
    null;
format_signature(#pqc_signature{algorithm = Algo, signature = Sig, public_key_hash = PkHash, timestamp = Ts}) ->
    #{
        algorithm => Algo,
        signature => base64:encode(Sig),
        public_key_hash => base64:encode(PkHash),
        timestamp => Ts
    };
format_signature(_) ->
    null.
