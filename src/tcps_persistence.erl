%%%-------------------------------------------------------------------
%%% @doc TCPS Receipt Storage and RDF Ontology Persistence
%%%
%%% Dual storage system:
%%% - JSON receipts with SHA-256 checksums
%%% - RDF/Turtle ontology with SPARQL queries
%%% - Backup/restore with integrity validation
%%% - Change tracking and audit trail
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcps_persistence).

-export([
    % Receipt Storage
    store_receipt/1,
    load_receipt/1,
    get_all_receipts/1,
    get_receipt/1,
    list_receipts_by_sku/1,
    list_receipts_by_stage/1,
    delete_receipt/1,
    verify_receipt/1,

    % Work Order Storage (NEW)
    store_work_order/1,
    get_work_order/1,
    list_work_orders/0,
    list_work_orders_by_status/1,
    list_work_orders_by_bucket/1,
    update_work_order/1,
    delete_work_order/1,

    % Andon Event Storage (NEW)
    store_andon_event/1,
    get_andon_event/1,
    list_andon_events/0,
    list_andon_events_by_status/1,
    update_andon_event/1,

    % RDF Ontology Persistence
    persist_to_ontology/1,
    persist_work_order/1,
    persist_andon/1,
    persist_root_cause/1,

    % SPARQL Query Interface (using Agent 9's cache)
    query_ontology/1,
    query_ontology/2,
    query_receipts_by_sku/1,
    query_work_orders_by_bucket/1,
    query_open_andons/0,

    % Ontology Rebuild (NEW)
    rebuild_ontology/0,
    update_ontology/1,

    % Backup and Restore (ENHANCED)
    backup/1,
    restore/1,
    backup_all/1,
    restore_from_backup/1,
    verify_integrity/0,

    % Ontology Validation
    validate_ontology/0,
    repair_ontology/1,

    % Change Log
    log_change/3,
    get_change_history/2,

    % Export Functions
    export_ontology/1,
    export_receipts/2,

    % Utility Functions
    init/0,
    ensure_dirs/0
]).

-include_lib("kernel/include/file.hrl").

-type receipt() :: #{
    sku_id := binary(),
    stage := atom(),
    timestamp := binary(),
    status := pass | fail | warning,
    evidence := binary(),
    checksum => binary()
}.

-type work_order() :: #{
    id := binary(),
    sku_id := binary(),
    bucket := atom(),
    priority := integer(),
    sla := integer(),
    created_at := binary()
}.

-type andon_event() :: #{
    id := binary(),
    sku_id := binary(),
    severity := atom(),
    root_cause := map(),
    status := open | investigating | resolved,
    created_at := binary()
}.

-type violation() :: #{
    entity := binary(),
    property := binary(),
    message := binary()
}.

%%%===================================================================
%%% API - Receipt Storage
%%%===================================================================

-spec store_receipt(Receipt :: receipt()) -> {ok, binary()} | {error, term()}.
store_receipt(Receipt) ->
    try
        #{sku_id := SkuId, stage := Stage} = Receipt,

        % Ensure directory exists
        ReceiptDir = receipt_dir(SkuId),
        ok = filelib:ensure_dir(filename:join(ReceiptDir, "dummy")),

        % Generate filename
        Timestamp = maps:get(timestamp, Receipt, timestamp_now()),
        Filename = generate_receipt_filename(Stage, Timestamp),
        Path = filename:join(ReceiptDir, Filename),

        % Calculate checksum
        ReceiptJson = jsone:encode(Receipt),
        Checksum = calculate_checksum(ReceiptJson),
        ReceiptWithChecksum = Receipt#{checksum => Checksum},

        % Write to file
        FinalJson = jsone:encode(ReceiptWithChecksum),
        ok = file:write_file(Path, FinalJson),

        % Persist to ontology
        ok = persist_to_ontology(ReceiptWithChecksum),

        % Log change
        ok = log_change(receipt, maps:get(sku_id, Receipt), #{
            action => created,
            path => list_to_binary(Path),
            timestamp => timestamp_now()
        }),

        {ok, list_to_binary(Path)}
    catch
        Error:Reason:Stack ->
            {error, {Error, Reason, Stack}}
    end.

-spec load_receipt(ReceiptPath :: binary()) -> {ok, receipt()} | {error, term()}.
load_receipt(ReceiptPath) ->
    try
        PathStr = binary_to_list(ReceiptPath),
        {ok, Content} = file:read_file(PathStr),
        Receipt = jsone:decode(Content, [{object_format, map}]),

        case verify_receipt(Receipt) of
            ok -> {ok, Receipt};
            {error, _} = Err -> Err
        end
    catch
        ErrClass:Reason ->
            {error, {ErrClass, Reason}}
    end.

-spec get_all_receipts(SkuId :: binary()) -> [receipt()].
get_all_receipts(SkuId) ->
    ReceiptDir = receipt_dir(SkuId),
    case filelib:is_dir(ReceiptDir) of
        true ->
            {ok, Files} = file:list_dir(ReceiptDir),
            JsonFiles = [filename:join(ReceiptDir, F) || F <- Files,
                         filename:extension(F) =:= ".json"],

            % Load and sort by timestamp
            Receipts = lists:filtermap(
                fun(Path) ->
                    case load_receipt(list_to_binary(Path)) of
                        {ok, Receipt} -> {true, Receipt};
                        {error, _} -> false
                    end
                end,
                JsonFiles
            ),

            lists:sort(
                fun(A, B) ->
                    maps:get(timestamp, A) =< maps:get(timestamp, B)
                end,
                Receipts
            );
        false ->
            []
    end.

-spec get_receipt(ReceiptId :: binary()) -> {ok, receipt()} | {error, not_found}.
get_receipt(ReceiptId) ->
    % Use ontology index for fast lookup (Agent 9's work)
    case tcps_ontology_index:lookup_receipt(ReceiptId) of
        {ok, SkuId, Stage, Timestamp} ->
            Path = receipt_path(SkuId, Stage, Timestamp),
            load_receipt(Path);
        {error, not_found} ->
            {error, not_found}
    end.

-spec list_receipts_by_sku(SkuId :: binary()) -> [receipt()].
list_receipts_by_sku(SkuId) ->
    % Use ontology index (Agent 9's work)
    tcps_ontology_index:get_receipts_by_sku(SkuId).

-spec list_receipts_by_stage(Stage :: atom()) -> [receipt()].
list_receipts_by_stage(Stage) ->
    % Use ontology index (Agent 9's work)
    tcps_ontology_index:get_receipts_by_stage(atom_to_binary(Stage, utf8)).

-spec delete_receipt(ReceiptId :: binary()) -> ok | {error, term()}.
delete_receipt(ReceiptId) ->
    case get_receipt(ReceiptId) of
        {ok, Receipt} ->
            SkuId = maps:get(sku_id, Receipt),
            Stage = maps:get(stage, Receipt),
            Timestamp = maps:get(timestamp, Receipt),
            Path = receipt_path(SkuId, Stage, Timestamp),

            % Delete JSON file
            case file:delete(Path) of
                ok ->
                    % TODO: Remove from RDF ontology
                    % TODO: Invalidate cache
                    tcps_query_cache:invalidate_all(),
                    ok;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, not_found} ->
            {error, not_found}
    end.

-spec verify_receipt(Receipt :: receipt()) -> ok | {error, term()}.
verify_receipt(Receipt) ->
    case maps:get(checksum, Receipt, undefined) of
        undefined ->
            {error, missing_checksum};
        StoredChecksum ->
            ReceiptWithoutChecksum = maps:remove(checksum, Receipt),
            ReceiptJson = jsone:encode(ReceiptWithoutChecksum),
            CalculatedChecksum = calculate_checksum(ReceiptJson),

            case StoredChecksum =:= CalculatedChecksum of
                true -> ok;
                false -> {error, checksum_mismatch}
            end
    end.

%%%===================================================================
%%% API - Work Order Storage
%%%===================================================================

-spec store_work_order(WorkOrder :: work_order()) -> {ok, binary()} | {error, term()}.
store_work_order(WorkOrder) ->
    try
        #{id := WorkOrderId} = WorkOrder,

        % Ensure directory exists
        WorkOrderDir = work_order_dir(),
        ok = filelib:ensure_dir(filename:join(WorkOrderDir, "dummy")),

        % Generate filename
        Filename = generate_work_order_filename(WorkOrderId),
        Path = filename:join(WorkOrderDir, Filename),

        % Calculate checksum
        WorkOrderJson = jsone:encode(WorkOrder),
        Checksum = calculate_checksum(WorkOrderJson),
        WorkOrderWithChecksum = WorkOrder#{checksum => Checksum},

        % Write to file
        FinalJson = jsone:encode(WorkOrderWithChecksum),
        ok = file:write_file(Path, FinalJson),

        % Persist to ontology (using incremental RDF - Agent 9's work)
        ok = tcps_rdf_incremental:add_work_order_to_ontology(WorkOrderWithChecksum),

        % Update ontology index (Agent 9's work)
        ok = tcps_ontology_index:index_work_order(WorkOrderWithChecksum),

        % Invalidate cache
        ok = tcps_query_cache:invalidate(<<"work_orders_pending">>),

        % Log change
        ok = log_change(work_order, WorkOrderId, #{
            action => created,
            path => list_to_binary(Path),
            timestamp => timestamp_now()
        }),

        {ok, list_to_binary(Path)}
    catch
        Error:Reason:Stack ->
            {error, {Error, Reason, Stack}}
    end.

-spec get_work_order(WorkOrderId :: binary()) -> {ok, work_order()} | {error, term()}.
get_work_order(WorkOrderId) ->
    try
        WorkOrderDir = work_order_dir(),
        Filename = generate_work_order_filename(WorkOrderId),
        Path = filename:join(WorkOrderDir, Filename),

        case file:read_file(Path) of
            {ok, Content} ->
                WorkOrder = jsone:decode(Content, [{object_format, map}]),
                {ok, WorkOrder};
            {error, enoent} ->
                {error, not_found};
            {error, FileReason} ->
                {error, FileReason}
        end
    catch
        ErrClass:Reason ->
            {error, {ErrClass, Reason}}
    end.

-spec list_work_orders() -> [work_order()].
list_work_orders() ->
    WorkOrderDir = work_order_dir(),
    case filelib:is_dir(WorkOrderDir) of
        true ->
            {ok, Files} = file:list_dir(WorkOrderDir),
            JsonFiles = [filename:join(WorkOrderDir, F) || F <- Files,
                         filename:extension(F) =:= ".json"],

            lists:filtermap(
                fun(Path) ->
                    case file:read_file(Path) of
                        {ok, Content} ->
                            WorkOrder = jsone:decode(Content, [{object_format, map}]),
                            {true, WorkOrder};
                        {error, _} ->
                            false
                    end
                end,
                JsonFiles
            );
        false ->
            []
    end.

-spec list_work_orders_by_status(Status :: atom()) -> [work_order()].
list_work_orders_by_status(Status) ->
    AllWorkOrders = list_work_orders(),
    [WO || WO <- AllWorkOrders, maps:get(status, WO) =:= Status].

-spec list_work_orders_by_bucket(Bucket :: atom()) -> [work_order()].
list_work_orders_by_bucket(Bucket) ->
    % Use ontology index for fast lookup (Agent 9's work)
    tcps_ontology_index:get_work_orders_by_bucket(atom_to_binary(Bucket, utf8)).

-spec update_work_order(WorkOrder :: work_order()) -> ok | {error, term()}.
update_work_order(WorkOrder) ->
    case store_work_order(WorkOrder) of
        {ok, _Path} ->
            % Log update
            WorkOrderId = maps:get(id, WorkOrder),
            log_change(work_order, WorkOrderId, #{
                action => updated,
                timestamp => timestamp_now()
            }),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

-spec delete_work_order(WorkOrderId :: binary()) -> ok | {error, term()}.
delete_work_order(WorkOrderId) ->
    WorkOrderDir = work_order_dir(),
    Filename = generate_work_order_filename(WorkOrderId),
    Path = filename:join(WorkOrderDir, Filename),

    case file:delete(Path) of
        ok ->
            % Log deletion
            log_change(work_order, WorkOrderId, #{
                action => deleted,
                timestamp => timestamp_now()
            }),

            % Invalidate cache
            tcps_query_cache:invalidate_all(),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%%%===================================================================
%%% API - Andon Event Storage
%%%===================================================================

-spec store_andon_event(AndonEvent :: andon_event()) -> {ok, binary()} | {error, term()}.
store_andon_event(AndonEvent) ->
    try
        #{id := AndonId} = AndonEvent,

        % Ensure directory exists
        AndonDir = andon_event_dir(),
        ok = filelib:ensure_dir(filename:join(AndonDir, "dummy")),

        % Generate filename
        Filename = generate_andon_filename(AndonId),
        Path = filename:join(AndonDir, Filename),

        % Calculate checksum
        AndonJson = jsone:encode(AndonEvent),
        Checksum = calculate_checksum(AndonJson),
        AndonWithChecksum = AndonEvent#{checksum => Checksum},

        % Write to file
        FinalJson = jsone:encode(AndonWithChecksum),
        ok = file:write_file(Path, FinalJson),

        % Persist to ontology (using incremental RDF)
        ok = tcps_rdf_incremental:add_andon_to_ontology(AndonWithChecksum),

        % Update ontology index
        ok = tcps_ontology_index:index_andon(AndonWithChecksum),

        % Invalidate cache
        ok = tcps_query_cache:invalidate(<<"andon_active">>),

        % Log change
        ok = log_change(andon, AndonId, #{
            action => created,
            path => list_to_binary(Path),
            timestamp => timestamp_now()
        }),

        {ok, list_to_binary(Path)}
    catch
        Error:Reason:Stack ->
            {error, {Error, Reason, Stack}}
    end.

-spec get_andon_event(AndonId :: binary()) -> {ok, andon_event()} | {error, term()}.
get_andon_event(AndonId) ->
    try
        AndonDir = andon_event_dir(),
        Filename = generate_andon_filename(AndonId),
        Path = filename:join(AndonDir, Filename),

        case file:read_file(Path) of
            {ok, Content} ->
                Andon = jsone:decode(Content, [{object_format, map}]),
                {ok, Andon};
            {error, enoent} ->
                {error, not_found};
            {error, FileReason} ->
                {error, FileReason}
        end
    catch
        ErrClass:Reason ->
            {error, {ErrClass, Reason}}
    end.

-spec list_andon_events() -> [andon_event()].
list_andon_events() ->
    AndonDir = andon_event_dir(),
    case filelib:is_dir(AndonDir) of
        true ->
            {ok, Files} = file:list_dir(AndonDir),
            JsonFiles = [filename:join(AndonDir, F) || F <- Files,
                         filename:extension(F) =:= ".json"],

            lists:filtermap(
                fun(Path) ->
                    case file:read_file(Path) of
                        {ok, Content} ->
                            Andon = jsone:decode(Content, [{object_format, map}]),
                            {true, Andon};
                        {error, _} ->
                            false
                    end
                end,
                JsonFiles
            );
        false ->
            []
    end.

-spec list_andon_events_by_status(Status :: atom()) -> [andon_event()].
list_andon_events_by_status(Status) ->
    AllAndons = list_andon_events(),
    [A || A <- AllAndons, maps:get(status, A) =:= Status].

-spec update_andon_event(AndonEvent :: andon_event()) -> ok | {error, term()}.
update_andon_event(AndonEvent) ->
    case store_andon_event(AndonEvent) of
        {ok, _Path} ->
            % Log update
            AndonId = maps:get(id, AndonEvent),
            log_change(andon, AndonId, #{
                action => updated,
                timestamp => timestamp_now()
            }),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%%%===================================================================
%%% API - RDF Ontology Persistence
%%%===================================================================

-spec persist_to_ontology(Receipt :: receipt()) -> ok | {error, term()}.
persist_to_ontology(Receipt) ->
    try
        #{sku_id := SkuId, stage := Stage, timestamp := Timestamp} = Receipt,

        % Generate RDF URI
        ReceiptId = generate_receipt_id(Stage, Timestamp),

        % Build RDF triples
        Triples = [
            {ReceiptId, <<"rdf:type">>, <<"tcps:Receipt">>},
            {ReceiptId, <<"tcps:stage">>, stage_to_uri(Stage)},
            {ReceiptId, <<"tcps:sku">>, sku_to_uri(SkuId)},
            {ReceiptId, <<"tcps:timestamp">>,
             format_datetime_literal(Timestamp)},
            {ReceiptId, <<"tcps:status">>,
             atom_to_binary(maps:get(status, Receipt), utf8)},
            {ReceiptId, <<"tcps:evidence">>,
             format_string_literal(maps:get(evidence, Receipt, <<"">>))}
        ],

        % Append to receipts.ttl
        OntologyFile = ontology_file(receipts),
        ok = append_triples_to_file(OntologyFile, Triples),

        ok
    catch
        Error:Reason:Stack ->
            {error, {Error, Reason, Stack}}
    end.

-spec persist_work_order(WorkOrder :: work_order()) -> ok | {error, term()}.
persist_work_order(WorkOrder) ->
    try
        #{id := Id} = WorkOrder,

        % Build RDF triples
        WoId = <<"tcps:WorkOrder_", Id/binary>>,
        Triples = [
            {WoId, <<"rdf:type">>, <<"tcps:WorkOrder">>},
            {WoId, <<"tcps:id">>, format_string_literal(Id)},
            {WoId, <<"tcps:sku">>, sku_to_uri(maps:get(sku_id, WorkOrder))},
            {WoId, <<"tcps:bucket">>,
             atom_to_binary(maps:get(bucket, WorkOrder), utf8)},
            {WoId, <<"tcps:priority">>,
             integer_to_binary(maps:get(priority, WorkOrder))},
            {WoId, <<"tcps:sla">>,
             integer_to_binary(maps:get(sla, WorkOrder))},
            {WoId, <<"tcps:createdAt">>,
             format_datetime_literal(maps:get(created_at, WorkOrder))}
        ],

        % Append to work_orders.ttl
        OntologyFile = ontology_file(work_orders),
        ok = append_triples_to_file(OntologyFile, Triples),

        % Log change
        ok = log_change(work_order, Id, #{
            action => created,
            timestamp => timestamp_now()
        }),

        ok
    catch
        Error:Reason:Stack ->
            {error, {Error, Reason, Stack}}
    end.

-spec persist_andon(AndonEvent :: andon_event()) -> ok | {error, term()}.
persist_andon(AndonEvent) ->
    try
        #{id := Id} = AndonEvent,

        % Build RDF triples
        AndonId = <<"tcps:Andon_", Id/binary>>,
        Triples = [
            {AndonId, <<"rdf:type">>, <<"tcps:AndonEvent">>},
            {AndonId, <<"tcps:id">>, format_string_literal(Id)},
            {AndonId, <<"tcps:sku">>, sku_to_uri(maps:get(sku_id, AndonEvent))},
            {AndonId, <<"tcps:severity">>,
             atom_to_binary(maps:get(severity, AndonEvent), utf8)},
            {AndonId, <<"tcps:status">>,
             atom_to_binary(maps:get(status, AndonEvent), utf8)},
            {AndonId, <<"tcps:createdAt">>,
             format_datetime_literal(maps:get(created_at, AndonEvent))}
        ],

        % Append to andon_events.ttl
        OntologyFile = ontology_file(andon_events),
        ok = append_triples_to_file(OntologyFile, Triples),

        % Log change
        ok = log_change(andon, Id, #{
            action => created,
            timestamp => timestamp_now()
        }),

        ok
    catch
        Error:Reason:Stack ->
            {error, {Error, Reason, Stack}}
    end.

-spec persist_root_cause(Analysis :: map()) -> ok | {error, term()}.
persist_root_cause(Analysis) ->
    try
        Id = maps:get(id, Analysis, generate_uuid()),

        % Build RDF triples
        RcaId = <<"tcps:RootCause_", Id/binary>>,
        Triples = [
            {RcaId, <<"rdf:type">>, <<"tcps:RootCauseAnalysis">>},
            {RcaId, <<"tcps:id">>, format_string_literal(Id)},
            {RcaId, <<"tcps:problem">>,
             format_string_literal(maps:get(problem, Analysis, <<"">>))},
            {RcaId, <<"tcps:rootCause">>,
             format_string_literal(maps:get(root_cause, Analysis, <<"">>))},
            {RcaId, <<"tcps:countermeasure">>,
             format_string_literal(maps:get(countermeasure, Analysis, <<"">>))},
            {RcaId, <<"tcps:createdAt">>,
             format_datetime_literal(timestamp_now())}
        ],

        % Append to root_cause_analyses.ttl
        OntologyFile = ontology_file(root_cause_analyses),
        ok = append_triples_to_file(OntologyFile, Triples),

        ok
    catch
        Error:Reason:Stack ->
            {error, {Error, Reason, Stack}}
    end.

%%%===================================================================
%%% API - SPARQL Query Interface (using Agent 9's query cache)
%%%===================================================================

-spec query_ontology(SparqlQuery :: binary()) -> {ok, [map()]} | {error, term()}.
query_ontology(SparqlQuery) ->
    query_ontology(SparqlQuery, 60).

-spec query_ontology(SparqlQuery :: binary(), TTL :: integer()) -> {ok, [map()]} | {error, term()}.
query_ontology(SparqlQuery, TTL) ->
    try
        % Load all ontology files
        OntologyFiles = [
            ontology_file(receipts),
            ontology_file(work_orders),
            ontology_file(andon_events),
            ontology_file(root_cause_analyses)
        ],

        % Execute SPARQL query with caching (Agent 9's work)
        case tcps_query_cache:cached_sparql_query(OntologyFiles, SparqlQuery, TTL) of
            {ok, Results} ->
                {ok, Results};
            {error, QueryReason} ->
                {error, QueryReason}
        end
    catch
        Error:Reason:Stack ->
            {error, {Error, Reason, Stack}}
    end.

%%%===================================================================
%%% API - Ontology Rebuild
%%%===================================================================

-spec rebuild_ontology() -> ok | {error, term()}.
rebuild_ontology() ->
    try
        % Clear existing ontology files
        OntologyFiles = [
            ontology_file(receipts),
            ontology_file(work_orders),
            ontology_file(andon_events),
            ontology_file(root_cause_analyses)
        ],

        lists:foreach(fun(File) ->
            case filelib:is_regular(File) of
                true -> file:delete(File);
                false -> ok
            end
        end, OntologyFiles),

        % Reinitialize ontology files
        initialize_ontology_files(),

        % Rebuild from JSON files

        % 1. Load all receipts and add to ontology
        ReceiptsRoot = config(receipts_dir),
        rebuild_receipts_ontology(ReceiptsRoot),

        % 2. Load all work orders and add to ontology
        WorkOrderDir = work_order_dir(),
        rebuild_work_orders_ontology(WorkOrderDir),

        % 3. Load all Andon events and add to ontology
        AndonDir = andon_event_dir(),
        rebuild_andons_ontology(AndonDir),

        % 4. Rebuild indexes (Agent 9's work)
        ok = tcps_ontology_index:rebuild_indexes(),

        % 5. Invalidate all caches
        ok = tcps_query_cache:invalidate_all(),

        ok
    catch
        Error:Reason:Stack ->
            {error, {Error, Reason, Stack}}
    end.

-spec update_ontology(Changes :: map()) -> ok | {error, term()}.
update_ontology(Changes) ->
    % Incremental ontology update (placeholder)
    % TODO: Implement specific ontology updates
    {error, not_implemented}.

-spec query_receipts_by_sku(SkuId :: binary()) -> [receipt()].
query_receipts_by_sku(SkuId) ->
    SkuUri = sku_to_uri(SkuId),
    Query = iolist_to_binary([
        <<"PREFIX tcps: <http://example.org/tcps#>\n">>,
        <<"SELECT ?receipt ?stage ?timestamp ?status ?evidence\n">>,
        <<"WHERE {\n">>,
        <<"  ?receipt rdf:type tcps:Receipt ;\n">>,
        <<"           tcps:sku <", SkuUri/binary, "> ;\n">>,
        <<"           tcps:stage ?stage ;\n">>,
        <<"           tcps:timestamp ?timestamp ;\n">>,
        <<"           tcps:status ?status ;\n">>,
        <<"           tcps:evidence ?evidence .\n">>,
        <<"}\n">>,
        <<"ORDER BY ?timestamp">>
    ]),

    case query_ontology(Query) of
        {ok, Results} ->
            [sparql_result_to_receipt(R) || R <- Results];
        {error, _} ->
            []
    end.

-spec query_work_orders_by_bucket(Bucket :: atom()) -> [work_order()].
query_work_orders_by_bucket(Bucket) ->
    BucketBin = atom_to_binary(Bucket, utf8),
    Query = iolist_to_binary([
        <<"PREFIX tcps: <http://example.org/tcps#>\n">>,
        <<"SELECT ?wo ?id ?sku ?bucket ?priority ?sla ?createdAt\n">>,
        <<"WHERE {\n">>,
        <<"  ?wo rdf:type tcps:WorkOrder ;\n">>,
        <<"      tcps:bucket \"", BucketBin/binary, "\" ;\n">>,
        <<"      tcps:id ?id ;\n">>,
        <<"      tcps:sku ?sku ;\n">>,
        <<"      tcps:bucket ?bucket ;\n">>,
        <<"      tcps:priority ?priority ;\n">>,
        <<"      tcps:sla ?sla ;\n">>,
        <<"      tcps:createdAt ?createdAt .\n">>,
        <<"}\n">>,
        <<"ORDER BY ?priority DESC">>
    ]),

    case query_ontology(Query) of
        {ok, Results} ->
            [sparql_result_to_work_order(R) || R <- Results];
        {error, _} ->
            []
    end.

-spec query_open_andons() -> [andon_event()].
query_open_andons() ->
    Query = <<"PREFIX tcps: <http://example.org/tcps#>\n"
              "SELECT ?andon ?id ?sku ?severity ?status ?createdAt\n"
              "WHERE {\n"
              "  ?andon rdf:type tcps:AndonEvent ;\n"
              "         tcps:status ?status ;\n"
              "         tcps:id ?id ;\n"
              "         tcps:sku ?sku ;\n"
              "         tcps:severity ?severity ;\n"
              "         tcps:createdAt ?createdAt .\n"
              "  FILTER(?status IN (\"open\", \"investigating\"))\n"
              "}\n"
              "ORDER BY ?severity DESC ?createdAt">>,

    case query_ontology(Query) of
        {ok, Results} ->
            [sparql_result_to_andon(R) || R <- Results];
        {error, _} ->
            []
    end.

%%%===================================================================
%%% API - Backup and Restore (ENHANCED)
%%%===================================================================

-spec backup(Type :: full | incremental) -> {ok, binary()} | {error, term()}.
backup(full) ->
    try
        Timestamp = erlang:system_time(second),
        BackupName = io_lib:format("full-~p.tar.gz", [Timestamp]),
        BackupPath = filename:join(config(backup_dir), BackupName),

        % Create tarball of entire priv/tcps/ directory
        SourceDir = "priv/tcps",
        Files = filelib:wildcard(SourceDir ++ "/**/*"),

        % Filter out directories (erl_tar needs files only)
        FilesOnly = [F || F <- Files, filelib:is_regular(F)],

        ok = erl_tar:create(BackupPath, FilesOnly, [compressed]),

        % Generate checksum
        {ok, Bin} = file:read_file(BackupPath),
        Checksum = crypto:hash(sha256, Bin),
        ChecksumHex = binary:encode_hex(Checksum),
        ChecksumPath = BackupPath ++ ".sha256",
        ok = file:write_file(ChecksumPath, ChecksumHex),

        {ok, list_to_binary(BackupPath)}
    catch
        Error:Reason:Stack ->
            {error, {Error, Reason, Stack}}
    end;

backup(incremental) ->
    try
        % Get last backup time
        LastBackupTime = get_last_backup_time(),

        Timestamp = erlang:system_time(second),
        BackupName = io_lib:format("incremental-~p.tar.gz", [Timestamp]),
        BackupPath = filename:join(config(backup_dir), BackupName),

        % Find files modified since last backup
        SourceDir = "priv/tcps",
        AllFiles = filelib:wildcard(SourceDir ++ "/**/*"),
        FilesOnly = [F || F <- AllFiles, filelib:is_regular(F)],

        ModifiedFiles = lists:filter(fun(File) ->
            case file:read_file_info(File) of
                {ok, #file_info{mtime = MTime}} ->
                    FileTime = calendar:datetime_to_gregorian_seconds(MTime),
                    FileTime > LastBackupTime;
                {error, _} ->
                    false
            end
        end, FilesOnly),

        case ModifiedFiles of
            [] ->
                {ok, <<"">>};  % No files to backup
            _ ->
                ok = erl_tar:create(BackupPath, ModifiedFiles, [compressed]),

                % Generate checksum
                {ok, Bin} = file:read_file(BackupPath),
                Checksum = crypto:hash(sha256, Bin),
                ChecksumHex = binary:encode_hex(Checksum),
                ChecksumPath = BackupPath ++ ".sha256",
                ok = file:write_file(ChecksumPath, ChecksumHex),

                % Update last backup time
                set_last_backup_time(Timestamp),

                {ok, list_to_binary(BackupPath)}
        end
    catch
        Error:Reason:Stack ->
            {error, {Error, Reason, Stack}}
    end.

-spec restore(BackupPath :: binary()) -> ok | {error, term()}.
restore(BackupPath) ->
    try
        BackupPathStr = binary_to_list(BackupPath),

        % 1. Verify checksum
        ChecksumPath = BackupPathStr ++ ".sha256",
        {ok, StoredChecksumHex} = file:read_file(ChecksumPath),
        StoredChecksum = binary:decode_hex(StoredChecksumHex),

        {ok, Bin} = file:read_file(BackupPathStr),
        CalculatedChecksum = crypto:hash(sha256, Bin),

        case StoredChecksum =:= CalculatedChecksum of
            false ->
                {error, checksum_mismatch};
            true ->
                % 2. Stop TCPS services (if running)
                stop_tcps_services(),

                % 3. Clear current data
                ok = file:del_dir_r("priv/tcps/"),

                % 4. Extract backup
                ok = erl_tar:extract(BackupPathStr, [compressed, {cwd, "priv"}]),

                % 5. Rebuild indexes
                ok = tcps_ontology_index:rebuild_indexes(),

                % 6. Restart services
                start_tcps_services(),

                ok
        end
    catch
        Error:Reason:Stack ->
            {error, {Error, Reason, Stack}}
    end.

-spec verify_integrity() -> {ok, Report :: map()} | {error, Corruptions :: [map()]}.
verify_integrity() ->
    try
        % 1. Check JSON files parseable
        JsonCheck = verify_all_json_files(),

        % 2. Check RDF ontology valid
        RdfCheck = verify_rdf_ontology(),

        % 3. Check ETS indexes match JSON
        IndexCheck = verify_indexes_match_storage(),

        % 4. Check receipt chains complete
        ChainCheck = verify_receipt_chains(),

        % 5. Compile report
        Report = #{
            json_files => JsonCheck,
            rdf_ontology => RdfCheck,
            indexes => IndexCheck,
            receipt_chains => ChainCheck,
            verified_at => timestamp_now()
        },

        case all_checks_passed(Report) of
            true -> {ok, Report};
            false -> {error, extract_corruptions(Report)}
        end
    catch
        Error:Reason:Stack ->
            {error, [{global_error, {Error, Reason, Stack}}]}
    end.

-spec backup_all(BackupPath :: binary()) -> ok | {error, term()}.
backup_all(BackupPath) ->
    try
        BackupDir = binary_to_list(BackupPath),

        % Create backup directory
        ok = filelib:ensure_dir(filename:join(BackupDir, "dummy")),

        % Generate manifest
        Manifest = #{
            timestamp => timestamp_now(),
            version => <<"1.0.0">>,
            files => []
        },

        % Copy ontology files
        OntologyFiles = [
            ontology_file(receipts),
            ontology_file(work_orders),
            ontology_file(andon_events),
            ontology_file(root_cause_analyses),
            ontology_file(changes)
        ],

        lists:foreach(
            fun(File) ->
                case filelib:is_regular(File) of
                    true ->
                        DestFile = filename:join(BackupDir, filename:basename(File)),
                        {ok, _} = file:copy(File, DestFile);
                    false ->
                        ok
                end
            end,
            OntologyFiles
        ),

        % Copy receipts directory
        ReceiptsRoot = config(receipts_dir),
        BackupReceiptsDir = filename:join(BackupDir, "receipts"),
        case filelib:is_dir(ReceiptsRoot) of
            true ->
                copy_dir_recursive(ReceiptsRoot, BackupReceiptsDir);
            false ->
                ok
        end,

        % Copy work orders directory
        WorkOrderDir = work_order_dir(),
        BackupWorkOrderDir = filename:join(BackupDir, "work_orders"),
        case filelib:is_dir(WorkOrderDir) of
            true ->
                copy_dir_recursive(WorkOrderDir, BackupWorkOrderDir);
            false ->
                ok
        end,

        % Copy andon events directory
        AndonDir = andon_event_dir(),
        BackupAndonDir = filename:join(BackupDir, "andon_events"),
        case filelib:is_dir(AndonDir) of
            true ->
                copy_dir_recursive(AndonDir, BackupAndonDir);
            false ->
                ok
        end,

        % Write manifest
        ManifestPath = filename:join(BackupDir, "manifest.json"),
        ManifestJson = jsone:encode(Manifest),
        ok = file:write_file(ManifestPath, ManifestJson),

        % Create tarball
        TarballPath = BackupPath,
        TarballPathStr = binary_to_list(TarballPath) ++ ".tar.gz",
        ok = erl_tar:create(TarballPathStr, [BackupDir], [compressed]),

        % Clean up temporary directory
        ok = delete_dir_recursive(BackupDir),

        ok
    catch
        Error:Reason:Stack ->
            {error, {Error, Reason, Stack}}
    end.

-spec restore_from_backup(BackupPath :: binary()) -> ok | {error, term()}.
restore_from_backup(BackupPath) ->
    try
        TarballPath = binary_to_list(BackupPath),

        % Extract tarball
        TempDir = filename:join(config(backup_dir), "temp_restore"),
        ok = filelib:ensure_dir(filename:join(TempDir, "dummy")),
        ok = erl_tar:extract(TarballPath, [{cwd, TempDir}, compressed]),

        % Verify manifest
        ManifestPath = filename:join(TempDir, "manifest.json"),
        {ok, ManifestJson} = file:read_file(ManifestPath),
        _Manifest = jsone:decode(ManifestJson, [{object_format, map}]),

        % Restore ontology files
        OntologyDir = config(ontology_dir),
        lists:foreach(
            fun(Basename) ->
                SourceFile = filename:join(TempDir, Basename),
                DestFile = filename:join(OntologyDir, Basename),
                case filelib:is_regular(SourceFile) of
                    true -> {ok, _} = file:copy(SourceFile, DestFile);
                    false -> ok
                end
            end,
            ["receipts.ttl", "work_orders.ttl", "andon_events.ttl",
             "root_cause_analyses.ttl", "changes.ttl"]
        ),

        % Restore receipts
        BackupReceiptsDir = filename:join(TempDir, "receipts"),
        ReceiptsRoot = config(receipts_dir),
        case filelib:is_dir(BackupReceiptsDir) of
            true ->
                copy_dir_recursive(BackupReceiptsDir, ReceiptsRoot);
            false ->
                ok
        end,

        % Validate integrity
        ok = validate_ontology(),

        % Clean up
        ok = delete_dir_recursive(TempDir),

        ok
    catch
        Error:Reason:Stack ->
            {error, {Error, Reason, Stack}}
    end.

%%%===================================================================
%%% API - Ontology Validation
%%%===================================================================

-spec validate_ontology() -> {ok, valid} | {error, [violation()]}.
validate_ontology() ->
    try
        % Run SHACL validation (via Python pyshacl port)
        OntologyFiles = [
            ontology_file(receipts),
            ontology_file(work_orders),
            ontology_file(andon_events),
            ontology_file(root_cause_analyses)
        ],

        Violations = run_shacl_validation(OntologyFiles),

        case Violations of
            [] -> {ok, valid};
            _ -> {error, Violations}
        end
    catch
        Error:Reason:Stack ->
            {error, {Error, Reason, Stack}}
    end.

-spec repair_ontology(Violations :: [violation()]) -> {ok, repaired} | {error, term()}.
repair_ontology(Violations) ->
    try
        % Attempt automatic repairs
        lists:foreach(
            fun(Violation) ->
                repair_violation(Violation)
            end,
            Violations
        ),

        % Re-validate
        case validate_ontology() of
            {ok, valid} -> {ok, repaired};
            {error, RemainingViolations} ->
                {error, {unrepaired, RemainingViolations}}
        end
    catch
        Error:Reason:Stack ->
            {error, {Error, Reason, Stack}}
    end.

%%%===================================================================
%%% API - Change Log
%%%===================================================================

-spec log_change(Entity :: atom(), Id :: binary(), Change :: map()) -> ok.
log_change(Entity, Id, Change) ->
    try
        % Build RDF triple
        ChangeId = generate_uuid(),
        ChangeUri = <<"tcps:Change_", ChangeId/binary>>,

        Triples = [
            {ChangeUri, <<"rdf:type">>, <<"tcps:Change">>},
            {ChangeUri, <<"tcps:entity">>, atom_to_binary(Entity, utf8)},
            {ChangeUri, <<"tcps:entityId">>, format_string_literal(Id)},
            {ChangeUri, <<"tcps:action">>,
             atom_to_binary(maps:get(action, Change), utf8)},
            {ChangeUri, <<"tcps:timestamp">>,
             format_datetime_literal(maps:get(timestamp, Change))}
        ],

        % Append to changes.ttl
        OntologyFile = ontology_file(changes),
        ok = append_triples_to_file(OntologyFile, Triples),

        ok
    catch
        _:_ ->
            ok % Log errors should not crash the system
    end.

-spec get_change_history(Entity :: atom(), Id :: binary()) -> [map()].
get_change_history(Entity, Id) ->
    EntityBin = atom_to_binary(Entity, utf8),
    Query = iolist_to_binary([
        <<"PREFIX tcps: <http://example.org/tcps#>\n">>,
        <<"SELECT ?change ?action ?timestamp\n">>,
        <<"WHERE {\n">>,
        <<"  ?change rdf:type tcps:Change ;\n">>,
        <<"          tcps:entity \"", EntityBin/binary, "\" ;\n">>,
        <<"          tcps:entityId \"", Id/binary, "\" ;\n">>,
        <<"          tcps:action ?action ;\n">>,
        <<"          tcps:timestamp ?timestamp .\n">>,
        <<"}\n">>,
        <<"ORDER BY ?timestamp DESC">>
    ]),

    case query_ontology(Query) of
        {ok, Results} -> Results;
        {error, _} -> []
    end.

%%%===================================================================
%%% API - Export Functions
%%%===================================================================

-spec export_ontology(Format :: turtle | ntriples | jsonld) -> {ok, binary()} | {error, term()}.
export_ontology(Format) ->
    try
        % Load all ontology files
        OntologyFiles = [
            ontology_file(receipts),
            ontology_file(work_orders),
            ontology_file(andon_events),
            ontology_file(root_cause_analyses),
            ontology_file(changes)
        ],

        % Combine all triples
        AllContent = lists:foldl(
            fun(File, Acc) ->
                case file:read_file(File) of
                    {ok, Content} -> <<Acc/binary, Content/binary, "\n">>;
                    {error, _} -> Acc
                end
            end,
            <<"">>,
            OntologyFiles
        ),

        % Convert format if needed
        case Format of
            turtle -> {ok, AllContent};
            ntriples -> convert_to_ntriples(AllContent);
            jsonld -> convert_to_jsonld(AllContent)
        end
    catch
        Error:Reason:Stack ->
            {error, {Error, Reason, Stack}}
    end.

-spec export_receipts(SkuId :: binary(), Format :: json | csv | pdf) -> {ok, binary()} | {error, term()}.
export_receipts(SkuId, Format) ->
    try
        Receipts = get_all_receipts(SkuId),

        case Format of
            json ->
                {ok, jsone:encode(Receipts)};
            csv ->
                {ok, receipts_to_csv(Receipts)};
            pdf ->
                {ok, receipts_to_pdf(Receipts)}
        end
    catch
        Error:Reason:Stack ->
            {error, {Error, Reason, Stack}}
    end.

%%%===================================================================
%%% API - Initialization
%%%===================================================================

-spec init() -> ok.
init() ->
    ensure_dirs(),
    initialize_ontology_files(),
    ok.

-spec ensure_dirs() -> ok.
ensure_dirs() ->
    Dirs = [
        config(ontology_dir),
        config(receipts_dir),
        config(backup_dir)
    ],

    lists:foreach(
        fun(Dir) ->
            ok = filelib:ensure_dir(filename:join(Dir, "dummy"))
        end,
        Dirs
    ),

    ok.

%%%===================================================================
%%% Internal Functions - File Operations
%%%===================================================================

receipt_dir(SkuId) ->
    filename:join([config(receipts_dir), SkuId]).

receipt_path(SkuId, Stage, Timestamp) ->
    Dir = receipt_dir(SkuId),
    Filename = generate_receipt_filename(Stage, Timestamp),
    filename:join(Dir, Filename).

work_order_dir() ->
    "priv/tcps/work_orders".

andon_event_dir() ->
    "priv/tcps/andon_events".

backup_dir() ->
    "priv/tcps/backups".

ontology_file(Type) ->
    Basename = atom_to_list(Type) ++ ".ttl",
    filename:join(config(ontology_dir), Basename).

generate_receipt_filename(Stage, Timestamp) ->
    StageStr = atom_to_list(Stage),
    TimestampStr = binary_to_list(Timestamp),
    lists:flatten(io_lib:format("~s-~s.json", [StageStr, TimestampStr])).

generate_work_order_filename(WorkOrderId) ->
    binary_to_list(WorkOrderId) ++ ".json".

generate_andon_filename(AndonId) ->
    binary_to_list(AndonId) ++ ".json".

generate_receipt_id(Stage, Timestamp) ->
    StageStr = atom_to_binary(Stage, utf8),
    iolist_to_binary([<<"tcps:Receipt_">>, StageStr, <<"_">>, Timestamp]).

%%%===================================================================
%%% Internal Functions - Checksums
%%%===================================================================

calculate_checksum(Data) ->
    Hash = crypto:hash(sha256, Data),
    base64:encode(Hash).

%%%===================================================================
%%% Internal Functions - RDF Generation
%%%===================================================================

sku_to_uri(SkuId) ->
    <<"tcps:SKU_", SkuId/binary>>.

stage_to_uri(Stage) ->
    StageBin = atom_to_binary(Stage, utf8),
    <<"tcps:", StageBin/binary>>.

format_datetime_literal(Timestamp) ->
    <<"\"", Timestamp/binary, "\"^^xsd:dateTime">>.

format_string_literal(Value) ->
    % Escape quotes
    Escaped = binary:replace(Value, <<"\"">>, <<"\\\"">>, [global]),
    <<"\"", Escaped/binary, "\"">>.

append_triples_to_file(File, Triples) ->
    % Ensure file exists with header
    case filelib:is_regular(File) of
        false ->
            Header = <<"@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n"
                       "@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n"
                       "@prefix tcps: <http://example.org/tcps#> .\n\n">>,
            ok = file:write_file(File, Header);
        true ->
            ok
    end,

    % Format triples
    TriplesText = lists:map(
        fun({Subject, Predicate, Object}) ->
            iolist_to_binary([Subject, " ", Predicate, " ", Object, " .\n"])
        end,
        Triples
    ),

    % Append to file
    ok = file:write_file(File, ["\n", TriplesText], [append]).

initialize_ontology_files() ->
    Types = [receipts, work_orders, andon_events, root_cause_analyses, changes],
    lists:foreach(
        fun(Type) ->
            File = ontology_file(Type),
            case filelib:is_regular(File) of
                false -> append_triples_to_file(File, []);
                true -> ok
            end
        end,
        Types
    ).

%%%===================================================================
%%% Internal Functions - SPARQL Execution
%%%===================================================================

execute_sparql_query(OntologyFiles, Query) ->
    % Placeholder: Execute SPARQL via Python rdflib
    % In real implementation, use port to Python script

    % For now, return empty results
    % TODO: Implement via port to Python rdflib
    [].

sparql_result_to_receipt(Result) ->
    #{
        sku_id => maps:get(<<"sku">>, Result, <<"">>),
        stage => binary_to_atom(maps:get(<<"stage">>, Result, <<"unknown">>), utf8),
        timestamp => maps:get(<<"timestamp">>, Result, <<"">>),
        status => binary_to_atom(maps:get(<<"status">>, Result, <<"unknown">>), utf8),
        evidence => maps:get(<<"evidence">>, Result, <<"">>)
    }.

sparql_result_to_work_order(Result) ->
    #{
        id => maps:get(<<"id">>, Result, <<"">>),
        sku_id => maps:get(<<"sku">>, Result, <<"">>),
        bucket => binary_to_atom(maps:get(<<"bucket">>, Result, <<"unknown">>), utf8),
        priority => binary_to_integer(maps:get(<<"priority">>, Result, <<"0">>)),
        sla => binary_to_integer(maps:get(<<"sla">>, Result, <<"0">>)),
        created_at => maps:get(<<"createdAt">>, Result, <<"">>)
    }.

sparql_result_to_andon(Result) ->
    #{
        id => maps:get(<<"id">>, Result, <<"">>),
        sku_id => maps:get(<<"sku">>, Result, <<"">>),
        severity => binary_to_atom(maps:get(<<"severity">>, Result, <<"unknown">>), utf8),
        status => binary_to_atom(maps:get(<<"status">>, Result, <<"unknown">>), utf8),
        created_at => maps:get(<<"createdAt">>, Result, <<"">>),
        root_cause => #{}
    }.

%%%===================================================================
%%% Internal Functions - Validation
%%%===================================================================

run_shacl_validation(_OntologyFiles) ->
    % Placeholder: Run SHACL validation via Python pyshacl
    % In real implementation, use port to Python script

    % For now, return no violations
    % TODO: Implement via port to Python pyshacl
    [].

repair_violation(_Violation) ->
    %% Placeholder: Attempt automatic repair
    %% TODO: Implement repair logic
    ok.

%%%===================================================================
%%% Internal Functions - Export
%%%===================================================================

convert_to_ntriples(TurtleContent) ->
    %% Placeholder: Convert Turtle to N-Triples
    %% TODO: Implement via rdflib
    {ok, TurtleContent}.

convert_to_jsonld(TurtleContent) ->
    %% Placeholder: Convert Turtle to JSON-LD
    %% TODO: Implement via rdflib
    {ok, <<"{}">>}.

receipts_to_csv(Receipts) ->
    Header = <<"SKU ID,Stage,Timestamp,Status,Evidence\n">>,
    Rows = lists:map(
        fun(Receipt) ->
            iolist_to_binary([
                maps:get(sku_id, Receipt, <<"">>), <<",">>,
                erlang:atom_to_binary(maps:get(stage, Receipt, unknown), utf8), <<",">>,
                maps:get(timestamp, Receipt, <<"">>), <<",">>,
                erlang:atom_to_binary(maps:get(status, Receipt, unknown), utf8), <<",">>,
                maps:get(evidence, Receipt, <<"">>), <<"\n">>
            ])
        end,
        Receipts
    ),
    iolist_to_binary([Header | Rows]).

receipts_to_pdf(_Receipts) ->
    %% Placeholder: Generate PDF
    %% TODO: Implement PDF generation
    <<"PDF content">>.

%%%===================================================================
%%% Internal Functions - File Utilities
%%%===================================================================

copy_dir_recursive(Source, Dest) ->
    ok = filelib:ensure_dir(filename:join(Dest, "dummy")),
    {ok, Files} = file:list_dir(Source),

    lists:foreach(
        fun(File) ->
            SourcePath = filename:join(Source, File),
            DestPath = filename:join(Dest, File),

            case filelib:is_dir(SourcePath) of
                true ->
                    copy_dir_recursive(SourcePath, DestPath);
                false ->
                    {ok, _} = file:copy(SourcePath, DestPath)
            end
        end,
        Files
    ),

    ok.

delete_dir_recursive(Dir) ->
    case filelib:is_dir(Dir) of
        true ->
            {ok, Files} = file:list_dir(Dir),
            lists:foreach(
                fun(File) ->
                    Path = filename:join(Dir, File),
                    case filelib:is_dir(Path) of
                        true -> delete_dir_recursive(Path);
                        false -> file:delete(Path)
                    end
                end,
                Files
            ),
            file:del_dir(Dir);
        false ->
            ok
    end.

%%%===================================================================
%%% Internal Functions - Utilities
%%%===================================================================

timestamp_now() ->
    {{Y, M, D}, {H, Min, S}} = calendar:universal_time(),
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                                   [Y, M, D, H, Min, S])).

generate_uuid() ->
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
    iolist_to_binary(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b",
                                   [A, B, C, D, E])).

%%%===================================================================
%%% Internal Functions - Configuration
%%%===================================================================

config(Key) ->
    Defaults = #{
        ontology_dir => "ontology",
        receipts_dir => "priv/receipts",
        backup_dir => "backups",
        rdf_format => turtle,
        auto_backup => daily,
        backup_retention => 30
    },

    case application:get_env(tcps, persistence) of
        {ok, Config} ->
            maps:get(Key, Config, maps:get(Key, Defaults));
        undefined ->
            maps:get(Key, Defaults)
    end.

%%%===================================================================
%%% Stub Functions (TODO: Implement)
%%%===================================================================

rebuild_receipts_ontology(_ReceiptsRoot) ->
    % TODO: Implement receipt ontology rebuilding
    ok.

rebuild_work_orders_ontology(_WorkOrderDir) ->
    % TODO: Implement work order ontology rebuilding
    ok.

rebuild_andons_ontology(_AndonDir) ->
    % TODO: Implement andon ontology rebuilding
    ok.

get_last_backup_time() ->
    % TODO: Implement last backup time tracking
    0.

set_last_backup_time(_Timestamp) ->
    % TODO: Implement backup time persistence
    ok.

stop_tcps_services() ->
    % TODO: Implement service shutdown
    ok.

start_tcps_services() ->
    % TODO: Implement service startup
    ok.

verify_all_json_files() ->
    % TODO: Implement JSON verification
    #{passed => true, details => []}.

verify_rdf_ontology() ->
    % TODO: Implement RDF ontology verification
    #{passed => true, details => []}.

verify_indexes_match_storage() ->
    % TODO: Implement index verification
    #{passed => true, details => []}.

verify_receipt_chains() ->
    % TODO: Implement receipt chain verification
    #{passed => true, details => []}.

all_checks_passed(Report) ->
    % Check if all verification steps passed
    JsonCheck = maps:get(json, Report, #{passed => true}),
    RdfCheck = maps:get(rdf, Report, #{passed => true}),
    IndexCheck = maps:get(indexes, Report, #{passed => true}),
    ChainCheck = maps:get(chains, Report, #{passed => true}),

    maps:get(passed, JsonCheck, true) andalso
    maps:get(passed, RdfCheck, true) andalso
    maps:get(passed, IndexCheck, true) andalso
    maps:get(passed, ChainCheck, true).

extract_corruptions(Report) ->
    % Extract corruption details from verification report
    lists:flatten([
        maps:get(details, maps:get(json, Report, #{details => []}), []),
        maps:get(details, maps:get(rdf, Report, #{details => []}), []),
        maps:get(details, maps:get(indexes, Report, #{details => []}), []),
        maps:get(details, maps:get(chains, Report, #{details => []}), [])
    ]).
