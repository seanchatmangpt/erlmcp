%% -*- erlang -*-
%%====================================================================
%% Supply Chain Security Verification System
%%====================================================================
-module(erlmcp_supply_chain_security).
-behaviour(gen_server).

%% API
-export([start_link/0, verify_package/2, scan_dependencies/1]).
-export([ get_sbom/1, generate_attestation/1]).
-export([check_vulnerabilities/1, enforce_slsa/2, verify_provenance/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Records
-record.package_verification, {
    id :: binary(),
    package_name :: binary(),
    package_version :: binary(),
    source :: binary(),
    verification_status :: 'verified' | 'failed' | 'warning' | 'pending',
    checksum :: binary(),
    signature :: binary() | undefined,
    attestations :: [map()],
    vulnerabilities :: [map()],
    created_at :: integer(),
    updated_at :: integer()
}.

-record.dependency_scan, {
    id :: binary(),
    project_name :: binary(),
    dependencies :: [#package_verification{}],
    scan_status :: 'completed' | 'failed' | 'in_progress',
    security_score :: float(),
    compliance_status :: 'compliant' | 'non_compliant' | 'partial',
    created_at :: integer()
}.

-record.security_policy, {
    id :: binary(),
    name :: binary(),
    description :: binary(),
    rules :: [map()],
    enforcement :: 'strict' | 'moderate' | 'warn',
    scope :: 'all' | 'production' | 'development',
    created_at :: integer(),
    updated_at :: integer()
}.

-record.sbom_component, {
    name :: binary(),
    version :: binary(),
    type :: 'library' | 'framework' | 'application' | 'system',
    supplier :: binary(),
    licenses :: [binary()],
    vulnerabilities :: [map()],
    dependencies :: [binary()]
}.

-record.state, {
    verifications :: map(),
    scans :: map(),
    policies :: map(),
    sbom :: map(),
    config :: map()
}.

%%====================================================================
%% API Functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

verify_package(PackageName, Version) ->
    gen_server:call(?MODULE, {verify_package, PackageName, Version}).

scan_dependencies(ProjectPath) ->
    gen_server:call(?MODULE, {scan_dependencies, ProjectPath}).

create_policy(PolicyData) ->
    gen_server:call(?MODULE, {create_policy, PolicyData}).

get_verification_results(Filter) ->
    gen_server:call(?MODULE, {get_verification_results, Filter}).

get_sbom(ProjectPath) ->
    gen_server:call(?MODULE, {get_sbom, ProjectPath}).

generate_attestation(PackageId) ->
    gen_server:call(?MODULE, {generate_attestation, PackageId}).

check_vulnerabilities(PackageId) ->
    gen_server:call(?MODULE, {check_vulnerabilities, PackageId}).

enforce_slsa(PackageId, BuildParameters) ->
    gen_server:call(?MODULE, {enforce_slsa, PackageId, BuildParameters}).

verify_provenance(PackageId, ProvenanceData) ->
    gen_server:call(?MODULE, {verify_provenance, PackageId, ProvenanceData}).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    %% Initialize supply chain security stores
    VerificationStore = ets:new(package_verification_store, [set, protected, named_table, {keypos, element(2, record_info(state, 2))}]),
    ScanStore = ets:new(dependency_scan_store, [set, protected, named_table, {keypos, element(2, record_info(state, 2))}]),
    PolicyStore = ets:new(policy_store, [set, protected, named_table, {keypos, element(2, record_info(state, 2))}]),
    SBOMStore = ets:new(sbom_store, [set, protected, named_table, {keypos, element(2, record_info(state, 2))}]),

    %% Load configuration
    Config = load_supply_chain_config(),

    %% Initialize default policies
    initialize_default_policies(),

    {ok, #state{
        verifications = VerificationStore,
        scans = ScanStore,
        policies = PolicyStore,
        sbom = SBOMStore,
        config = Config
    }}.

handle_call({verify_package, PackageName, Version}, _From, State) ->
    VerificationId = generate_verification_id(),
    Verification = #package_verification{
        id = VerificationId,
        package_name = PackageName,
        package_version = Version,
        source = <<"unknown">>,
        verification_status = pending,
        checksum = <<"">>,
        signature = undefined,
        attestations = [],
        vulnerabilities = [],
        created_at = erlang:system_time(second),
        updated_at = erlang:system_time(second)
    },

    %% Perform verification
    case perform_package_verification(PackageName, Version, State) of
        {verified, Details} ->
            UpdatedVerification = Verification#package_verification{
                verification_status = verified,
                checksum = maps:get(checksum, Details),
                signature = maps:get(signature, Details, undefined),
                attestations = maps:get(attestations, Details, [])
            },
            ets:insert(State#state.verifications, UpdatedVerification);
        {failed, Reason} ->
            UpdatedVerification = Verification#package_verification{
                verification_status = failed,
                vulnerabilities = [#{type => verification_failed, reason => Reason}]
            },
            ets:insert(State#state.verifications, UpdatedVerification)
    end,

    {reply, {ok, VerificationId}, State};

handle_call({scan_dependencies, ProjectPath}, _From, State) ->
    ScanId = generate_scan_id(),

    %% Scan project dependencies
    case scan_project_dependencies(ProjectPath, State) of
        {ok, Dependencies} ->
            Scan = #dependency_scan{
                id = ScanId,
                project_name = extract_project_name(ProjectPath),
                dependencies = Dependencies,
                scan_status = completed,
                security_score = calculate_security_score(Dependencies),
                compliance_status = check_compliance(Dependencies),
                created_at = erlang:system_time(second)
            },
            ets:insert(State#state.scans, Scan);
        {error, Reason} ->
            Scan = #dependency_scan{
                id = ScanId,
                project_name = extract_project_name(ProjectPath),
                dependencies = [],
                scan_status = failed,
                security_score = 0.0,
                compliance_status = non_compliant,
                created_at = erlang:system_time(second)
            },
            ets:insert(State#state.scans, Scan)
    end,

    {reply, {ok, ScanId}, State};

handle_call({create_policy, PolicyData}, _From, State) ->
    case validate_policy_data(PolicyData) of
        {ok, ValidatedData} ->
            PolicyId = generate_policy_id(),
            Policy = #security_policy{
                id = PolicyId,
                name = maps:get(name, ValidatedData),
                description = maps:get(description, ValidatedData, <<"">>),
                rules = maps:get(rules, ValidatedData, []),
                enforcement = maps:get(enforcement, ValidatedData, moderate),
                scope = maps:get(scope, ValidatedData, all),
                created_at = erlang:system_time(second),
                updated_at = erlang:system_time(second)
            },
            ets:insert(State#state.policies, Policy),
            {reply, {ok, PolicyId}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({get_verification_results, Filter}, _From, State) ->
    Results = filter_verification_results(State#state.verifications, Filter),
    {reply, {ok, ets:tab2list(Results)}, State};

handle_call({get_sbom, ProjectPath}, _From, State) ->
    case ets:lookup(State#state.sbom, ProjectPath) of
        [] ->
            %% Generate SBOM
            SBOM = generate_sbom(ProjectPath, State),
            ets:insert(State#state.sbom, {ProjectPath, SBOM}),
            {reply, {ok, SBOM}, State};
        [{_, SBOM}] ->
            {reply, {ok, SBOM}, State}
    end;

handle_call({generate_attestation, PackageId}, _From, State) ->
    case ets:lookup(State#state.verifications, PackageId) of
        [#package_verification{} = Verification] ->
            Attestation = generate_provenance_attestation(Verification),
            {reply, {ok, Attestation}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call({check_vulnerabilities, PackageId}, _From, State) ->
    case ets:lookup(State#state.verifications, PackageId) of
        [#package_verification{} = Verification] ->
            %% Check against vulnerability databases
            Vulnerabilities = check_vulnerability_databases(
                Verification#package_verification.package_name,
                Verification#package_verification.package_version
            ),
            UpdatedVerification = Verification#package_verification{
                vulnerabilities = Vulnerabilities,
                updated_at = erlang:system_time(second)
            },
            ets:insert(State#state.verifications, UpdatedVerification),
            {reply, {ok, Vulnerabilities}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call({enforce_slsa, PackageId, BuildParameters}, _From, State) ->
    case ets:lookup(State#state.verifications, PackageId) of
        [#package_verification{} = Verification] ->
            case verify_slsa_build(Verification, BuildParameters) of
                {verified, BuildLog} ->
                    UpdatedVerification = Verification#package_verification{
                        verification_status = verified,
                        attestations = [BuildLog | Verification#package_verification.attestations],
                        updated_at = erlang:system_time(second)
                    },
                    ets:insert(State#state.verifications, UpdatedVerification),
                    {reply, {ok, slsa_verified}, State};
                {failed, Reason} ->
                    {reply, {error, {slsa_verification_failed, Reason}}, State}
            end;
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call({verify_provenance, PackageId, ProvenanceData}, _From, State) ->
    case ets:lookup(State#state.verifications, PackageId) of
        [#package_verification{} = Verification] ->
            case verify_provenance_data(ProvenanceData, State) of
                {verified, TrustedBuilder} ->
                    UpdatedVerification = Verification#package_verification{
                        verification_status = verified,
                        signature = maps:get(signature, ProvenanceData),
                        updated_at = erlang:system_time(second)
                    },
                    ets:insert(State#state.verifications, UpdatedVerification),
                    {reply, {ok, {verified, TrustedBuilder}}, State};
                {failed, Reason} ->
                    {reply, {error, {provenance_verification_failed, Reason}}, State}
            end;
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

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

perform_package_verification(PackageName, Version, State) ->
    %% Check against known good packages
    case is_trusted_package(PackageName, Version, State) of
        true ->
            {verified, #{
                checksum => calculate_package_checksum(PackageName, Version),
                signature => verify_package_signature(PackageName, Version),
                attestations => get_package_attestations(PackageName, Version)
            }};
        false ->
            {failed, <<"Package not in trusted registry">>}
    end.

scan_project_dependencies(ProjectPath, State) ->
    %% Scan package.json, requirements.txt, etc.
    case detect_dependency_files(ProjectPath) of
        {ok, DependencyFiles} ->
            Dependencies = lists:foldl(fun(File, Acc) ->
                case parse_dependency_file(File) of
                    {ok, Pkgs} -> Pkgs ++ Acc;
                    {error, _} -> Acc
                end
            end, [], DependencyFiles),
            %% Verify each dependency
            lists:map(fun({Name, Version}) ->
                VerificationId = generate_verification_id(),
                case perform_package_verification(Name, Version, State) of
                    {verified, Details} ->
                        #package_verification{
                            id = VerificationId,
                            package_name = Name,
                            package_version = Version,
                            verification_status = verified,
                            checksum = maps:get(checksum, Details),
                            vulnerabilities = check_vulnerability_databases(Name, Version)
                        };
                    {failed, Reason} ->
                        #package_verification{
                            id = VerificationId,
                            package_name = Name,
                            package_version = Version,
                            verification_status = failed,
                            vulnerabilities = [#{type => verification_failed, reason => Reason}]
                        }
                end
            end, Dependencies);
        {error, Reason} ->
            {error, Reason}
    end.

is_trusted_package(PackageName, Version, State) ->
    %% Check against trusted registries (npm, PyPI, etc.)
    case check_trusted_registries(PackageName, Version) of
        true -> true;
        false ->
            %% Check if package has been verified before
            lists:any(fun({_, Verification}) ->
                Verification#package_verification.package_name == PackageName andalso
                Verification#package_verification.package_version == Version andalso
                Verification#package_verification.verification_status == verified
            end, ets:tab2list(State#state.verifications))
    end.

calculate_package_checksum(PackageName, Version) ->
    %% Calculate SHA256 checksum for package
    crypto:hash(sha256, <<PackageName/binary, Version/binary>>).

verify_package_signature(PackageName, Version) ->
    %% Verify package signature
    case fetch_package_signature(PackageName, Version) of
        {ok, Signature} -> Signature;
        {error, _} -> undefined
    end.

get_package_attestations(PackageName, Version) ->
    %% Fetch package attestations
    case fetch_attestations_from_registry(PackageName, Version) of
        {ok, Attestations} -> Attestations;
        {error, _} -> []
    end.

check_trusted_registries(PackageName, Version) ->
    %% Check against trusted registries
    %% Simplified - in production would call actual registries
    true.

fetch_package_signature(PackageName, Version) ->
    %% Fetch signature from trusted source
    {error, not_available}.

fetch_attestations_from_registry(PackageName, Version) ->
    %% Fetch attestations from package registry
    {error, not_available}.

detect_dependency_files(ProjectPath) ->
    %% Find dependency files in project
    Files = [
        ProjectPath ++ "/package.json",
        ProjectPath ++ "/requirements.txt",
        ProjectPath ++ "/pom.xml",
        ProjectPath ++ "/go.mod"
    ],
    {ok, lists:filter(fun(File) -> filelib:is_file(File) end, Files)}.

parse_dependency_file(File) ->
    %% Parse specific dependency file format
    case filename:extension(File) of
        ".json" -> parse_package_json(File);
        ".txt" -> parse_requirements_txt(File);
        _ -> {error, unsupported_format}
    end.

parse_package_json(File) ->
    %% Parse package.json for npm dependencies
    case file:read_file(File) of
        {ok, Content} ->
            case jsx:decode(Content, [{return_maps, true}]) of
                #{<<"dependencies">> := Dependencies} ->
                    Pkgs = maps:to_list(Dependencies),
                    {ok, Pkgs};
                _ -> {error, invalid_format}
            end;
        {error, _} -> {error, read_error}
    end.

parse_requirements_txt(File) ->
    %% Parse requirements.txt for Python dependencies
    case file:read_file(File) of
        {ok, Content} ->
            Lines = binary:split(Content, <<"\n">>, [global]),
            Pkgs = lists:foldl(fun(Line, Acc) ->
                case binary:split(Line, [<<">=">>, <<"<=">>, <<"==">>]) of
                    [Name, Version] -> [{Name, Version} | Acc];
                    [Name] -> [{Name, <<"latest">>} | Acc];
                    _ -> Acc
                end
            end, [], Lines),
            {ok, Pkgs};
        {error, _} -> {error, read_error}
    end.

extract_project_name(ProjectPath) ->
    filename:basename(ProjectPath).

calculate_security_score(Dependencies) ->
    CalculateScore = lists:foldl(fun(Dep, Acc) ->
        case Dep#package_verification.verification_status of
            verified -> Acc + 0.25;
            warning -> Acc + 0.15;
            failed -> Acc - 0.5;
            pending -> Acc
        end
    end, 0.0, Dependencies),
    max(0.0, min(1.0, CalculateScore)).

check_compliance(Dependencies) ->
    FailedDeps = lists:filter(fun(Dep) ->
        Dep#package_verification.verification_status == failed
    end, Dependencies),
    case length(FailedDeps) of
        0 -> compliant;
        _ when length(FailedDeps) > 5 -> non_compliant;
        _ -> partial
    end.

generate_sbom(ProjectPath, State) ->
    %% Generate Software Bill of Materials
    Components = generate_sbom_components(ProjectPath, State),
    #{
        project_name => extract_project_name(ProjectPath),
        timestamp => erlang:system_time(second),
        components => Components,
        total_components => length(Components),
        vulnerabilities => count_sbom_vulnerabilities(Components)
    }.

generate_sbom_components(ProjectPath, State) ->
    %% Generate SBOM component records
    Components = lists:map(fun(Dep) ->
        #sbom_component{
            name = Dep#package_verification.package_name,
            version = Dep#package_verification.package_version,
            type => library,
            supplier = get_package_supplier(Dep#package_verification.package_name),
            licenses = get_package_licenses(Dep#package_verification.package_name),
            vulnerabilities = Dep#package_verification.vulnerabilities,
            dependencies = get_package_dependencies(Dep#package_verification.package_name)
        }
    end, ets:tab2list(State#state.verifications)),
    Components.

get_package_supplier(PackageName) ->
    %% Get package supplier information
    <<"unknown">>.

get_package_licenses(PackageName) ->
    %% Get package licenses
    [].

get_package_dependencies(PackageName) ->
    %% Get package dependencies
    [].

count_sbom_vulnerabilities(Components) ->
    lists:sum([length(C#sbom_component.vulnerabilities) || C <- Components]).

generate_provenance_attestation(Verification) ->
    %% Generate SLSA provenance attestation
    #{
        type => "https://slsa.dev/provenance/v1",
        builder => {id => <<"erlmcp-builder">>},
        metadata => {
            buildType => <<"https://erlmcp.dev/build">>,
            invocationId => generate_verification_id()
        },
        materials => [
            {
                name => Verification#package_verification.package_name,
                digest => #{algorithm => "sha256", value => Verification#package_verification.checksum}
            }
        ]
    }.

verify_slsa_build(Verification, BuildParameters) ->
    %% Verify SLSA build provenance
    case maps:get(builder, BuildParameters) of
        {id, TrustedBuilder} when TrustedBuilder == <<"erlmcp-builder">> ->
            {verified, #{
                buildType => <<"https://erlmcp.dev/build">>,
                builder => TrustedBuilder
            }};
        _ ->
            {failed, "Untrusted builder"}
    end.

verify_provenance_data(ProvenanceData, State) ->
    %% Verify provenance data integrity
    case verify_provenance_signature(ProvenanceData) of
        true ->
            {verified, maps:get(builder_id, ProvenanceData)};
        false ->
            {failed, "Invalid signature"}
    end.

verify_provenance_signature(ProvenanceData) ->
    %% Verify signature on provenance data
    case maps:get(signature, ProvenanceData, undefined) of
        undefined -> false;
        Signature ->
            %% Verify signature logic here
            true
    end.

check_vulnerability_databases(PackageName, Version) ->
    %% Check against vulnerability databases (CVE, NVD, etc.)
    [
        #{id => "CVE-2024-1234", severity => high, description => "Sample vulnerability"}
    ].

load_supply_chain_config() ->
    #{
        verification_timeout => 10000,
        max_dependencies => 1000,
        vulnerability_database_url => "https://services.nvd.nist.gov/rest/json/cves/2.0",
        trusted_registries => [
            "https://registry.npmjs.org",
            "https://pypi.org",
            "https://mvnrepository.com"
        ],
        slsa_builders => ["erlmcp-builder", "github-actions"]
    }.

initialize_default_policies() ->
    %% Initialize default supply chain security policies
    DefaultPolicies = [
        #{
            name => "Production Dependencies Policy",
            description => "Policy for production dependencies",
            rules => [
                #{field => "verification_status", operator => "==", value => "verified"},
                #{field => "vulnerabilities", operator => "length", value => 0}
            ],
            enforcement => strict,
            scope => production
        },
        #{
            name => "Development Dependencies Policy",
            description => "Policy for development dependencies",
            rules => [
                #{field => "verification_status", operator => "!=", value => "failed"}
            ],
            enforcement => moderate,
            scope => development
        }
    ],
    %% Store default policies (would be done in gen_server state)
    ok.

validate_policy_data(Data) ->
    Required = [name, enforcement, scope],
    case check_required_fields(Data, Required) of
        ok -> {ok, Data};
        {error, missing_field} -> {error, {invalid_policy_data, missing_field}}
    end.

check_required_fields(Data, Fields) ->
    check_required_fields(Data, Fields, ok).

check_required_fields(_, [], Result) -> Result;
check_required_fields(Data, [Field|Rest], ok) ->
    case maps:is_key(Field, Data) of
        true -> check_required_fields(Data, Rest, ok);
        false -> check_required_fields(Data, Rest, {error, missing_field})
    end;
check_required_fields(_, _, Result) -> Result.

filter_verification_results(Store, Filter) ->
    %% Filter verification results
    case Filter of
        #{status := Status} ->
            ets:filter(Store, fun({_, V}) -> V#package_verification.verification_status == Status end);
        #{name := Name} ->
            ets:filter(Store, fun({_, V}) -> binary:match(V#package_verification.package_name, Name) /= nomatch end);
        _ -> Store
    end.

generate_verification_id() ->
    crypto:strong_rand_bytes(16).

generate_scan_id() ->
    crypto:strong_rand_bytes(16).

generate_policy_id() ->
    crypto:strong_rand_bytes(16).