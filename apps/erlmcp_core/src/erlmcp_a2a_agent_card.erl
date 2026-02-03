%%%-------------------------------------------------------------------
%%% @doc A2A Agent Card Management Module
%%%
%%% This module manages agent discovery through Agent Cards. It provides
%%% functionality for creating, validating, storing, and retrieving
%%% agent cards, as well as managing agent skills, interfaces, security
%%% schemes, and JWS signatures.
%%%
%%% The Agent Card is the primary mechanism for agent discovery in the
%%% A2A protocol, allowing clients to learn about an agent's capabilities,
%%% supported interfaces, and security requirements.
%%%
%%% Key features:
%%% - Agent card creation and validation
%%% - Extended agent card support (authenticated access)
%%% - Skill registration and management
%%% - Interface registration (URL, protocol binding, version)
%%% - Security scheme configuration
%%% - JWS signature support for card authenticity
%%% - Provider information management
%%% - Capability declaration
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_a2a_agent_card).

-behaviour(gen_server).

-include("erlmcp_a2a.hrl").

%% API exports
-export([
    start_link/0,
    start_link/1,
    stop/0,

    %% Agent card management
    create_agent_card/1,
    validate_agent_card/1,
    get_agent_card/0,
    get_extended_agent_card/0,
    set_agent_card/1,
    set_extended_agent_card/1,

    %% Skill management
    register_skill/2,
    unregister_skill/1,
    get_skill/1,
    list_skills/0,

    %% Capability management
    update_capabilities/1,
    get_capabilities/0,

    %% Interface management
    register_interface/1,
    unregister_interface/1,
    list_interfaces/0,

    %% Security scheme management
    add_security_scheme/2,
    remove_security_scheme/1,
    get_security_scheme/1,
    list_security_schemes/0,

    %% Signature management
    sign_agent_card/2,
    verify_agent_card_signature/2,

    %% Provider management
    set_provider/1,
    get_provider/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% Internal state
-record(state, {
    agent_card :: #a2a_agent_card{} | undefined,
    extended_agent_card :: #a2a_agent_card{} | undefined,
    skills :: #{binary() => #a2a_agent_skill{}},
    skill_handlers :: #{binary() => fun()},
    interfaces :: [#a2a_agent_interface{}],
    security_schemes :: #{binary() => #a2a_security_scheme{}},
    provider :: #a2a_agent_provider{} | undefined,
    capabilities :: #a2a_agent_capabilities{},
    signatures :: [#a2a_agent_card_signature{}]
}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the agent card manager with default options
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start the agent card manager with options
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%% @doc Stop the agent card manager
-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%% @doc Create an agent card from options map
%% Options:
%%   - name: binary() - Required agent name
%%   - description: binary() - Required agent description
%%   - version: binary() - Required agent version
%%   - interfaces: [map()] - Supported interfaces
%%   - provider: map() - Provider information
%%   - capabilities: map() - Agent capabilities
%%   - skills: [map()] - Agent skills
%%   - default_input_modes: [binary()] - Default input MIME types
%%   - default_output_modes: [binary()] - Default output MIME types
%%   - documentation_url: binary() - Documentation URL
%%   - icon_url: binary() - Icon URL
%%   - security_schemes: map() - Security schemes
%%   - security_requirements: [map()] - Security requirements
-spec create_agent_card(map()) -> {ok, #a2a_agent_card{}} | {error, term()}.
create_agent_card(Options) when is_map(Options) ->
    try
        %% Extract required fields
        Name = maps:get(name, Options),
        Description = maps:get(description, Options),
        Version = maps:get(version, Options),

        %% Parse interfaces
        InterfacesMaps = maps:get(interfaces, Options, []),
        Interfaces = [parse_interface(I) || I <- InterfacesMaps],

        %% Validate at least one interface
        case Interfaces of
            [] -> throw({error, missing_interfaces});
            _ -> ok
        end,

        %% Parse provider
        Provider = case maps:get(provider, Options, undefined) of
            undefined -> undefined;
            ProvMap -> parse_provider(ProvMap)
        end,

        %% Parse capabilities
        Capabilities = parse_capabilities(maps:get(capabilities, Options, #{})),

        %% Parse skills
        SkillsMaps = maps:get(skills, Options, []),
        Skills = [parse_skill(S) || S <- SkillsMaps],

        %% Parse security schemes
        SecuritySchemes = parse_security_schemes(
            maps:get(security_schemes, Options, undefined)
        ),

        %% Build the agent card
        Card = #a2a_agent_card{
            name = Name,
            description = Description,
            supported_interfaces = Interfaces,
            provider = Provider,
            version = Version,
            documentation_url = maps:get(documentation_url, Options, undefined),
            capabilities = Capabilities,
            security_schemes = SecuritySchemes,
            security_requirements = maps:get(security_requirements, Options, undefined),
            default_input_modes = maps:get(default_input_modes, Options,
                                           [?A2A_MIME_TEXT_PLAIN]),
            default_output_modes = maps:get(default_output_modes, Options,
                                            [?A2A_MIME_TEXT_PLAIN]),
            skills = Skills,
            signatures = undefined,
            icon_url = maps:get(icon_url, Options, undefined)
        },

        %% Validate the card
        case validate_agent_card(Card) of
            ok -> {ok, Card};
            {error, _} = Err -> Err
        end
    catch
        throw:Error -> Error;
        error:{badkey, Key} -> {error, {missing_required_field, Key}};
        _:Reason -> {error, {invalid_options, Reason}}
    end.

%% @doc Validate an agent card structure
-spec validate_agent_card(#a2a_agent_card{}) -> ok | {error, term()}.
validate_agent_card(#a2a_agent_card{name = undefined}) ->
    {error, missing_name};
validate_agent_card(#a2a_agent_card{name = Name}) when not is_binary(Name) ->
    {error, {invalid_name, Name}};
validate_agent_card(#a2a_agent_card{description = undefined}) ->
    {error, missing_description};
validate_agent_card(#a2a_agent_card{description = Desc}) when not is_binary(Desc) ->
    {error, {invalid_description, Desc}};
validate_agent_card(#a2a_agent_card{version = undefined}) ->
    {error, missing_version};
validate_agent_card(#a2a_agent_card{version = Ver}) when not is_binary(Ver) ->
    {error, {invalid_version, Ver}};
validate_agent_card(#a2a_agent_card{supported_interfaces = []}) ->
    {error, missing_interfaces};
validate_agent_card(#a2a_agent_card{supported_interfaces = undefined}) ->
    {error, missing_interfaces};
validate_agent_card(#a2a_agent_card{capabilities = undefined}) ->
    {error, missing_capabilities};
validate_agent_card(#a2a_agent_card{default_input_modes = []}) ->
    {error, missing_default_input_modes};
validate_agent_card(#a2a_agent_card{default_input_modes = undefined}) ->
    {error, missing_default_input_modes};
validate_agent_card(#a2a_agent_card{default_output_modes = []}) ->
    {error, missing_default_output_modes};
validate_agent_card(#a2a_agent_card{default_output_modes = undefined}) ->
    {error, missing_default_output_modes};
validate_agent_card(#a2a_agent_card{skills = undefined}) ->
    {error, missing_skills};
validate_agent_card(#a2a_agent_card{supported_interfaces = Interfaces} = Card) ->
    %% Validate interfaces
    case validate_interfaces(Interfaces) of
        ok ->
            %% Validate skills
            case validate_skills(Card#a2a_agent_card.skills) of
                ok -> ok;
                {error, _} = Err -> Err
            end;
        {error, _} = Err ->
            Err
    end;
validate_agent_card(_) ->
    {error, invalid_agent_card}.

%% @doc Get the current agent card (public, unauthenticated access)
-spec get_agent_card() -> {ok, #a2a_agent_card{}} | {error, not_configured}.
get_agent_card() ->
    gen_server:call(?MODULE, get_agent_card).

%% @doc Get the extended agent card (authenticated access only)
-spec get_extended_agent_card() -> {ok, #a2a_agent_card{}} | {error, term()}.
get_extended_agent_card() ->
    gen_server:call(?MODULE, get_extended_agent_card).

%% @doc Set the agent card
-spec set_agent_card(#a2a_agent_card{}) -> ok | {error, term()}.
set_agent_card(Card) ->
    case validate_agent_card(Card) of
        ok -> gen_server:call(?MODULE, {set_agent_card, Card});
        {error, _} = Err -> Err
    end.

%% @doc Set the extended agent card
-spec set_extended_agent_card(#a2a_agent_card{}) -> ok | {error, term()}.
set_extended_agent_card(Card) ->
    case validate_agent_card(Card) of
        ok -> gen_server:call(?MODULE, {set_extended_agent_card, Card});
        {error, _} = Err -> Err
    end.

%% @doc Register a skill with the agent
-spec register_skill(#a2a_agent_skill{}, fun()) -> ok | {error, term()}.
register_skill(#a2a_agent_skill{} = Skill, Handler) when is_function(Handler) ->
    case validate_skill(Skill) of
        ok -> gen_server:call(?MODULE, {register_skill, Skill, Handler});
        {error, _} = Err -> Err
    end;
register_skill(SkillMap, Handler) when is_map(SkillMap), is_function(Handler) ->
    try
        Skill = parse_skill(SkillMap),
        register_skill(Skill, Handler)
    catch
        _:Reason -> {error, {invalid_skill, Reason}}
    end.

%% @doc Unregister a skill by ID
-spec unregister_skill(binary()) -> ok | {error, not_found}.
unregister_skill(SkillId) when is_binary(SkillId) ->
    gen_server:call(?MODULE, {unregister_skill, SkillId}).

%% @doc Get a skill by ID
-spec get_skill(binary()) -> {ok, #a2a_agent_skill{}} | {error, not_found}.
get_skill(SkillId) when is_binary(SkillId) ->
    gen_server:call(?MODULE, {get_skill, SkillId}).

%% @doc List all registered skills
-spec list_skills() -> [#a2a_agent_skill{}].
list_skills() ->
    gen_server:call(?MODULE, list_skills).

%% @doc Update agent capabilities
-spec update_capabilities(map() | #a2a_agent_capabilities{}) -> ok.
update_capabilities(#a2a_agent_capabilities{} = Caps) ->
    gen_server:call(?MODULE, {update_capabilities, Caps});
update_capabilities(CapsMap) when is_map(CapsMap) ->
    Caps = parse_capabilities(CapsMap),
    update_capabilities(Caps).

%% @doc Get current capabilities
-spec get_capabilities() -> #a2a_agent_capabilities{}.
get_capabilities() ->
    gen_server:call(?MODULE, get_capabilities).

%% @doc Register an interface
-spec register_interface(#a2a_agent_interface{} | map()) -> ok | {error, term()}.
register_interface(#a2a_agent_interface{} = Interface) ->
    case validate_interface(Interface) of
        ok -> gen_server:call(?MODULE, {register_interface, Interface});
        {error, _} = Err -> Err
    end;
register_interface(InterfaceMap) when is_map(InterfaceMap) ->
    try
        Interface = parse_interface(InterfaceMap),
        register_interface(Interface)
    catch
        _:Reason -> {error, {invalid_interface, Reason}}
    end.

%% @doc Unregister an interface by URL
-spec unregister_interface(binary()) -> ok | {error, not_found}.
unregister_interface(Url) when is_binary(Url) ->
    gen_server:call(?MODULE, {unregister_interface, Url}).

%% @doc List all registered interfaces
-spec list_interfaces() -> [#a2a_agent_interface{}].
list_interfaces() ->
    gen_server:call(?MODULE, list_interfaces).

%% @doc Add a security scheme
-spec add_security_scheme(binary(), #a2a_security_scheme{} | map()) -> ok | {error, term()}.
add_security_scheme(Name, #a2a_security_scheme{} = Scheme) when is_binary(Name) ->
    gen_server:call(?MODULE, {add_security_scheme, Name, Scheme});
add_security_scheme(Name, SchemeMap) when is_binary(Name), is_map(SchemeMap) ->
    try
        Scheme = parse_security_scheme(SchemeMap),
        add_security_scheme(Name, Scheme)
    catch
        _:Reason -> {error, {invalid_security_scheme, Reason}}
    end.

%% @doc Remove a security scheme by name
-spec remove_security_scheme(binary()) -> ok | {error, not_found}.
remove_security_scheme(Name) when is_binary(Name) ->
    gen_server:call(?MODULE, {remove_security_scheme, Name}).

%% @doc Get a security scheme by name
-spec get_security_scheme(binary()) -> {ok, #a2a_security_scheme{}} | {error, not_found}.
get_security_scheme(Name) when is_binary(Name) ->
    gen_server:call(?MODULE, {get_security_scheme, Name}).

%% @doc List all security schemes
-spec list_security_schemes() -> #{binary() => #a2a_security_scheme{}}.
list_security_schemes() ->
    gen_server:call(?MODULE, list_security_schemes).

%% @doc Sign the agent card using JWS
%% JWK is a JSON Web Key map with at least:
%%   - kty: Key type (e.g., "RSA", "EC", "oct")
%%   - Algorithm-specific parameters
-spec sign_agent_card(#a2a_agent_card{}, map()) ->
    {ok, #a2a_agent_card{}} | {error, term()}.
sign_agent_card(#a2a_agent_card{} = Card, JWK) when is_map(JWK) ->
    try
        %% Encode the card to JSON
        CardMap = erlmcp_a2a_protocol:encode_agent_card(Card),
        CardJson = erlmcp_json:encode(CardMap),

        %% Create protected header
        Algorithm = maps:get(<<"alg">>, JWK, <<"RS256">>),
        ProtectedHeader = #{
            <<"alg">> => Algorithm,
            <<"typ">> => <<"JWT">>
        },
        ProtectedJson = erlmcp_json:encode(ProtectedHeader),
        ProtectedB64 = base64url_encode(ProtectedJson),

        %% Create payload
        PayloadB64 = base64url_encode(CardJson),

        %% Create signing input
        SigningInput = <<ProtectedB64/binary, ".", PayloadB64/binary>>,

        %% Sign
        Signature = compute_jws_signature(SigningInput, JWK, Algorithm),
        SignatureB64 = base64url_encode(Signature),

        %% Create signature record
        Sig = #a2a_agent_card_signature{
            protected = ProtectedB64,
            signature = SignatureB64,
            header = undefined
        },

        %% Add signature to card
        ExistingSigs = case Card#a2a_agent_card.signatures of
            undefined -> [];
            Sigs -> Sigs
        end,
        SignedCard = Card#a2a_agent_card{signatures = ExistingSigs ++ [Sig]},
        {ok, SignedCard}
    catch
        _:Reason -> {error, {signing_failed, Reason}}
    end.

%% @doc Verify an agent card signature
-spec verify_agent_card_signature(#a2a_agent_card{}, map()) ->
    {ok, valid} | {error, term()}.
verify_agent_card_signature(#a2a_agent_card{signatures = undefined}, _JWK) ->
    {error, no_signatures};
verify_agent_card_signature(#a2a_agent_card{signatures = []}, _JWK) ->
    {error, no_signatures};
verify_agent_card_signature(#a2a_agent_card{signatures = Sigs} = Card, JWK) ->
    %% Try to verify any signature
    %% Remove signatures before encoding for verification
    CardWithoutSigs = Card#a2a_agent_card{signatures = undefined},
    CardMap = erlmcp_a2a_protocol:encode_agent_card(CardWithoutSigs),
    CardJson = erlmcp_json:encode(CardMap),
    PayloadB64 = base64url_encode(CardJson),

    VerifyOne = fun(#a2a_agent_card_signature{protected = Protected, signature = Sig}) ->
        try
            %% Decode protected header
            ProtectedJson = base64url_decode(Protected),
            #{<<"alg">> := Algorithm} = erlmcp_json:decode(ProtectedJson),

            %% Reconstruct signing input
            SigningInput = <<Protected/binary, ".", PayloadB64/binary>>,

            %% Decode signature
            SignatureBytes = base64url_decode(Sig),

            %% Verify
            verify_jws_signature(SigningInput, SignatureBytes, JWK, Algorithm)
        catch
            _:_ -> false
        end
    end,

    case lists:any(VerifyOne, Sigs) of
        true -> {ok, valid};
        false -> {error, invalid_signature}
    end.

%% @doc Set the provider information
-spec set_provider(#a2a_agent_provider{} | map()) -> ok.
set_provider(#a2a_agent_provider{} = Provider) ->
    gen_server:call(?MODULE, {set_provider, Provider});
set_provider(ProviderMap) when is_map(ProviderMap) ->
    Provider = parse_provider(ProviderMap),
    set_provider(Provider).

%% @doc Get the provider information
-spec get_provider() -> #a2a_agent_provider{} | undefined.
get_provider() ->
    gen_server:call(?MODULE, get_provider).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

%% @private
init(Options) ->
    %% Initialize with defaults or from options
    InitialCaps = #a2a_agent_capabilities{
        streaming = maps:get(streaming, Options, false),
        push_notifications = maps:get(push_notifications, Options, false),
        extensions = undefined,
        extended_agent_card = maps:get(extended_agent_card, Options, false)
    },

    State = #state{
        agent_card = undefined,
        extended_agent_card = undefined,
        skills = #{},
        skill_handlers = #{},
        interfaces = [],
        security_schemes = #{},
        provider = undefined,
        capabilities = InitialCaps,
        signatures = []
    },

    %% If initial card options provided, create it
    FinalState = case maps:get(agent_card, Options, undefined) of
        undefined -> State;
        CardOpts when is_map(CardOpts) ->
            case create_agent_card(CardOpts) of
                {ok, Card} -> State#state{agent_card = Card};
                {error, _} -> State
            end
    end,

    {ok, FinalState}.

%% @private
handle_call(get_agent_card, _From, #state{agent_card = undefined} = State) ->
    {reply, {error, not_configured}, State};
handle_call(get_agent_card, _From, #state{agent_card = Card} = State) ->
    %% Rebuild card with current state
    UpdatedCard = rebuild_agent_card(Card, State),
    {reply, {ok, UpdatedCard}, State};

handle_call(get_extended_agent_card, _From,
            #state{extended_agent_card = undefined, agent_card = undefined} = State) ->
    {reply, {error, not_configured}, State};
handle_call(get_extended_agent_card, _From,
            #state{extended_agent_card = undefined, agent_card = Card} = State) ->
    %% Fall back to regular card if no extended card
    UpdatedCard = rebuild_agent_card(Card, State),
    {reply, {ok, UpdatedCard}, State};
handle_call(get_extended_agent_card, _From,
            #state{extended_agent_card = Card} = State) ->
    UpdatedCard = rebuild_agent_card(Card, State),
    {reply, {ok, UpdatedCard}, State};

handle_call({set_agent_card, Card}, _From, State) ->
    %% Extract skills and interfaces from card
    NewSkills = lists:foldl(
        fun(S, Acc) -> Acc#{S#a2a_agent_skill.id => S} end,
        State#state.skills,
        Card#a2a_agent_card.skills
    ),
    NewState = State#state{
        agent_card = Card,
        skills = NewSkills,
        interfaces = Card#a2a_agent_card.supported_interfaces,
        security_schemes = case Card#a2a_agent_card.security_schemes of
            undefined -> State#state.security_schemes;
            Schemes -> Schemes
        end,
        provider = Card#a2a_agent_card.provider,
        capabilities = Card#a2a_agent_card.capabilities
    },
    {reply, ok, NewState};

handle_call({set_extended_agent_card, Card}, _From, State) ->
    {reply, ok, State#state{extended_agent_card = Card}};

handle_call({register_skill, Skill, Handler}, _From, State) ->
    SkillId = Skill#a2a_agent_skill.id,
    NewSkills = maps:put(SkillId, Skill, State#state.skills),
    NewHandlers = maps:put(SkillId, Handler, State#state.skill_handlers),
    {reply, ok, State#state{skills = NewSkills, skill_handlers = NewHandlers}};

handle_call({unregister_skill, SkillId}, _From, State) ->
    case maps:is_key(SkillId, State#state.skills) of
        true ->
            NewSkills = maps:remove(SkillId, State#state.skills),
            NewHandlers = maps:remove(SkillId, State#state.skill_handlers),
            {reply, ok, State#state{skills = NewSkills, skill_handlers = NewHandlers}};
        false ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_skill, SkillId}, _From, State) ->
    case maps:find(SkillId, State#state.skills) of
        {ok, Skill} -> {reply, {ok, Skill}, State};
        error -> {reply, {error, not_found}, State}
    end;

handle_call(list_skills, _From, State) ->
    Skills = maps:values(State#state.skills),
    {reply, Skills, State};

handle_call({update_capabilities, Caps}, _From, State) ->
    {reply, ok, State#state{capabilities = Caps}};

handle_call(get_capabilities, _From, State) ->
    {reply, State#state.capabilities, State};

handle_call({register_interface, Interface}, _From, State) ->
    %% Remove any existing interface with same URL
    Filtered = [I || I <- State#state.interfaces,
                     I#a2a_agent_interface.url =/= Interface#a2a_agent_interface.url],
    NewInterfaces = Filtered ++ [Interface],
    {reply, ok, State#state{interfaces = NewInterfaces}};

handle_call({unregister_interface, Url}, _From, State) ->
    case lists:any(fun(I) -> I#a2a_agent_interface.url =:= Url end,
                   State#state.interfaces) of
        true ->
            Filtered = [I || I <- State#state.interfaces,
                             I#a2a_agent_interface.url =/= Url],
            {reply, ok, State#state{interfaces = Filtered}};
        false ->
            {reply, {error, not_found}, State}
    end;

handle_call(list_interfaces, _From, State) ->
    {reply, State#state.interfaces, State};

handle_call({add_security_scheme, Name, Scheme}, _From, State) ->
    NewSchemes = maps:put(Name, Scheme, State#state.security_schemes),
    {reply, ok, State#state{security_schemes = NewSchemes}};

handle_call({remove_security_scheme, Name}, _From, State) ->
    case maps:is_key(Name, State#state.security_schemes) of
        true ->
            NewSchemes = maps:remove(Name, State#state.security_schemes),
            {reply, ok, State#state{security_schemes = NewSchemes}};
        false ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_security_scheme, Name}, _From, State) ->
    case maps:find(Name, State#state.security_schemes) of
        {ok, Scheme} -> {reply, {ok, Scheme}, State};
        error -> {reply, {error, not_found}, State}
    end;

handle_call(list_security_schemes, _From, State) ->
    {reply, State#state.security_schemes, State};

handle_call({set_provider, Provider}, _From, State) ->
    {reply, ok, State#state{provider = Provider}};

handle_call(get_provider, _From, State) ->
    {reply, State#state.provider, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Parse an interface map to record
parse_interface(Map) when is_map(Map) ->
    #a2a_agent_interface{
        url = maps:get(url, Map, maps:get(<<"url">>, Map)),
        protocol_binding = maps:get(protocol_binding, Map,
                                    maps:get(<<"protocolBinding">>, Map,
                                             ?A2A_PROTOCOL_BINDING_JSONRPC)),
        tenant = maps:get(tenant, Map, maps:get(<<"tenant">>, Map, undefined)),
        protocol_version = maps:get(protocol_version, Map,
                                    maps:get(<<"protocolVersion">>, Map,
                                             ?A2A_PROTOCOL_VERSION))
    }.

%% @private Parse a provider map to record
parse_provider(Map) when is_map(Map) ->
    #a2a_agent_provider{
        url = maps:get(url, Map, maps:get(<<"url">>, Map)),
        organization = maps:get(organization, Map,
                                maps:get(<<"organization">>, Map))
    }.

%% @private Parse capabilities map to record
parse_capabilities(Map) when is_map(Map) ->
    #a2a_agent_capabilities{
        streaming = maps:get(streaming, Map,
                             maps:get(<<"streaming">>, Map, undefined)),
        push_notifications = maps:get(push_notifications, Map,
                                      maps:get(<<"pushNotifications">>, Map, undefined)),
        extensions = parse_extensions(
            maps:get(extensions, Map,
                     maps:get(<<"extensions">>, Map, undefined))),
        extended_agent_card = maps:get(extended_agent_card, Map,
                                       maps:get(<<"extendedAgentCard">>, Map, undefined))
    }.

%% @private Parse extensions
parse_extensions(undefined) -> undefined;
parse_extensions([]) -> undefined;
parse_extensions(Extensions) when is_list(Extensions) ->
    [parse_extension(E) || E <- Extensions].

%% @private Parse a single extension
parse_extension(Map) when is_map(Map) ->
    #a2a_agent_extension{
        uri = maps:get(uri, Map, maps:get(<<"uri">>, Map)),
        description = maps:get(description, Map,
                               maps:get(<<"description">>, Map, undefined)),
        required = maps:get(required, Map,
                            maps:get(<<"required">>, Map, false)),
        params = maps:get(params, Map,
                          maps:get(<<"params">>, Map, undefined))
    }.

%% @private Parse a skill map to record
parse_skill(Map) when is_map(Map) ->
    #a2a_agent_skill{
        id = maps:get(id, Map, maps:get(<<"id">>, Map)),
        name = maps:get(name, Map, maps:get(<<"name">>, Map)),
        description = maps:get(description, Map,
                               maps:get(<<"description">>, Map)),
        tags = maps:get(tags, Map, maps:get(<<"tags">>, Map, [])),
        examples = maps:get(examples, Map,
                            maps:get(<<"examples">>, Map, undefined)),
        input_modes = maps:get(input_modes, Map,
                               maps:get(<<"inputModes">>, Map, undefined)),
        output_modes = maps:get(output_modes, Map,
                                maps:get(<<"outputModes">>, Map, undefined)),
        security_requirements = maps:get(security_requirements, Map,
                                         maps:get(<<"securityRequirements">>, Map, undefined))
    }.

%% @private Parse security schemes
parse_security_schemes(undefined) -> undefined;
parse_security_schemes(Schemes) when is_map(Schemes) ->
    maps:map(fun(_K, V) -> parse_security_scheme(V) end, Schemes).

%% @private Parse a single security scheme
parse_security_scheme(#{api_key := ApiKey}) ->
    #a2a_security_scheme{api_key = parse_api_key_scheme(ApiKey)};
parse_security_scheme(#{<<"apiKey">> := ApiKey}) ->
    #a2a_security_scheme{api_key = parse_api_key_scheme(ApiKey)};
parse_security_scheme(#{http_auth := HttpAuth}) ->
    #a2a_security_scheme{http_auth = parse_http_auth_scheme(HttpAuth)};
parse_security_scheme(#{<<"httpAuth">> := HttpAuth}) ->
    #a2a_security_scheme{http_auth = parse_http_auth_scheme(HttpAuth)};
parse_security_scheme(#{oauth2 := OAuth2}) ->
    #a2a_security_scheme{oauth2 = parse_oauth2_scheme(OAuth2)};
parse_security_scheme(#{<<"oauth2">> := OAuth2}) ->
    #a2a_security_scheme{oauth2 = parse_oauth2_scheme(OAuth2)};
parse_security_scheme(#{openid_connect := OpenId}) ->
    #a2a_security_scheme{openid_connect = parse_openid_scheme(OpenId)};
parse_security_scheme(#{<<"openIdConnect">> := OpenId}) ->
    #a2a_security_scheme{openid_connect = parse_openid_scheme(OpenId)};
parse_security_scheme(#{mtls := Mtls}) ->
    #a2a_security_scheme{mtls = parse_mtls_scheme(Mtls)};
parse_security_scheme(#{<<"mtls">> := Mtls}) ->
    #a2a_security_scheme{mtls = parse_mtls_scheme(Mtls)};
parse_security_scheme(_) ->
    #a2a_security_scheme{}.

%% @private Parse API key scheme
parse_api_key_scheme(Map) ->
    #a2a_api_key_security_scheme{
        description = maps:get(description, Map,
                               maps:get(<<"description">>, Map, undefined)),
        location = maps:get(location, Map, maps:get(<<"location">>, Map)),
        name = maps:get(name, Map, maps:get(<<"name">>, Map))
    }.

%% @private Parse HTTP auth scheme
parse_http_auth_scheme(Map) ->
    #a2a_http_auth_security_scheme{
        description = maps:get(description, Map,
                               maps:get(<<"description">>, Map, undefined)),
        scheme = maps:get(scheme, Map, maps:get(<<"scheme">>, Map)),
        bearer_format = maps:get(bearer_format, Map,
                                 maps:get(<<"bearerFormat">>, Map, undefined))
    }.

%% @private Parse OAuth2 scheme
parse_oauth2_scheme(Map) ->
    #a2a_oauth2_security_scheme{
        description = maps:get(description, Map,
                               maps:get(<<"description">>, Map, undefined)),
        flows = parse_oauth_flows(maps:get(flows, Map,
                                           maps:get(<<"flows">>, Map, #{}))),
        oauth2_metadata_url = maps:get(oauth2_metadata_url, Map,
                                       maps:get(<<"oauth2MetadataUrl">>, Map, undefined))
    }.

%% @private Parse OAuth flows
parse_oauth_flows(Map) ->
    case Map of
        #{authorization_code := AuthCode} ->
            #a2a_oauth_flows{authorization_code = parse_auth_code_flow(AuthCode)};
        #{<<"authorizationCode">> := AuthCode} ->
            #a2a_oauth_flows{authorization_code = parse_auth_code_flow(AuthCode)};
        #{client_credentials := ClientCreds} ->
            #a2a_oauth_flows{client_credentials = parse_client_creds_flow(ClientCreds)};
        #{<<"clientCredentials">> := ClientCreds} ->
            #a2a_oauth_flows{client_credentials = parse_client_creds_flow(ClientCreds)};
        #{device_code := DeviceCode} ->
            #a2a_oauth_flows{device_code = parse_device_code_flow(DeviceCode)};
        #{<<"deviceCode">> := DeviceCode} ->
            #a2a_oauth_flows{device_code = parse_device_code_flow(DeviceCode)};
        _ ->
            #a2a_oauth_flows{}
    end.

%% @private Parse authorization code flow
parse_auth_code_flow(Map) ->
    #a2a_authorization_code_oauth_flow{
        authorization_url = maps:get(authorization_url, Map,
                                     maps:get(<<"authorizationUrl">>, Map)),
        token_url = maps:get(token_url, Map, maps:get(<<"tokenUrl">>, Map)),
        refresh_url = maps:get(refresh_url, Map,
                               maps:get(<<"refreshUrl">>, Map, undefined)),
        scopes = maps:get(scopes, Map, maps:get(<<"scopes">>, Map, #{})),
        pkce_required = maps:get(pkce_required, Map,
                                 maps:get(<<"pkceRequired">>, Map, undefined))
    }.

%% @private Parse client credentials flow
parse_client_creds_flow(Map) ->
    #a2a_client_credentials_oauth_flow{
        token_url = maps:get(token_url, Map, maps:get(<<"tokenUrl">>, Map)),
        refresh_url = maps:get(refresh_url, Map,
                               maps:get(<<"refreshUrl">>, Map, undefined)),
        scopes = maps:get(scopes, Map, maps:get(<<"scopes">>, Map, #{}))
    }.

%% @private Parse device code flow
parse_device_code_flow(Map) ->
    #a2a_device_code_oauth_flow{
        device_authorization_url = maps:get(device_authorization_url, Map,
                                            maps:get(<<"deviceAuthorizationUrl">>, Map)),
        token_url = maps:get(token_url, Map, maps:get(<<"tokenUrl">>, Map)),
        refresh_url = maps:get(refresh_url, Map,
                               maps:get(<<"refreshUrl">>, Map, undefined)),
        scopes = maps:get(scopes, Map, maps:get(<<"scopes">>, Map, #{}))
    }.

%% @private Parse OpenID Connect scheme
parse_openid_scheme(Map) ->
    #a2a_openid_connect_security_scheme{
        description = maps:get(description, Map,
                               maps:get(<<"description">>, Map, undefined)),
        open_id_connect_url = maps:get(open_id_connect_url, Map,
                                       maps:get(<<"openIdConnectUrl">>, Map))
    }.

%% @private Parse mTLS scheme
parse_mtls_scheme(Map) ->
    #a2a_mtls_security_scheme{
        description = maps:get(description, Map,
                               maps:get(<<"description">>, Map, undefined))
    }.

%% @private Validate a single interface
validate_interface(#a2a_agent_interface{url = undefined}) ->
    {error, missing_url};
validate_interface(#a2a_agent_interface{url = Url}) when not is_binary(Url) ->
    {error, {invalid_url, Url}};
validate_interface(#a2a_agent_interface{protocol_binding = undefined}) ->
    {error, missing_protocol_binding};
validate_interface(#a2a_agent_interface{protocol_version = undefined}) ->
    {error, missing_protocol_version};
validate_interface(#a2a_agent_interface{}) ->
    ok;
validate_interface(_) ->
    {error, invalid_interface}.

%% @private Validate a list of interfaces
validate_interfaces([]) -> ok;
validate_interfaces([H|T]) ->
    case validate_interface(H) of
        ok -> validate_interfaces(T);
        {error, _} = Err -> Err
    end.

%% @private Validate a single skill
validate_skill(#a2a_agent_skill{id = undefined}) ->
    {error, missing_skill_id};
validate_skill(#a2a_agent_skill{id = Id}) when not is_binary(Id) ->
    {error, {invalid_skill_id, Id}};
validate_skill(#a2a_agent_skill{name = undefined}) ->
    {error, missing_skill_name};
validate_skill(#a2a_agent_skill{name = Name}) when not is_binary(Name) ->
    {error, {invalid_skill_name, Name}};
validate_skill(#a2a_agent_skill{description = undefined}) ->
    {error, missing_skill_description};
validate_skill(#a2a_agent_skill{tags = undefined}) ->
    {error, missing_skill_tags};
validate_skill(#a2a_agent_skill{tags = Tags}) when not is_list(Tags) ->
    {error, {invalid_skill_tags, Tags}};
validate_skill(#a2a_agent_skill{}) ->
    ok;
validate_skill(_) ->
    {error, invalid_skill}.

%% @private Validate a list of skills
validate_skills([]) -> ok;
validate_skills([H|T]) ->
    case validate_skill(H) of
        ok -> validate_skills(T);
        {error, _} = Err -> Err
    end.

%% @private Rebuild agent card with current state
rebuild_agent_card(Card, State) ->
    Card#a2a_agent_card{
        skills = maps:values(State#state.skills),
        supported_interfaces = case State#state.interfaces of
            [] -> Card#a2a_agent_card.supported_interfaces;
            Interfaces -> Interfaces
        end,
        security_schemes = case maps:size(State#state.security_schemes) of
            0 -> Card#a2a_agent_card.security_schemes;
            _ -> State#state.security_schemes
        end,
        provider = case State#state.provider of
            undefined -> Card#a2a_agent_card.provider;
            Provider -> Provider
        end,
        capabilities = State#state.capabilities,
        signatures = case State#state.signatures of
            [] -> Card#a2a_agent_card.signatures;
            Sigs -> Sigs
        end
    }.

%% @private Base64 URL encoding (RFC 4648)
base64url_encode(Data) when is_binary(Data) ->
    B64 = base64:encode(Data),
    << <<(base64url_char(C))/integer>> || <<C>> <= B64, C =/= $= >>.

base64url_char($+) -> $-;
base64url_char($/) -> $_;
base64url_char(C) -> C.

%% @private Base64 URL decoding (RFC 4648)
base64url_decode(Data) when is_binary(Data) ->
    %% Add padding if needed
    Padding = case byte_size(Data) rem 4 of
        0 -> <<>>;
        2 -> <<"==">>;
        3 -> <<"=">>
    end,
    B64 = << <<(base64url_char_reverse(C))/integer>> || <<C>> <= Data >>,
    base64:decode(<<B64/binary, Padding/binary>>).

base64url_char_reverse($-) -> $+;
base64url_char_reverse($_) -> $/;
base64url_char_reverse(C) -> C.

%% @private Compute JWS signature
%% Note: This is a simplified implementation. In production, use a proper
%% crypto library like jose or erlang-jose
compute_jws_signature(SigningInput, JWK, Algorithm) ->
    case Algorithm of
        <<"HS256">> ->
            Key = maps:get(<<"k">>, JWK),
            KeyBytes = base64url_decode(Key),
            crypto:mac(hmac, sha256, KeyBytes, SigningInput);
        <<"HS384">> ->
            Key = maps:get(<<"k">>, JWK),
            KeyBytes = base64url_decode(Key),
            crypto:mac(hmac, sha384, KeyBytes, SigningInput);
        <<"HS512">> ->
            Key = maps:get(<<"k">>, JWK),
            KeyBytes = base64url_decode(Key),
            crypto:mac(hmac, sha512, KeyBytes, SigningInput);
        <<"RS256">> ->
            %% RSA PKCS#1 v1.5 signature with SHA-256
            PrivKey = jwk_to_rsa_private_key(JWK),
            crypto:sign(rsa, sha256, SigningInput, PrivKey);
        <<"RS384">> ->
            PrivKey = jwk_to_rsa_private_key(JWK),
            crypto:sign(rsa, sha384, SigningInput, PrivKey);
        <<"RS512">> ->
            PrivKey = jwk_to_rsa_private_key(JWK),
            crypto:sign(rsa, sha512, SigningInput, PrivKey);
        <<"ES256">> ->
            %% ECDSA with P-256 and SHA-256
            PrivKey = jwk_to_ec_private_key(JWK),
            crypto:sign(ecdsa, sha256, SigningInput, PrivKey);
        <<"ES384">> ->
            PrivKey = jwk_to_ec_private_key(JWK),
            crypto:sign(ecdsa, sha384, SigningInput, PrivKey);
        <<"ES512">> ->
            PrivKey = jwk_to_ec_private_key(JWK),
            crypto:sign(ecdsa, sha512, SigningInput, PrivKey);
        _ ->
            throw({unsupported_algorithm, Algorithm})
    end.

%% @private Verify JWS signature
verify_jws_signature(SigningInput, Signature, JWK, Algorithm) ->
    try
        case Algorithm of
            <<"HS", _/binary>> ->
                ExpectedSig = compute_jws_signature(SigningInput, JWK, Algorithm),
                crypto:hash_equals(ExpectedSig, Signature);
            <<"RS256">> ->
                PubKey = jwk_to_rsa_public_key(JWK),
                crypto:verify(rsa, sha256, SigningInput, Signature, PubKey);
            <<"RS384">> ->
                PubKey = jwk_to_rsa_public_key(JWK),
                crypto:verify(rsa, sha384, SigningInput, Signature, PubKey);
            <<"RS512">> ->
                PubKey = jwk_to_rsa_public_key(JWK),
                crypto:verify(rsa, sha512, SigningInput, Signature, PubKey);
            <<"ES256">> ->
                PubKey = jwk_to_ec_public_key(JWK),
                crypto:verify(ecdsa, sha256, SigningInput, Signature, PubKey);
            <<"ES384">> ->
                PubKey = jwk_to_ec_public_key(JWK),
                crypto:verify(ecdsa, sha384, SigningInput, Signature, PubKey);
            <<"ES512">> ->
                PubKey = jwk_to_ec_public_key(JWK),
                crypto:verify(ecdsa, sha512, SigningInput, Signature, PubKey);
            _ ->
                false
        end
    catch
        _:_ -> false
    end.

%% @private Convert JWK to RSA private key
jwk_to_rsa_private_key(JWK) ->
    N = base64url_decode(maps:get(<<"n">>, JWK)),
    E = base64url_decode(maps:get(<<"e">>, JWK)),
    D = base64url_decode(maps:get(<<"d">>, JWK)),
    [E, N, D].

%% @private Convert JWK to RSA public key
jwk_to_rsa_public_key(JWK) ->
    N = base64url_decode(maps:get(<<"n">>, JWK)),
    E = base64url_decode(maps:get(<<"e">>, JWK)),
    [E, N].

%% @private Convert JWK to EC private key
jwk_to_ec_private_key(JWK) ->
    Curve = jwk_curve_to_crypto(maps:get(<<"crv">>, JWK)),
    D = base64url_decode(maps:get(<<"d">>, JWK)),
    [D, Curve].

%% @private Convert JWK to EC public key
jwk_to_ec_public_key(JWK) ->
    Curve = jwk_curve_to_crypto(maps:get(<<"crv">>, JWK)),
    X = base64url_decode(maps:get(<<"x">>, JWK)),
    Y = base64url_decode(maps:get(<<"y">>, JWK)),
    [{X, Y}, Curve].

%% @private Convert JWK curve name to crypto curve
jwk_curve_to_crypto(<<"P-256">>) -> secp256r1;
jwk_curve_to_crypto(<<"P-384">>) -> secp384r1;
jwk_curve_to_crypto(<<"P-521">>) -> secp521r1;
jwk_curve_to_crypto(Curve) -> throw({unsupported_curve, Curve}).
