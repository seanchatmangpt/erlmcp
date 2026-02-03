-module(erlmcp_api_openapi).

-export([generate_openapi/1, generate_docs/1, validate_spec/1, optimize_spec/1]).

-include("erlmcp_api_gateway.hrl").

%%====================================================================
%% API Functions
%%====================================================================

generate_openapi(ApiId) ->
    %% Generate OpenAPI 3.0 specification for API
    case erlmcp_api_management:get_api(ApiId) of
        {ok, Api} ->
            OpenAPI = build_openapi_spec(Api),
            {ok, OpenAPI};
        {error, not_found} ->
            {error, api_not_found}
    end.

generate_docs(ApiId) ->
    %% Generate documentation for API
    case erlmcp_api_management:get_api(ApiId) of
        {ok, Api} ->
            Routes = erlmcp_api_management:list_routes(ApiId),
            Documentation = build_documentation(Api, Routes),
            {ok, Documentation};
        {error, not_found} ->
            {error, api_not_found}
    end.

validate_spec(Spec) ->
    %% Validate OpenAPI specification
    case jsx:is_json(Spec) of
        true ->
            Validation = validate_openapi_spec(Spec),
            case Validation of
                {valid, _} -> {ok, valid};
                {invalid, Errors} -> {error, Errors}
            end;
        false ->
            {error, invalid_json}
    end.

optimize_spec(Spec) ->
    %% Optimize OpenAPI specification
    case validate_spec(Spec) of
        {ok, valid} ->
            Optimized = optimize_openapi_spec(Spec),
            {ok, Optimized};
        {error, Errors} ->
            {error, Errors}
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

build_openapi_spec(Api) ->
    %% Build OpenAPI 3.0 specification
    ApiId = Api#api_definition.id,
    Version = Api#api_definition.version,
    Name = Api#api_definition.name,
    Description = Api#api_definition.description,

    %% Get routes
    Routes = erlmcp_api_management:list_routes(ApiId),

    %% Build components
    Components = build_components(),

    %% Build paths
    Paths = build_paths(Routes),

    %% Build info section
    Info = #{
        title => Name,
        description => Description,
        version => Version,
        contact => build_contact(),
        license => build_license()
    },

    %% Build servers
    Servers = build_servers(Api#api_definition.servers),

    %% Build security requirements
    Security = build_security(),

    %% Build full spec
    OpenAPI = #{
        openapi => <<"3.0.0">>,
        info => Info,
        servers => Servers,
        paths => Paths,
        components => Components,
        security => Security,
        tags => build_tags(Api),
        externalDocs => build_external_docs()
    },

    OpenAPI.

build_components() ->
    %% OpenAPI components section
    #{
        schemas => build_schemas(),
        responses => build_responses(),
        parameters => build_parameters(),
        examples => build_examples(),
        requestBodies => build_request_bodies(),
        headers => build_headers(),
        securitySchemes => build_security_schemes()
    }.

build_schemas() ->
    %% Build schema definitions
    #{
        error_response => #{
            type => <<"object">>,
            required => [<<"error">>, <<"message">>, <<"code">>],
            properties => #{
                error => #{
                    type => <<"string">>,
                    description => <<"Error type">>
                },
                message => #{
                    type => <<"string">>,
                    description => <<"Error message">>
                },
                code => #{
                    type => <<"integer">>,
                    description => <<"Error code">>
                },
                details => #{
                    type => <<"object">>,
                    description => <<"Additional error details">>
                }
            }
        },
        pagination => #{
            type => <<"object">>,
            required => [<<"total">>, <<"page">>, <<"per_page">>],
            properties => #{
                total => #{
                    type => <<"integer">>,
                    description => <<"Total number of items">>
                },
                page => #{
                    type => <<"integer">>,
                    description => <<"Current page number">>
                },
                per_page => #{
                    type => <<"integer">>,
                    description => <<"Items per page">>
                },
                next_page => #{
                    type => <<"string">>,
                    description => <<"URL for next page">>
                }
            }
        }
    }.

build_responses() ->
    %% Build response definitions
    #{
        ok => #{
            description => <<"Successful response">>,
            content => #{
                <<"application/json">> => #{
                    schema => #{
                        type => <<"object">>
                    }
                }
            }
        },
        error => #{
            description => <<"Error response">>,
            content => #{
                <<"application/json">> => #{
                    schema => #{
                        "$ref" => <<"#/components/schemas/error_response">>
                    }
                }
            }
        },
        unauthorized => #{
            description => <<"Unauthorized">>,
            content => #{
                <<"application/json">> => #{
                    schema => #{
                        "$ref" => <<"#/components/schemas/error_response">>
                    }
                }
            }
        },
        forbidden => #{
            description => <<"Forbidden">>,
            content => #{
                <<"application/json">> => #{
                    schema => #{
                        "$ref" => <<"#/components/schemas/error_response">>
                    }
                }
            }
        },
        not_found => #{
            description => <<"Resource not found">>,
            content => #{
                <<"application/json">> => #{
                    schema => #{
                        "$ref" => <<"#/components/schemas/error_response">>
                    }
                }
            }
        }
    }.

build_parameters() ->
    %% Build parameter definitions
    #{
        api_id => #{
            name => <<"api_id">>,
            in => <<"path">>,
            required => true,
            schema => #{
                type => <<"string">>
            },
            description => <<"API ID">>
        },
        version => #{
            name => <<"version">>,
            in => <<"path">>,
            required => true,
            schema => #{
                type => <<"string">>
            },
            description => <<"API version">>
        },
        page => #{
            name => <<"page">>,
            in => <<"query">>,
            required => false,
            schema => #{
                type => <<"integer">>,
                default => 1
            },
            description => <<"Page number">>
        },
        per_page => #{
            name => <<"per_page">>,
            in => <<"query">>,
            required => false,
            schema => #{
                type => <<"integer">>,
                default => 20,
                maximum => 100
            },
            description => <<"Items per page">>
        }
    }.

build_examples() ->
    %% Build example definitions
    #{
        api_list => #{
            summary => <<"List of APIs">>,
            value => #{
                apis => [
                    #{
                        id => <<"api1">>,
                        name => <<"User API">>,
                        version => <<"1.0.0">>,
                        description => <<"User management API">>
                    }
                ]
            }
        }
    }.

build_request_bodies() ->
    %% Build request body definitions
    #{
        create_api => #{
            description => <<"Create API request">>,
            content => #{
                <<"application/json">> => #{
                    schema => #{
                        type => <<"object">>,
                        required => [name, version, base_path],
                        properties => #{
                            name => #{
                                type => <<"string">>,
                                description => <<"API name">>
                            },
                            version => #{
                                type => <<"string">>,
                                description => <<"API version">>
                            },
                            base_path => #{
                                type => <<"string">>,
                                description => <<"API base path">>
                            },
                            description => #{
                                type => <<"string">>,
                                description => <<"API description">>
                            }
                        }
                    }
                }
            }
        }
    }.

build_headers() =>
    %% Build header definitions
    #{
        api_key => #{
            name => <<"X-API-Key">>,
            in => <<"header">>,
            required => true,
            schema => #{
                type => <<"string">>
            },
            description => <<"API key">>
        },
        authorization => #{
            name => <<"Authorization">>,
            in => <<"header">>,
            required => true,
            schema => #{
                type => <<"string">>
            },
            description => <<"Bearer token">>
        }
    }.

build_security_schemes() ->
    %% Build security scheme definitions
    #{
        api_key => #{
            type => <<"apiKey">>,
            in => <<"header">>,
            name => <<"X-API-Key">>,
            description => <<"API Key authentication">>
        },
        oauth2 => #{
            type => <<"oauth2">>,
            flows => #{
                implicit => #{
                    authorizationUrl => <<"https://api.example.com/oauth/authorize">>,
                    scopes => #{
                        "apis:read" => <<"Read APIs">>,
                        "apis:write" => <<"Write APIs">>,
                        "consumers:read" => <<"Read consumers">>,
                        "consumers:write" => <<"Write consumers">>
                    }
                }
            }
        }
    }.

build_paths(Routes) ->
    %% Build paths section
    lists:foldl(fun(Route, Acc) ->
        Path = Route#api_route.path,
        Method = Route#api_route.method,
        Operation = build_operation(Route),
        maps:put(Path, maps:put(binary_to_existing_atom(Method), Operation, maps:get(Path, Acc, #{})), Acc)
    end, #{}, Routes).

build_operation(Route) ->
    %% Build operation definition
    #{
        summary => Route#api_route.description,
        description => <<"API operation description">>,
        tags => [Route#api_route.api_id],
        operationId => generate_operation_id(Route),
        responses => build_operation_responses(Route),
        parameters => build_operation_parameters(Route),
        requestBody => build_operation_request_body(Route),
        security => build_operation_security(Route)
    }.

build_operation_responses(Route) ->
    %% Build operation responses
    #{
        200 => #{
            description => <<"Successful operation">>,
            content => #{
                <<"application/json">> => #{
                    schema => #{
                        type => <<"object">>
                    }
                }
            }
        },
        400 => #{
            "$ref" => <<"#/components/responses/error">>
        },
        401 => #{
            "$ref" => <<"#/components/responses/unauthorized">>
        },
        403 => #{
            "$ref" => <<"#/components/responses/forbidden">>
        },
        404 => #{
            "$ref" => <<"#/components/responses/not_found">>
        }
    }.

build_operation_parameters(Route) ->
    %% Build operation parameters
    [
        #{
            "$ref" => <<"#/components/parameters/api_id">>
        },
        #{
            "$ref" => <<"#/components/parameters/version">>
        }
    ].

build_operation_request_body(Route) ->
    %% Build operation request body
    case Route#api_route.method of
        <<"POST">> ->
            #{
                "$ref" => <<"#/components/requestBodies/create_api">>
            };
        <<"PUT">> ->
            #{
                "$ref" => <<"#/components/requestBodies/create_api">>
            };
        _ ->
            undefined
    end.

build_operation_security(Route) ->
    %% Build operation security
    case Route#api_route.auth_required of
        true ->
            [#{<<"oauth2">> => [<<"apis:read">>]}];
        false ->
            []
    end.

generate_operation_id(Route) ->
    %% Generate operation ID
    ApiId = Route#api_route.api_id,
    Method = Route#api_route.method,
    Path = Route#api_route.path,
    list_to_binary([Method, "_", ApiId, "_", re:replace(Path, "/", "_", [{return, list}])]).

build_contact() ->
    %% Build contact information
    #{
        name => <<"erlmcp API Team">>,
        email => <<"api@example.com">>,
        url => <<"https://erlmcp.dev">>
    }.

build_license() ->
    %% Build license information
    #{
        name => <<"MIT">>,
        url => <<"https://opensource.org/licenses/MIT">>
    }.

build_servers(Servers) ->
    %% Build servers array
    case Servers of
        undefined ->
            [#{url => <<"https://api.example.com">>, description => <<"Production server">>}];
        _ ->
            lists:map(fun(Server) ->
                #{
                    url => Server#{"url"},
                    description => maps:get("description", Server, <<"Server">>)
                }
            end, Servers)
    end.

build_security() ->
    %% Build security requirements
    [#{<<"oauth2">> => [<<"apis:read">>]}].

build_tags(Api) ->
    %% Build tags array
    [Api#api_definition.name].

build_external_docs() ->
    %% Build external docs
    #{
        description => <<"More documentation">>,
        url => <<"https://docs.erlmcp.dev">>
    }.

build_documentation(Api, Routes) ->
    %% Build comprehensive documentation
    #{
        api => Api,
        routes => Routes,
        examples => generate_examples(Routes),
        tutorials => generate_tutorials(Routes),
        faqs => generate_faqs(),
        support => generate_support_info()
    }.

generate_examples(Routes) ->
    %% Generate API examples
    lists:map(fun(Route) ->
        #{
            route => Route,
            examples => [
                #{
                    title => <<"Example request">>,
                    request => build_example_request(Route),
                    response => build_example_response(Route)
                }
            ]
        }
    end, Routes).

build_example_request(Route) ->
    %% Build example request
    #{
        method => Route#api_route.method,
        url => Route#api_route.path,
        headers => #{
            <<"Content-Type">> => <<"application/json">>,
            <<"X-API-Key">> => <<"your-api-key">>
        },
        body => build_example_body(Route)
    }.

build_example_response(Route) ->
    %% Build example response
    #{
        status => 200,
        headers => #{
            <<"Content-Type">> => <<"application/json">>
        },
        body => build_example_body(Route)
    }.

build_example_body(Route) ->
    %% Build example response body
    #{
        success => true,
        data => #{
            id => <<"example-id">>,
            name => <<"Example resource">>
        }
    }.

generate_tutorials(Routes) ->
    %% Generate tutorials
    [
        #{
            title => <<"Getting Started">>,
            content => <<"Tutorial content">>
        },
        #{
            title => <<"Advanced Usage">>,
            content => <<"Advanced tutorial content">>
        }
    ].

generate_faqs() ->
    %% Generate FAQs
    [
        #{
            question => <<"How do I get an API key?">>,
            answer => <<"You can generate an API key from the developer portal.">>
        },
        #{
            question => <<"What are the rate limits?">>,
            answer => <<"Rate limits depend on your subscription tier.">>
        }
    ].

generate_support_info() ->
    %% Generate support information
    #{
        email => <<"support@example.com">>,
        documentation => <<"https://docs.erlmcp.dev">>,
        community => <<"https://community.erlmcp.dev">>
    }.

validate_openapi_spec(Spec) ->
    %% Validate OpenAPI specification
    case maps:get(<<"openapi">>, Spec, undefined) of
        <<"3.0.0">> ->
            validate_info(Spec),
            validate_paths(Spec),
            {valid, Spec};
        _ ->
            {invalid, [<<"Invalid OpenAPI version">>]}
    end.

validate_info(Spec) ->
    %% Validate info section
    Info = maps:get(<<"info">>, Spec, #{}),
    Required = [<<"title">>, <<"version">>],

    case check_required_fields(Required, Info) of
        ok ->
            ok;
        {error, Field} ->
            throw({invalid, [<<"Missing required field: ", Field/binary>>]})
    end.

validate_paths(Spec) ->
    %% Validate paths section
    Paths = maps:get(<<"paths">>, Spec, #{}),

    maps:map(fun(Path, PathItem) ->
        maps:map(fun(Method, Operation) ->
            validate_operation(Operation)
        end, PathItem)
    end, Paths).

validate_operation(Operation) ->
    %% Validate operation
    case maps:get(<<"responses">>, Operation, #{}) of
        Responses when map_size(Responses) > 0 ->
            ok;
        _ ->
            throw({invalid, [<<"Operation must have responses">>]})
    end.

check_required_fields(Fields, Map) ->
    check_required_fields(Fields, Map, ok).

check_required_fields([], _Map, Result) ->
    Result;

check_required_fields([Field|Rest], Map, Result) ->
    case Result of
        ok ->
            case maps:is_key(Field, Map) of
                true ->
                    check_required_fields(Rest, Map, ok);
                false ->
                    check_required_fields(Rest, Map, {error, Field})
            end;
        _ ->
            check_required_fields(Rest, Map, Result)
    end.

optimize_openapi_spec(Spec) ->
    %% Optimize OpenAPI specification
    Optimized = Spec,

    %% Remove unnecessary fields
    Optimized = maps:remove(<<"externalDocs">>, Optimized),

    %% Compress schemas
    Optimized = compress_schemas(Optimized),

    %% Add cache headers
    Optimized = add_cache_headers(Optimized),

    Optimized.

compress_schemas(Spec) ->
    %% Compress schema definitions
    Schemas = maps:get(<<"components">>, Spec, #{}),
    CompressedSchemas = maps:map(fun(_Key, Schema) ->
        compress_schema(Schema)
    end, Schemas),

    maps:put(<<"components">>, CompressedSchemas, Spec).

compress_schema(Schema) ->
    %% Compress individual schema
    case Schema of
        #{<<"properties">> := Properties} ->
            CompressedProperties = maps:map(fun(_Key, Prop) ->
                compress_property(Prop)
            end, Properties),
            Schema#{<<"properties">> := CompressedProperties};
        _ ->
            Schema
    end.

compress_property(Prop) ->
    %% Compress property
    case Prop of
        #{<<"type">> := <<"array">>, <<"items">> := Items} ->
            Prop#{<<"items">> := compress_property(Items)};
        _ ->
            Prop
    end.

add_cache_headers(Spec) ->
    %% Add cache headers
    Spec#{<<"x-cache">> => <<"public, max-age=3600">>}.