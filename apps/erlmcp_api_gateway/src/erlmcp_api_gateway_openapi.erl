-module(erlmcp_api_gateway_openapi).
-export([generate_api_spec/1, generate_consumer_portal_spec/1, generate_app_spec/1]).

generate_api_spec(ApiId) ->
    case erlmcp_api_gateway_registry:get_api(ApiId) of
        {ok, Api} ->
            Spec = #{
                <<"openapi">> => <<"3.0.0">>,
                <<"info">> => #{
                    <<"title">> => maps:get(name, Api),
                    <<"description">> => maps:get(description, Api),
                    <<"version">> => <<"1.0.0">>,
                    <<"contact">> => #{
                        <<"name">> => "erlmcp API Support",
                        <<"email">> => "support@erlmcp.com"
                    },
                    <<"license">> => #{
                        <<"name">> => "Apache 2.0",
                        <<"url">> => "https://www.apache.org/licenses/LICENSE-2.0.html"
                    }
                },
                <<"servers">> => [
                    #{
                        <<"url">> => <<"https://api.erlmcp.com">>,
                        <<"description">> => <<"Production Server">>
                    }
                ],
                <<"paths">> => generate_paths(ApiId),
                <<"components">> => #{
                    <<"schemas">> => generate_schemas(),
                    <<"securitySchemes">> => generate_security_schemes()
                }
            },
            {ok, jsx:encode(Spec)};
        {error, not_found} ->
            {error, api_not_found}
    end.

generate_consumer_portal_spec(ConsumerId) ->
    {ok, Consumer} = erlmcp_api_gateway_registry:get_consumer(ConsumerId),
    Apis = erlmcp_api_gateway_registry:list_apis(),

    Spec = #{
        <<"consumer">> => Consumer,
        <<"apis">> => lists:map(fun(Api) ->
            Api#{<<"spec">> => generate_api_spec_simple(Api)}
        end, Apis),
        <<"quotas">> => generate_quotas(Consumer),
        <<"support">> => #{
            <<"level">> => determine_support_level(Consumer),
            <<"contacts">> => generate_contacts(Consumer)
        }
    },
    {ok, jsx:encode(Spec)}.

generate_app_spec(AppSpec) ->
    #{<<"name">> := AppName, <<"description">> := Description} = AppSpec,
    BaseSpec = #{
        <<"openapi">> => <<"3.0.0">>,
        <<"info">> => #{
            <<"title">> => AppName,
            <<"description">> => Description,
            <<"version">> => <<"1.0.0">>
        },
        <<"paths">> => generate_app_paths(),
        <<"components">> => #{
            <<"schemas">> => generate_app_schemas(),
            <<"securitySchemes">> => generate_app_security_schemes()
        }
    },
    {ok, jsx:encode(BaseSpec)}.

generate_paths(ApiId) ->
    Routes = erlmcp_api_gateway_registry:list_routes(ApiId),
    lists:foldl(fun(Route, Paths) ->
        Path = maps:get(path, Route),
        Method = string:lowercase(atom_to_binary(maps:get(method, Route))),
        Paths#{Path => generate_path_spec(Method, Route)}
    end, #{}, Routes).

generate_path_spec(Method, Route) ->
    Spec = #{
        <<"summary">> => maps:get(summary, Route),
        <<"description">> => maps:get(description, Route, <<>>),
        <<"operationId">> => generate_operation_id(Method, Route),
        <<"tags">> => maps:get(tags, Route, [<<"API">>]),
        <<"parameters">> => generate_parameters(maps:get(parameters, Route, [])),
        <<"requestBody">> => generate_request_body(maps:get(requestBody, Route, undefined)),
        <<"responses">> => generate_responses(maps:get(responses, Route, #{})),
        <<"security">> => generate_security(maps:get(security, Route, []))
    },
    Spec.

generate_parameters([]) -> [];
generate_parameters(Params) ->
    lists:map(fun(Param) ->
        #{<<"name">> => maps:get(name, Param),
          <<"in">> => maps:get(in, Param),
          <<"required">> => maps:get(required, Param, false),
          <<"schema">> => maps:get(schema, Param, #{
              <<"type">> => <<"string">>
          })}
    end, Params).

generate_request_body(undefined) -> undefined;
generate_request_body(RequestBody) ->
    #{<<"content">> => #{<<"application/json">> => #{
        <<"schema">> => maps:get(schema, RequestBody)
    }}}.

generate_responses([]) -> #{};
generate_responses(Responses) ->
    maps:map(fun(Code, Response) ->
        #{<<"description">> => maps:get(description, Response),
          <<"content">> => #{<<"application/json">> => #{
              <<"schema">> => maps:get(schema, Response, #{})
          }}}
    end, Responses).

generate_security([]) -> [];
generate_security(Security) ->
    lists:map(fun(Scheme) ->
        #{<<"oauth2">> => Scheme}
    end, Security).

generate_schemas() ->
    #{
        <<"Error">> => #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"code">> => #{
                    <<"type">> => <<"integer">>,
                    <<"description">> => <<"Error code">>
                },
                <<"message">> => #{
                    <<"type">> => <<"string">>,
                    <<"description">> => <<"Error message">>
                }
            }
        },
        <<"ApiResponse">> => #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"success">> => #{
                    <<"type">> => <<"boolean">>
                },
                <<"data">> => #{
                    <<"type">> => <<"object">>
                },
                <<"error">> => #{
                    <<"$ref">> => <<"#/components/schemas/Error">>
                }
            }
        }
    }.

generate_security_schemes() ->
    #{
        <<"oauth2">> => #{
            <<"type">> => <<"oauth2">>,
            <<"flows">> => #{
                <<"authorizationCode">> => #{
                    <<"authorizationUrl">> => <<"https://api.erlmcp.com/oauth/authorize">>,
                    <<"tokenUrl">> => <<"https://api.erlmcp.com/oauth/token">>,
                    <<"scopes">> => #{
                        <<"read:api">> => <<"Read API information">>,
                        <<"write:api">> => <<"Modify API configurations">>,
                        <<"admin:api">> => <<"Full API administration">>
                    }
                }
            }
        },
        <<"apiKey">> => #{
            <<"type">> => <<"apiKey">>,
            <<"in">> => <<"header">>,
            <<"name">> => <<"X-API-Key">>
        }
    }.

generate_api_spec_simple(Api) ->
    #{
        <<"id">> => maps:get(id, Api),
        <<"name">> => maps:get(name, Api),
        <<"description">> => maps:get(description, Api),
        <<"rate_limit">> => maps:get(rate_limit, Api),
        <<"endpoints">> => length(erlmcp_api_gateway_registry:list_routes(maps:get(id, Api)))
    }.

generate_quotas(Consumer) ->
    case lists:member(<<"enterprise">>, maps:get(tags, Consumer, [])) of
        true -> #{
            <<"monthly_requests">> => 1000000,
            <<"concurrent_requests">> => 1000,
            <<"rate_limit_per_second">> => 1000
        };
        false -> #{
            <<"monthly_requests">> => 100000,
            <<"concurrent_requests">> => 100,
            <<"rate_limit_per_second">> => 100
        }
    end.

determine_support_level(Consumer) ->
    case lists:member(<<"enterprise">>, maps:get(tags, Consumer, [])) of
        true -> <<"enterprise">>;
        true when lists:member(<<"pro">>, maps:get(tags, Consumer, [])) -> <<"pro">>;
        _ -> <<"basic">>
    end.

generate_contacts(Consumer) ->
    case determine_support_level(Consumer) of
        <<"enterprise">> -> #{
            <<"primary">> => <<"enterprise@erlmcp.com">>,
            <<"premium">> => <<"premium@erlmcp.com">>,
            <<"backup">> => <<"support@erlmcp.com">>
        };
        _ -> #{
            <<"primary">> => <<"support@erlmcp.com">>
        }
    end.

generate_operation_id(Method, Route) ->
    Path = maps:get(path, Route),
    lists:flatten([Method, "_", lists:map(fun(C) ->
        case C of
            $/ -> "_";
            $_ -> "_";
            $- -> "_";
            C when C >= $a, C =< $z -> C;
            C when C >= $A, C =< $Z -> C;
            C when C >= $0, C =< $9 -> C;
            _ -> "_"
        end
    end, Path)]).

generate_app_paths() ->
    #{
        <<"/app">> => #{
            <<"get">> => #{
                <<"summary">> => <<"Get Application Details">>,
                <<"responses">> => #{
                    200 => #{
                        <<"description">> => <<"Application details">>,
                        <<"content">> => #{
                            <<"application/json">> => #{
                                <<"schema">> => #{
                                    <<"$ref">> => <<"#/components/schemas/App">>
                                }
                            }
                        }
                    }
                }
            }
        }
    }.

generate_app_schemas() ->
    #{
        <<"App">> => #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"id">> => #{
                    <<"type">> => <<"string">>
                },
                <<"name">> => #{
                    <<"type">> => <<"string">>
                },
                <<"status">> => #{
                    <<"type">> => <<"string">>,
                    <<"enum">> => [<<"active">>, <<"inactive">>, <<"suspended">>]
                }
            }
        }
    }.

generate_app_security_schemes() ->
    #{
        <<"oauth2">> => #{
            <<"type">> => <<"oauth2">>,
            <<"flows">> => #{
                <<"clientCredentials">> => #{
                    <<"tokenUrl">> => <<"https://api.erlmcp.com/oauth/token">>,
                    <<"scopes">> => #{
                        <<"app:read">> => <<"Read application data">>
                    }
                }
            }
        }
    }.