-module(erlmcp_api_gateway_jwt).
-export([generate_token/2, verify_token/2, decode/1]).

generate_token(Payload, Secret) ->
    Headers = #{<<"typ">> => <<"JWT">>, <<"alg">> => <<"HS256">>},
    PayloadWithExp = maps:put(<<"exp">>, erlang:system_time(second) + 3600, Payload),
    EncodedHeader = base64url(jsx:encode(Headers)),
    EncodedPayload = base64url(jsx:encode(PayloadWithExp)),
    Signature = crypto:hmac(sha256, Secret, <<EncodedHeader/binary, ".", EncodedPayload/binary>>),
    EncodedSignature = base64url(Signature),
    <<EncodedHeader/binary, ".", EncodedPayload/binary, ".", EncodedSignature/binary>>.

verify_token(Token, Secret) ->
    case decode(Token) of
        {ok, Claims} ->
            case maps:get(<<"exp">>, Claims) > erlang:system_time(second) of
                true -> {ok, Claims};
                false -> {error, expired}
            end;
        {error, Reason} -> {error, Reason}
    end.

decode(Token) ->
    case binary:split(Token, <<".">>, [global]) of
        [Header, Payload, Signature] ->
            try
                DecodedHeader = jsx:decode(base64url_decode(Header), [{return_maps, true}]),
                DecodedPayload = jsx:decode(base64url_decode(Payload), [{return_maps, true}]),
                {ok, DecodedPayload}
            catch
                _ -> {error, invalid}
            end;
        _ -> {error, invalid}
    end.

base64url(Data) when is_binary(Data) ->
    Base64 = base64:encode(Data),
    Binary = binary:replace(Base64, <<"+">>, <<"-">>),
    binary:replace(Binary, <<"/">>, <<"_">>),
    binary:replace(Binary, <<"\n">>, <<>>);
base64url(Data) when is_list(Data) ->
    base64url(list_to_binary(Data)).

base64url_decode(Data) ->
    Padding = case length(Data) rem 4 of
        0 -> 0;
        2 -> 2;
        3 -> 1
    end,
    Binary = binary:replace(Data, <<"-">>, <<"+">>),
    Binary2 = binary:replace(Binary, <<"_">>, <<"/">>),
    case Padding of
        0 -> base64:decode(Binary2);
        _ -> base64:decode(<<Binary2/binary, (binary:copy(<<$=>>, Padding))/binary>>)
    end.