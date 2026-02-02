-module(erlmcp_tool_fetch).

%% Fetch tool for erlmcp_tool_router
%% Fetches content from URLs

-export([handle/2]).

%% @doc Handle fetch tool invocation
-spec handle(binary(), map()) -> {ok, map()} | {error, term()}.
handle(Url, _Captures) ->
    try
        %% Validate URL
        case validate_url(Url) of
            ok ->
                %% In production, this would use gun HTTP client
                Content = fetch_content(Url),
                {ok, #{<<"url">> => Url,
                        <<"content">> => Content,
                        <<"status">> => 200}};
            {error, Reason} ->
                {error, {invalid_url, Reason}}
        end
    catch
        _:_:Stacktrace ->
            {error, {fetch_failed, Stacktrace}}
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% Validate URL format
validate_url(Url) ->
    case re:run(Url, "^https?://[\\w\\.-]+(/[^\\s]*)?$", [unicode]) of
        {match, _} ->
            ok;
        nomatch ->
            {error, invalid_url_format}
    end.

%% Mock fetch implementation
fetch_content(_Url) ->
    %% In production, this would:
    %% 1. Use gun:async_request/4 for HTTP/HTTPS
    %% 2. Handle redirects, timeouts, errors
    %% 3. Support content-type handling
    %% 4. Return binary content
    <<"HTTP fetch not yet implemented">>.
