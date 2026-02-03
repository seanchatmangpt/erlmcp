-module(erlmcp_dev_portal_assets).

-behaviour(cowboy_handler).

%% API exports
-export([init/2, handle/2, terminate/3]).

%%====================================================================
%% API Functions
%%====================================================================

init(Req, _Opts) ->
    %% Initialize assets state
    State = #{},
    {ok, Req, State}.

handle(Req, State) ->
    %% Handle asset requests
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),

    case Method of
        <<"GET">> ->
            handle_get_asset(Path, Req, State);
        _ ->
            cowboy_req:reply(405, #{}, <<"Method not allowed">>, Req)
    end.

terminate(_Reason, _Req, _State) ->
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

handle_get_asset(<<"/assets/", Path/binary>>, Req, State) ->
    %% Handle asset request
    case serve_asset(Path, Req, State) of
        {ok, Body, ContentType} ->
            Headers = case ContentType of
                <<"text/css">> -> #{<<"content-type">> => ContentType};
                <<"application/javascript">> -> #{<<"content-type">> => ContentType};
                <<"image/">> -> #{<<"content-type">> => ContentType};
                _ -> #{}
            end,
            cowboy_req:reply(200, Headers, Body, Req);
        {error, not_found} ->
            cowboy_req:reply(404, #{}, <<"Asset not found">>, Req);
        {error, _} ->
            cowboy_req:reply(500, #{}, <<"Internal server error">>, Req)
    end;

handle_get_asset(Path, Req, State) ->
    cowboy_req:reply(404, #{}, <<"Not found">>, Req).

serve_asset(Path, Req, State) ->
    %% Serve static asset
    AssetPath = list_to_binary(["/Users/sac/erlmcp/apps/erlmcp_dev_portal/assets/", binary_to_list(Path)]),

    case file:read_file(AssetPath) of
        {ok, Binary} ->
            %% Determine content type
            ContentType = determine_content_type(Path),
            {ok, Binary, ContentType};
        {error, enoent} ->
            {error, not_found};
        {error, _} ->
            {error, internal_error}
    end.

determine_content_type(Path) ->
    %% Determine content type from file extension
    case filename:extension(Path) of
        <<".css">> -> <<"text/css">>;
        <<".js">> -> <<"application/javascript">>;
        <<".png">> -> <<"image/png">>;
        <<".jpg">> -> <<"image/jpeg">>;
        <<".jpeg">> -> <<"image/jpeg">>;
        <<".gif">> -> <<"image/gif">>;
        <<".svg">> -> <<"image/svg+xml">>;
        <<".ico">> -> <<"image/x-icon">>;
        _ -> <<"application/octet-stream">>
    end.