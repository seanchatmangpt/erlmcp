-module(erlmcp_stdio).
-export([
    start/0, start/1, stop/0,
    add_tool/3, add_tool/4,
    add_resource/3, add_resource/4,
    add_prompt/3, add_prompt/4,
    is_running/0
]).

%% Types
-type tool_handler() :: fun((map()) -> binary() | iolist()).
-type resource_handler() :: fun((binary()) -> binary() | iolist()).
-type prompt_handler() :: fun((map()) -> binary() | iolist() | [map()]).

%%====================================================================
%% API Functions
%%====================================================================

-spec start() -> ok | {error, term()}.
start() ->
    start(#{}).

-spec start(map()) -> ok | {error, term()}.
start(Options) ->
    % Ensure the main erlmcp application is started
    case application:ensure_all_started(erlmcp) of
        {ok, _} ->
            case erlmcp_sup:start_stdio_server(Options) of
                {ok, _Pid} -> ok;
                Error -> Error
            end;
        Error -> Error
    end.

-spec stop() -> ok.
stop() ->
    erlmcp_sup:stop_stdio_server().

-spec add_tool(binary(), binary(), tool_handler()) -> ok | {error, term()}.
add_tool(Name, Description, Handler) ->
    add_tool(Name, Description, Handler, undefined).

-spec add_tool(binary(), binary(), tool_handler(), map() | undefined) -> ok | {error, term()}.
add_tool(Name, Description, Handler, Schema) ->
    case is_running() of
        true ->
            erlmcp_stdio_server:add_tool(Name, Description, Handler, Schema);
        false ->
            {error, stdio_server_not_running}
    end.

-spec add_resource(binary(), binary(), resource_handler()) -> ok | {error, term()}.
add_resource(Uri, Description, Handler) ->
    add_resource(Uri, Description, Handler, <<"text/plain">>).

-spec add_resource(binary(), binary(), resource_handler(), binary()) -> ok | {error, term()}.
add_resource(Uri, Description, Handler, MimeType) ->
    case is_running() of
        true ->
            erlmcp_stdio_server:add_resource(Uri, Description, Handler, MimeType);
        false ->
            {error, stdio_server_not_running}
    end.

-spec add_prompt(binary(), binary(), prompt_handler()) -> ok | {error, term()}.
add_prompt(Name, Description, Handler) ->
    add_prompt(Name, Description, Handler, undefined).

-spec add_prompt(binary(), binary(), prompt_handler(), [map()] | undefined) -> ok | {error, term()}.
add_prompt(Name, Description, Handler, Arguments) ->
    case is_running() of
        true ->
            erlmcp_stdio_server:add_prompt(Name, Description, Handler, Arguments);
        false ->
            {error, stdio_server_not_running}
    end.

-spec is_running() -> boolean().
is_running() ->
    case whereis(erlmcp_stdio_supervisor) of
        undefined -> false;
        Pid when is_pid(Pid) -> is_process_alive(Pid)
    end.