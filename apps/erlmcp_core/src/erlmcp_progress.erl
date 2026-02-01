%%%-------------------------------------------------------------------
%%% @doc Progress token support for incremental updates
%%% Handles progress tracking and notification per MCP 2025-11-25 spec.
%%%
%%% == Progress Token Overview ==
%%% Progress tokens enable servers to send incremental updates during
%%% long-running operations (tool calls, resource reads, etc.).
%%%
%%% == Flow ==
%%% 1. Client provides progressToken in _meta field of request
%%% 2. Server creates progress tracker and sends notifications
%%% 3. Server updates progress incrementally
%%% 4. Server marks progress complete when done
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_progress).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([create/2, update/2, complete/1, cancel/1]).
-export([get_progress/1, generate_token/0]).
-export([track_tool_call/3, cleanup_completed/1]).
-export([encode_progress_notification/3]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("erlmcp.hrl").

%% Records
-record(progress_info,
        {token :: reference(),
         client_pid :: pid() | undefined,
         total :: number() | undefined,
         current :: number(),
         message :: binary() | undefined,
         start_time :: integer(),
         operation :: binary() | undefined,
         metadata :: map()}).
%% State record
-record(state,
        {progress :: #{reference() => #progress_info{}}, next_token_id = 1 :: non_neg_integer()}).

%% Type definitions
-type progress_token() :: reference().
-type progress_update() ::
    #{increment => number(),
      current => number(),
      total => number(),
      message => binary()}.
-type progress_info() :: #progress_info{}.

-export_type([progress_token/0, progress_update/0, progress_info/0]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Start the progress tracker server.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Create a new progress token with initial message.
%% Returns a reference that uniquely identifies this progress stream.
-spec create(pid() | undefined, binary()) -> progress_token().
create(ClientPid, Message) when is_pid(ClientPid); ClientPid =:= undefined ->
    Token = make_ref(),
    gen_server:cast(?MODULE, {create, Token, ClientPid, Message}),
    Token.

%% @doc Update progress with increment, current value, total, or message.
%% Update map can contain:
%%   - increment: Number to add to current
%%   - current: Absolute current value (overrides increment)
%%   - total: Total value for percentage calculation
%%   - message: Status message
-spec update(progress_token(), progress_update()) -> ok.
update(Token, Update) when is_reference(Token), is_map(Update) ->
    gen_server:cast(?MODULE, {update, Token, Update}),
    ok.

%% @doc Mark progress as complete and send final notification.
-spec complete(progress_token()) -> ok.
complete(Token) when is_reference(Token) ->
    gen_server:cast(?MODULE, {complete, Token}),
    ok.

%% @doc Cancel a progress stream and remove tracking.
-spec cancel(progress_token()) -> ok.
cancel(Token) when is_reference(Token) ->
    gen_server:cast(?MODULE, {cancel, Token}),
    ok.

%% @doc Get current progress state.
-spec get_progress(progress_token()) -> {ok, map()} | {error, not_found}.
get_progress(Token) when is_reference(Token) ->
    gen_server:call(?MODULE, {get_progress, Token}).

%% @doc Generate a unique progress token (reference).
-spec generate_token() -> progress_token().
generate_token() ->
    make_ref().

%% @doc Track a tool call with progress token.
%% Used by erlmcp_server to associate progress with tool execution.
-spec track_tool_call(progress_token(), binary(), pid()) -> ok.
track_tool_call(Token, ToolName, ServerPid) when is_reference(Token) ->
    create(ServerPid, <<"Tool execution started: ", ToolName/binary>>),
    ok.

%% @doc Clean up completed progress tracking.
-spec cleanup_completed(progress_token()) -> ok.
cleanup_completed(Token) when is_reference(Token) ->
    complete(Token),
    ok.

%% @doc Encode a progress notification for sending to client.
-spec encode_progress_notification(progress_token(), number(), number()) -> map().
encode_progress_notification(Token, Progress, Total) ->
    #{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
      ?JSONRPC_FIELD_METHOD => ?MCP_METHOD_NOTIFICATIONS_PROGRESS,
      ?JSONRPC_FIELD_PARAMS =>
          #{?MCP_PARAM_PROGRESS_TOKEN => Token,
            ?MCP_PARAM_PROGRESS => Progress,
            ?MCP_PARAM_TOTAL => Total}}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
-spec init([]) -> {ok, #state{}}.
init([]) ->
    logger:info("Starting progress tracker server"),
    {ok, #state{progress = #{}, next_token_id = 1}}.

%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.
handle_call({get_progress, Token}, _From, State) ->
    Reply =
        case maps:get(Token, State#state.progress, undefined) of
            undefined ->
                {error, not_found};
            Progress ->
                {ok, format_progress(Progress)}
        end,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({create, Token, ClientPid, Message}, State) ->
    Progress =
        #progress_info{token = Token,
                       client_pid = ClientPid,
                       total = undefined,
                       current = 0,
                       message = Message,
                       start_time = erlang:system_time(millisecond),
                       operation = undefined,
                       metadata = #{}},
    send_progress_notification(ClientPid, Token, 0, undefined, Message),
    {noreply, State#state{progress = (State#state.progress)#{Token => Progress}}};
handle_cast({update, Token, Update}, State) ->
    case maps:get(Token, State#state.progress, undefined) of
        undefined ->
            logger:warning("Progress token not found: ~p", [Token]),
            {noreply, State};
        Progress ->
            Progress1 = update_progress_info(Progress, Update),
            Percent = calc_percent(Progress1),
            send_progress_notification(Progress1#progress_info.client_pid,
                                       Token,
                                       Percent,
                                       Progress1#progress_info.total,
                                       Progress1#progress_info.message),
            {noreply, State#state{progress = (State#state.progress)#{Token => Progress1}}}
    end;
handle_cast({complete, Token}, State) ->
    case maps:get(Token, State#state.progress, undefined) of
        undefined ->
            logger:warning("Progress token not found for completion: ~p", [Token]),
            {noreply, State};
        Progress ->
            send_complete_notification(Progress),
            {noreply, State#state{progress = maps:remove(Token, State#state.progress)}}
    end;
handle_cast({cancel, Token}, State) ->
    case maps:get(Token, State#state.progress, undefined) of
        undefined ->
            logger:debug("Progress token not found for cancellation: ~p", [Token]),
            {noreply, State};
        _Progress ->
            logger:debug("Progress token cancelled: ~p", [Token]),
            {noreply, State#state{progress = maps:remove(Token, State#state.progress)}}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, #state{progress = ProgressMap}) ->
    logger:info("Progress tracker terminating with ~p active streams", [maps:size(ProgressMap)]),
    ok.

%% @private
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% Update progress info based on update map
%% Handles multiple keys in a single update
-spec update_progress_info(#progress_info{}, progress_update()) -> #progress_info{}.
update_progress_info(Progress, Update) when is_map(Update) ->
    %% Apply all updates in sequence
    Progress1 =
        case maps:get(increment, Update, undefined) of
            undefined ->
                Progress;
            Inc when is_number(Inc) ->
                Progress#progress_info{current = Progress#progress_info.current + Inc}
        end,
    Progress2 =
        case maps:get(current, Update, undefined) of
            undefined ->
                Progress1;
            Cur when is_number(Cur) ->
                Progress1#progress_info{current = Cur}
        end,
    Progress3 =
        case maps:get(total, Update, undefined) of
            undefined ->
                Progress2;
            Total when is_number(Total) ->
                Progress2#progress_info{total = Total}
        end,
    Progress4 =
        case maps:get(message, Update, undefined) of
            undefined ->
                Progress3;
            Msg when is_binary(Msg) ->
                Progress3#progress_info{message = Msg}
        end,
    Progress4.

%% @private
%% Calculate percentage complete
-spec calc_percent(#progress_info{}) -> number() | undefined.
calc_percent(#progress_info{total = undefined}) ->
    undefined;
calc_percent(#progress_info{total = 0}) ->
    undefined;
calc_percent(#progress_info{total = Total, current = Cur}) when is_number(Total), is_number(Cur) ->
    trunc(Cur / Total * 100).

%% @private
%% Send progress notification to client
-spec send_progress_notification(pid() | undefined,
                                 progress_token(),
                                 number() | undefined,
                                 number() | undefined,
                                 binary()) ->
                                    ok.
send_progress_notification(undefined, _Token, _Percent, _Total, _Message) ->
    ok;
send_progress_notification(ClientPid, Token, Percent, Total, Message) when is_pid(ClientPid) ->
    Notification =
        #{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
          ?JSONRPC_FIELD_METHOD => ?MCP_METHOD_NOTIFICATIONS_PROGRESS,
          ?JSONRPC_FIELD_PARAMS =>
              #{?MCP_PARAM_PROGRESS_TOKEN => Token,
                ?MCP_PARAM_PROGRESS => Percent,
                ?MCP_PARAM_TOTAL => Total,
                <<"message">> => Message}},
    ClientPid ! {mcp_notification, Notification},
    ok.

%% @private
%% Send complete notification (100% progress)
-spec send_complete_notification(#progress_info{}) -> ok.
send_complete_notification(#progress_info{token = Token,
                                          client_pid = ClientPid,
                                          message = Message}) ->
    Notification =
        #{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
          ?JSONRPC_FIELD_METHOD => ?MCP_METHOD_NOTIFICATIONS_PROGRESS,
          ?JSONRPC_FIELD_PARAMS =>
              #{?MCP_PARAM_PROGRESS_TOKEN => Token,
                ?MCP_PARAM_PROGRESS => 100,
                ?MCP_PARAM_TOTAL => 100,
                <<"message">> => <<Message/binary, " - Complete">>}},
    case ClientPid of
        undefined ->
            ok;
        _ when is_pid(ClientPid) ->
            ClientPid ! {mcp_notification, Notification},
            ok
    end.

%% @private
%% Format progress info for API response
-spec format_progress(#progress_info{}) -> map().
format_progress(#progress_info{token = Token,
                               current = Current,
                               total = Total,
                               message = Message,
                               start_time = StartTime}) ->
    Elapsed = erlang:system_time(millisecond) - StartTime,
    Percent = calc_percent(#progress_info{total = Total, current = Current}),
    #{token => Token,
      current => Current,
      total => Total,
      progress => Percent,
      message => Message,
      elapsed_ms => Elapsed}.
