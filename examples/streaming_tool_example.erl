-module(streaming_tool_example).

%% Example demonstrating streaming tool execution in erlmcp

-export([
    start_server/0,
    register_streaming_tools/1,
    call_streaming_tool/2,
    demo/0
]).

-include("erlmcp.hrl").

%% @doc Start an erlmcp server with streaming-capable tools
start_server() ->
    %% Define server capabilities
    Capabilities = #mcp_server_capabilities{
        tools = #mcp_capability{enabled = true},
        resources = #mcp_capability{enabled = true},
        prompts = #mcp_capability{enabled = true}
    },

    %% Start the server
    {ok, Server} = erlmcp_server:start_link({local, streaming_demo}, Capabilities),

    %% Register streaming tools
    register_streaming_tools(Server),

    {ok, Server}.

%% @doc Register example streaming tools
register_streaming_tools(Server) ->
    %% Tool 1: Data processing that returns chunks
    erlmcp_server:add_tool(Server, <<"process_data">>, fun(Args) ->
        Count = maps:get(<<"count">>, Args, 5),
        Delay = maps:get(<<"delay">>, Args, 100),

        %% Return chunks as a list
        lists:map(fun(N) ->
            timer:sleep(Delay),
            #{
                <<"chunk">> => N,
                <<"timestamp">> => erlang:system_time(millisecond),
                <<"data">> => <<"Processing chunk #", (integer_to_binary(N))/binary>>
            }
        end, lists:seq(1, Count))
    end),

    %% Tool 2: Log file reader that streams lines
    erlmcp_server:add_tool(Server, <<"stream_logs">>, fun(Args) ->
        LogFile = maps:get(<<"file">>, Args, <<"example.log">>),

        %% Return log lines as chunks
        {chunks, [
            #{<<"line">> => 1, <<"content">> => <<"[INFO] Application started">>},
            #{<<"line">> => 2, <<"content">> => <<"[DEBUG] Loading configuration">>},
            #{<<"line">> => 3, <<"content">> => <<"[INFO] Server listening on port 8080">>},
            #{<<"line">> => 4, <<"content">> => <<"[DEBUG] Request received: GET /">>},
            #{<<"line">> => 5, <<"content">> => <<"[INFO] Request completed: 200 OK">>}
        ]}
    end),

    %% Tool 3: Long computation that provides progress
    erlmcp_server:add_tool(Server, <<"long_computation">>, fun(Args) ->
        Steps = maps:get(<<"steps">>, Args, 10),

        %% Simulate long computation with progress chunks
        lists:map(fun(Step) ->
            timer:sleep(500), % Simulate work
            Progress = (Step / Steps) * 100,
            #{
                <<"step">> => Step,
                <<"progress">> => Progress,
                <<"status">> => if
                    Step < Steps -> <<"processing">>;
                    true -> <<"complete">>
                end,
                <<"message">> => <<"Completed step ", (integer_to_binary(Step))/binary,
                                   " of ", (integer_to_binary(Steps))/binary>>
            }
        end, lists:seq(1, Steps))
    end),

    ok.

%% @doc Call a streaming tool and handle chunks
call_streaming_tool(Server, ToolName) ->
    call_streaming_tool(Server, ToolName, #{}).

call_streaming_tool(Server, ToolName, Args) ->
    %% Call tool with streaming enabled
    Request = #{
        <<"name">> => ToolName,
        <<"arguments">> => Args,
        <<"stream">> => true  % Enable streaming
    },

    %% Send the request (in real usage, this would go through transport)
    gen_server:call(Server, {call_tool, ToolName, Args, #{stream => true}}).

%% @doc Demo function showing streaming in action
demo() ->
    io:format("~n=== erlmcp Streaming Tool Demo ===~n~n"),

    %% Start server
    {ok, Server} = start_server(),
    io:format("[1] Started server with streaming tools~n"),

    %% Example 1: Stream data processing
    io:format("~n[2] Calling process_data tool with streaming...~n"),

    %% In a real scenario, you would:
    %% 1. Client sends: {tools/call, #{name => <<"process_data">>, stream => true}}
    %% 2. Server responds: {ok, #{streamId => Ref, _meta => #{progressToken => Token}}}
    %% 3. Client receives: {stream_chunk, Ref, Chunk1}
    %% 4. Client receives: {stream_chunk, Ref, Chunk2}
    %% ...
    %% 5. Client receives: {stream_complete, Ref, #{total_chunks => N}}

    %% Simulate receiving stream
    io:format("   Client would receive:~n"),
    io:format("     {stream_start, StreamId, ProgressToken}~n"),
    io:format("     {stream_chunk, StreamId, #{chunk => 1, data => ...}}~n"),
    io:format("     {stream_chunk, StreamId, #{chunk => 2, data => ...}}~n"),
    io:format("     ...~n"),
    io:format("     {stream_complete, StreamId}~n"),

    %% Example 2: Stream log file
    io:format("~n[3] Calling stream_logs tool...~n"),
    io:format("   Client would receive each log line as a separate chunk~n"),

    %% Example 3: Long computation
    io:format("~n[4] Calling long_computation tool...~n"),
    io:format("   Client receives progress updates as computation proceeds~n"),

    %% Cleanup
    erlmcp_server:stop(Server),

    io:format("~n=== Demo Complete ===~n~n"),
    ok.

%% @doc Example of how a client would handle streaming responses
-spec handle_stream_response(reference(), term()) -> ok.
handle_stream_response(StreamId, Message) ->
    case Message of
        {stream_chunk, StreamId, ChunkData} ->
            %% Handle individual chunk
            ChunkNum = maps:get(<<"chunk_num">>, ChunkData),
            Data = maps:get(<<"data">>, ChunkData),
            io:format("Received chunk ~p: ~p~n", [ChunkNum, Data]),
            ok;

        {stream_complete, StreamId, FinalResult} ->
            %% Stream completed successfully
            TotalChunks = maps:get(<<"total_chunks">>, FinalResult),
            io:format("Stream completed with ~p chunks~n", [TotalChunks]),
            ok;

        {stream_error, StreamId, Error} ->
            %% Stream encountered an error
            io:format("Stream error: ~p~n", [Error]),
            {error, Error};

        {stream_cancelled, StreamId} ->
            %% Stream was cancelled
            io:format("Stream cancelled~n"),
            cancelled;

        _Other ->
            %% Unknown message
            ok
    end.

%% Usage:
%%   1> streaming_tool_example:demo().
%%
%%   Or for real usage:
%%   1> {ok, Server} = streaming_tool_example:start_server().
%%   2> %% Client connects and calls tool with stream option
%%   3> %% Client receives chunks progressively as tool executes
