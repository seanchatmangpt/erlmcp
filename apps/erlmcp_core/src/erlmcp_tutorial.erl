%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_tutorial - Interactive Tutorial Module
%%%
%%% Provides guided learning experience for ErlMCP users.
%%% Tracks progress and offers interactive lessons.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_tutorial).

%% API
-export([start/0,
         start/1,
         list_lessons/0,
         resume/0,
         reset/0,
         get_progress/0,
         complete_lesson/1,
         skip_lesson/1,
         get_all_lessons/0,
         handle_learn_command/1,
         format_progress/0]).

%% Tutorial state
-record(tutorial_state, {
    current_lesson = 1 :: pos_integer(),
    completed = [] :: [pos_integer()],
    started_at :: integer() | undefined,
    last_activity :: integer() | undefined
}).

-record(lesson, {
    id :: pos_integer(),
    title :: binary(),
    description :: binary(),
    content :: binary(),
    tasks :: [binary()],
    quiz :: quiz() | undefined
}).

-type quiz() :: #{
    questions => [quiz_question()],
    passing_score => number()
}.

-type quiz_question() :: #{
    question => binary(),
    options => [binary()],
    correct => pos_integer()
}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start tutorial from beginning
-spec start() -> ok.
start() ->
    start(1).

%% @doc Start tutorial from specific lesson
-spec start(pos_integer()) -> ok.
start(LessonId) when is_integer(LessonId), LessonId > 0 ->
    State = #tutorial_state{
        current_lesson = LessonId,
        completed = [],
        started_at = erlang:system_time(millisecond),
        last_activity = erlang:system_time(millisecond)
    },
    save_state(State),
    show_lesson(LessonId),
    ok.

%% @doc List all available lessons
-spec list_lessons() -> ok.
list_lessons() ->
    Lessons = get_all_lessons(),
    io:format("~n=== ErlMCP Tutorial Lessons ===~n~n"),
    lists:foreach(fun(Lesson) ->
        #lesson{id = Id, title = Title, description = Desc} = Lesson,
        io:format("[~p] ~s~n", [Id, Title]),
        io:format("    ~s~n~n", [Desc])
    end, Lessons),
    io:format("Use 'erlmcp learn <number>' to start a lesson.~n~n"),
    ok.

%% @doc Resume from where you left off
-spec resume() -> {ok, pos_integer()} | {error, no_progress}.
resume() ->
    case load_state() of
        {ok, #tutorial_state{current_lesson = LessonId}} ->
            io:format("Resuming from lesson ~p~n~n", [LessonId]),
            show_lesson(LessonId),
            {ok, LessonId};
        {error, no_progress} ->
            io:format("No tutorial progress found.~n"),
            io:format("Use 'erlmcp learn' to start from the beginning.~n"),
            {error, no_progress}
    end.

%% @doc Reset tutorial progress
-spec reset() -> ok.
reset() ->
    case file:delete("data/tutorial_state") of
        ok -> io:format("Tutorial progress reset.~n");
        {error, enoent} -> io:format("No progress to reset.~n")
    end,
    ok.

%% @doc Get current progress
-spec get_progress() -> {ok, map()} | {error, no_progress}.
get_progress() ->
    case load_state() of
        {ok, #tutorial_state{current_lesson = Current, completed = Completed}} = State ->
            Total = length(get_all_lessons()),
            Percentage = (length(Completed) * 100) div Total,
            {ok, #{
                current_lesson => Current,
                completed_lessons => length(Completed),
                total_lessons => Total,
                progress_percent => Percentage,
                started_at => State#tutorial_state.started_at,
                last_activity => State#tutorial_state.last_activity
            }};
        Error ->
            Error
    end.

%% @doc Mark lesson as complete
-spec complete_lesson(pos_integer()) -> ok | {error, term()}.
complete_lesson(LessonId) when is_integer(LessonId) ->
    case load_state() of
        {ok, State} ->
            NewCompleted = lists:usort([LessonId | State#tutorial_state.completed]),
            NewState = State#tutorial_state{
                completed = NewCompleted,
                current_lesson = LessonId + 1,
                last_activity = erlang:system_time(millisecond)
            },
            save_state(NewState),
            io:format("~n✓ Lesson ~p completed!~n", [LessonId]),
            case LessonId + 1 > length(get_all_lessons()) of
                true ->
                    io:format("~n🎉 Congratulations! You've completed all lessons!~n~n");
                false ->
                    io:format("Next: erlmcp learn ~p~n~n", [LessonId + 1])
            end,
            ok;
        Error ->
            Error
    end.

%% @doc Skip a lesson
-spec skip_lesson(pos_integer()) -> ok.
skip_lesson(LessonId) ->
    io:format("Skipped lesson ~p~n", [LessonId]),
    complete_lesson(LessonId).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Show lesson content
show_lesson(LessonId) ->
    Lessons = get_all_lessons(),
    case LessonId > length(Lessons) of
        true ->
            io:format("~nAll lessons completed! Use 'erlmcp learn --reset' to start over.~n~n");
        false ->
            Lesson = lists:nth(LessonId, Lessons),
            #lesson{
                title = Title,
                description = Desc,
                content = Content,
                tasks = Tasks,
                quiz = Quiz
            } = Lesson,

            io:format("~n=== Lesson ~p: ~s ===~n", [LessonId, Title]),
            io:format("~s~n~n", [Desc]),
            io:format("~s~n~n", [Content]),

            case Tasks of
                [] -> ok;
                _ ->
                    io:format("Tasks:~n"),
                    lists:foreach(fun({I, Task}) ->
                        io:format("  [~p] ~s~n", [I, Task])
                    end, enumerate(Tasks, 1)),
                    io:format("~n")
            end,

            case Quiz of
                undefined -> ok;
                _ ->
                    io:format("--- Quiz ---~n"),
                    show_quiz(Quiz, 1),
                    io:format("~n")
            end
    end.

%% @private Show quiz
show_quiz(Quiz, StartIndex) ->
    Questions = maps:get(questions, Quiz),
    lists:foreach(fun({I, Q}) ->
        #{
            question := QuestionText,
            options := Options,
            correct := _Correct
        } = Q,
        io:format("~nQ~p: ~s~n", [StartIndex + I - 1, QuestionText]),
        lists:foreach(fun({J, Opt}) ->
            io:format("  [~p] ~s~n", [J, Opt])
        end, enumerate(Options, 1))
    end, enumerate(Questions, 1)),
    io:format("~nAnswer with option number (1-~p).~n", [length(Questions)]).

%% @private Get all lessons
get_all_lessons() ->
    [
        #lesson{
            id = 1,
            title = <<"Introduction to ErlMCP">>,
            description = <<"Learn what ErlMCP is and how it works">>,
            content = <<"
ErlMCP is an Erlang/OTP implementation of the Model Context Protocol (MCP).

What is MCP?
- MCP is a protocol for connecting AI assistants to tools and data sources
- It uses JSON-RPC 2.0 for communication
- Supports multiple transports: stdio, SSE, WebSocket, TCP

Key Features:
- Built on Erlang/OTP for reliability
- Supports 100K+ concurrent connections
- Hot code reloading without downtime
- Distributed clustering support
">>,
            tasks = [
                <<"Install Erlang/OTP 27+">>,
                <<"Clone the erlmcp repository">>,
                <<"Run 'erlmcp init' to set up environment">>
            ],
            quiz = #{
                questions => [
                    #{
                        question => "What protocol does MCP use?",
                        options => ["gRPC", "JSON-RPC 2.0", "REST", "GraphQL"],
                        correct => 2
                    },
                    #{
                        question => "Which transport is NOT supported?",
                        options => ["stdio", "SSE", "WebSocket", "FTP"],
                        correct => 4
                    }
                ],
                passing_score => 0.5
            }
        },
        #lesson{
            id = 2,
            title = <<"Creating Your First Tool">>,
            description = <<"Implement and register a custom tool">>,
            content = <<"
Tools are functions that AI assistants can call.

Tool Structure:
- name: Unique identifier
- description: What the tool does
- inputSchema: JSON Schema for parameters
- handler: Erlang function that implements the logic

Example Tool:
```erlang
handle_echo(Args, _Context) ->
    Msg = maps:get(<<\"msg\">>, Args),
    {ok, #{content => Msg}}.
```
">>,
            tasks = [
                <<"Create a new tool module">>,
                <<"Implement the handle/2 function">>,
                <<"Register the tool in config">>,
                <<"Test with erlmcp test-100k">>
            ],
            quiz = #{
                questions => [
                    #{
                        question => "What field is required in all tools?",
                        options => ["name", "description", "inputSchema", "All of the above"],
                        correct => 4
                    }
                ],
                passing_score => 0.5
            }
        },
        #lesson{
            id = 3,
            title = <<"Working with Resources">>,
            description = <<"Expose data sources to AI assistants">>,
            content = <<"
Resources provide read-only access to data.

Resource Features:
- URI-based addressing
- MIME type metadata
- Change notifications (subscribe/unsubscribe)
- Batch listing

Resource Lifecycle:
1. Register resource with URI
2. Implement list/0 callback
3. Implement read/1 callback
4. Optional: subscribe/0 and unsubscribe/0
">>,
            tasks = [
                <<"Create a resource module">>,
                <<"Implement list/0 to return available URIs">>,
                <<"Implement read/1 to return content">>,
                <<"Test resource access">>
            ],
            quiz = undefined
        },
        #lesson{
            id = 4,
            title = <<"Error Handling and Logging">>,
            description = <<"Proper error handling and observability">>,
            content = <<"
Error Handling Patterns:
- Return {ok, Result} for success
- Return {error, Reason} for expected errors
- Use try/catch for unexpected errors
- Log errors with appropriate levels

Logging Levels:
- debug: Detailed diagnostics
- info: General information
- warning: Something unexpected but not critical
- error: Failure that needs attention

OpenTelemetry Integration:
- Automatic trace generation
- Span correlation
- Metrics collection
">>,
            tasks = [
                <<"Add error handling to your tool">>,
                <<"Add logging at key points">>,
                <<"View traces in dashboard">>,
                <<"Test error scenarios">>
            ],
            quiz = undefined
        },
        #lesson{
            id = 5,
            title = <<"Production Deployment">>,
            description = <<"Deploy ErlMCP to production">>,
            content = <<"
Production Checklist:
- Use proper configuration profiles (prod)
- Enable authentication
- Set up TLS certificates
- Configure resource limits
- Set up monitoring

Release Process:
1. Build release: rebar3 release
2. Test in staging environment
3. Run load tests
4. Deploy with rolling updates
5. Monitor health metrics

Docker Deployment:
```bash
docker build -t erlmcp:3.0.0 .
docker run -p 6000:6000 erlmcp:3.0.0
```
">>,
            tasks = [
                <<"Create production config">>,
                <<"Build release tarball">>,
                <<"Test deployment locally">>,
                <<"Deploy to staging">>
            ],
            quiz = undefined
        }
    ].

%% @private Save tutorial state
save_state(State) ->
    Binary = term_to_binary(State, [compressed]),
    filelib:ensure_dir("data/"),
    file:write_file("data/tutorial_state", Binary).

%% @private Load tutorial state
load_state() ->
    case file:read_file("data/tutorial_state") of
        {ok, Binary} ->
            {ok, binary_to_term(Binary)};
        {error, _} ->
            {error, no_progress}
    end.

%% @private Enumerate list
enumerate(List, Start) ->
    lists:zip(lists:seq(Start, Start + length(List) - 1), List).

%%====================================================================
%% CLI Integration
%%====================================================================

%% @doc Handle learn command from CLI
-spec handle_learn_command([string()]) -> ok.
handle_learn_command([]) ->
    start();
handle_learn_command(["--list"]) ->
    list_lessons();
handle_learn_command(["--resume"]) ->
    case resume() of
        {ok, _} -> ok;
        {error, _} -> start()
    end;
handle_learn_command(["--reset"]) ->
    reset();
handle_learn_command([Arg]) ->
    case string:to_integer(Arg) of
        {LessonId, ""} when LessonId > 0 ->
            start(LessonId);
        _ ->
            io:format("Invalid lesson number: ~s~n", [Arg]),
            io:format("Use 'erlmcp learn --list' to see available lessons.~n"),
            halt(1)
    end;
handle_learn_command(Args) ->
    io:format("Unknown options: ~p~n", [Args]),
    io:format("Usage: erlmcp learn [--list|--resume|--reset|<lesson-number>]~n"),
    halt(1).

%% @doc Format progress for display
-spec format_progress() -> iolist().
format_progress() ->
    case get_progress() of
        {ok, Progress} ->
            Current = maps:get(current_lesson, Progress),
            Completed = maps:get(completed_lessons, Progress),
            Total = maps:get(total_lessons, Progress),
            Percent = maps:get(progress_percent, Progress),
            io_lib:format("Progress: ~p/~p lessons (~p%)~nCurrent: Lesson ~p~n",
                          [Completed, Total, Percent, Current]);
        {error, no_progress} ->
            "No progress yet. Start with 'erlmcp learn'~n"
    end.
