-module(erlmcp_dev_portal_support).

-behaviour(gen_server).

%% API exports
-export([start_link/0, create_ticket/1, get_ticket/1, get_tickets/0, get_tickets_by_status/1, reply_to_ticket/2, search_tickets/1]).

%%====================================================================
%% API Functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_ticket(Ticket) ->
    %% Create support ticket
    gen_server:call(?MODULE, {create_ticket, Ticket}).

get_ticket(TicketId) ->
    %% Get support ticket by ID
    gen_server:call(?MODULE, {get_ticket, TicketId}).

get_tickets() ->
    %% Get all support tickets
    gen_server:call(?MODULE, get_tickets).

get_tickets_by_status(Status) ->
    %% Get tickets by status
    gen_server:call(?MODULE, {get_tickets_by_status, Status}).

reply_to_ticket(TicketId, Reply) ->
    %% Reply to support ticket
    gen_server:call(?MODULE, {reply_to_ticket, TicketId, Reply}).

search_tickets(Query) ->
    %% Search support tickets
    gen_server:call(?MODULE, {search_tickets, Query}).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    %% Initialize support management
    case mnesia:create_table(support_tickets, [
        {attributes, record_info(fields, support_ticket)},
        {disc_copies, [node()]},
        {type, set},
        {index, #support_ticket.status},
        {index, #support_ticket.category},
        {index, #support_ticket.user_id}
    ]) of
        {atomic, ok} ->
            ok;
        {aborted, {already_exists, support_tickets}} ->
            ok
    end,

    case mnesia:create_table(support_replies, [
        {attributes, record_info(fields, support_reply)},
        {disc_copies, [node()]},
        {type, set},
        {index, #support_reply.ticket_id}
    ]) of
        {atomic, ok} ->
            ok;
        {aborted, {already_exists, support_replies}} ->
            ok
    end,

    {ok, #{}}.

handle_call({create_ticket, TicketData}, _From, State) ->
    %% Create support ticket
    TicketId = ?GENERATE_ID(),
    Ticket = #support_ticket{
        id = TicketId,
        user_id = maps:get(user_id, TicketData),
        subject = maps:get(subject, TicketData),
        description = maps:get(description, TicketData),
        category = maps:get(category, TicketData),
        status = open,
        priority = maps:get(priority, TicketData, normal),
        created = ?TIMESTAMP(),
        updated = ?TIMESTAMP(),
        replies = []
    },
    case save_support_ticket(Ticket) of
        {atomic, ok} ->
            {reply, {ok, Ticket}, State};
        {atomic, {error, Reason}} ->
            {reply, {error, Reason}, State}
    end;

handle_call({get_ticket, TicketId}, _From, State) ->
    %% Get support ticket by ID
    case mnesia:dirty_read(support_tickets, TicketId) of
        [Ticket] ->
            {reply, {ok, Ticket}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call(get_tickets, _From, State) ->
    %% Get all support tickets
    Tickets = mnesia:dirty_all_objects(support_tickets),
    SortedTickets = lists:sort(fun(A, B) ->
        A#support_ticket.created >= B#support_ticket.created
    end, Tickets),
    {reply, {ok, SortedTickets}, State};

handle_call({get_tickets_by_status, Status}, _From, State) ->
    %% Get tickets by status
    Tickets = mnesia:dirty_all_objects(support_tickets),
    MatchingTickets = lists:filter(fun(Ticket) ->
        Ticket#support_ticket.status == Status
    end, Tickets),
    SortedTickets = lists:sort(fun(A, B) ->
        A#support_ticket.created >= B#support_ticket.created
    end, MatchingTickets),
    {reply, {ok, SortedTickets}, State};

handle_call({reply_to_ticket, TicketId, ReplyData}, _From, State) ->
    %% Reply to support ticket
    case mnesia:dirty_read(support_tickets, TicketId) of
        [Ticket] ->
            ReplyId = ?GENERATE_ID(),
            Reply = #support_reply{
                id = ReplyId,
                ticket_id = TicketId,
                content = maps:get(content, ReplyData),
                author = maps:get(author, ReplyData),
                created = ?TIMESTAMP(),
                type = maps:get(type, ReplyData, public)
            },
            case save_support_reply(Reply) of
                {atomic, ok} ->
                    %% Update ticket with new reply
                    UpdatedTicket = Ticket#support_ticket{
                        replies = [ReplyId | Ticket#support_ticket.replies],
                        updated = ?TIMESTAMP(),
                        status = case Reply#support_reply.type of
                            internal -> Ticket#support_ticket.status;
                            public -> in_progress
                        end
                    },
                    save_support_ticket(UpdatedTicket),
                    {reply, {ok, Reply}, State};
                {atomic, {error, Reason}} ->
                    {reply, {error, Reason}, State}
            end;
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call({search_tickets, Query}, _From, State) ->
    %% Search support tickets
    Tickets = mnesia:dirty_all_objects(support_tickets),
    MatchingTickets = lists:filter(fun(Ticket) ->
        case Ticket#support_ticket.subject of
            Subject when is_binary(Subject) ->
                case re:run(Subject, Query, [{case_insensitive, true}]) of
                    nomatch -> false;
                    _ -> true
                end;
            _ -> false
        end
    end, Tickets),
    {reply, {ok, MatchingTickets}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

save_support_ticket(Ticket) ->
    %% Save support ticket to mnesia
    F = fun() ->
        mnesia:write(Ticket)
    end,
    mnesia:transaction(F).

save_support_reply(Reply) ->
    %% Save support reply to mnesia
    F = fun() ->
        mnesia:write(Reply)
    end,
    mnesia:transaction(F).