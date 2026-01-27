%%%-------------------------------------------------------------------
%%% @doc MCP 2025-11-25 Cursor-Based Pagination Helper Module
%%%
%%% Implements cursor-based pagination for list endpoints per MCP specification.
%%% Cursors are opaque, base64-encoded JSON structures that encode offset and page size.
%%%
%%% Features:
%%% - Cursor generation and encoding
%%% - Cursor validation and decoding
%%% - Next cursor generation with "has more" detection
%%% - Support for all list endpoints (resources, tools, prompts, etc.)
%%% - Backward compatible (cursor parameter is optional)
%%%
%%% Implementation Note:
%%% Cursors are base64-encoded JSON: {offset: integer(), pagesize: integer()}
%%% This format allows clients to pass cursors without modification while
%%% remaining opaque to client interpretation.
%%%
%%% @since 0.6.0
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_pagination).

-export([
    encode_cursor/2,
    decode_cursor/1,
    validate_cursor/1,
    generate_next_cursor/3,
    paginate_list/4,
    apply_pagination/3
]).

%% Types
-export_type([
    cursor/0,
    pagination_params/0,
    paginated_result/0
]).

-type cursor() :: binary() | undefined.
-type pagination_params() :: #{
    cursor => cursor(),
    limit => pos_integer()
}.
-type paginated_result() :: #{
    items => [term()],
    nextCursor => cursor() | undefined,
    totalCount => non_neg_integer() | undefined
}.

%% Default page size (spec: typically 100 items per page)
-define(DEFAULT_PAGE_SIZE, 100).
-define(MAX_PAGE_SIZE, 1000).
-define(MIN_PAGE_SIZE, 1).

%%====================================================================
%% Cursor Operations
%%====================================================================

%% @doc Generate a cursor from offset and page size
%%
%% Cursor format: base64(json({offset, pagesize}))
%%
%% Example:
%% ```
%% Cursor1 = erlmcp_pagination:encode_cursor(0, 100),
%% Cursor2 = erlmcp_pagination:encode_cursor(100, 100),
%% '''
%%
%% @param Offset Starting index for items (0-based)
%% @param PageSize Number of items per page
%% @return Opaque cursor binary
%% @end
-spec encode_cursor(non_neg_integer(), pos_integer()) -> binary().
encode_cursor(Offset, PageSize) when is_integer(Offset), Offset >= 0,
                                      is_integer(PageSize), PageSize > 0 ->
    CursorData = jsx:encode(#{<<"offset">> => Offset, <<"pagesize">> => PageSize}),
    base64:encode(CursorData).

%% @doc Decode a cursor to extract pagination parameters
%%
%% Returns {ok, {Offset, PageSize}} on success,
%% {error, invalid_cursor} on decode failure.
%%
%% @param Cursor Opaque cursor binary
%% @return {ok, {Offset :: non_neg_integer(), PageSize :: pos_integer()}} |
%%         {error, invalid_cursor}
%% @end
-spec decode_cursor(cursor()) -> {ok, {non_neg_integer(), pos_integer()}} |
                                  {error, invalid_cursor}.
decode_cursor(undefined) ->
    {ok, {0, ?DEFAULT_PAGE_SIZE}};
decode_cursor(Cursor) when is_binary(Cursor) ->
    try
        DecodedData = base64:decode(Cursor),
        CursorMap = jsx:decode(DecodedData, [return_maps]),
        Offset = maps:get(<<"offset">>, CursorMap, 0),
        PageSize = maps:get(<<"pagesize">>, CursorMap, ?DEFAULT_PAGE_SIZE),

        %% Validate bounds
        case validate_bounds(Offset, PageSize) of
            ok -> {ok, {Offset, PageSize}};
            {error, Reason} -> {error, Reason}
        end
    catch
        _:_ -> {error, invalid_cursor}
    end;
decode_cursor(_) ->
    {error, invalid_cursor}.

%% @doc Validate cursor format without decoding
%%
%% Performs basic validation that cursor is properly formatted.
%%
%% @param Cursor Cursor binary to validate
%% @return ok | {error, invalid_cursor}
%% @end
-spec validate_cursor(cursor()) -> ok | {error, invalid_cursor}.
validate_cursor(undefined) ->
    ok;
validate_cursor(Cursor) when is_binary(Cursor) ->
    case decode_cursor(Cursor) of
        {ok, _} -> ok;
        {error, _} -> {error, invalid_cursor}
    end;
validate_cursor(_) ->
    {error, invalid_cursor}.

%% @doc Generate next cursor if more items available
%%
%% If HasMore is true, returns encoded cursor for next page.
%% Otherwise returns undefined (no more pages).
%%
%% @param CurrentOffset Current pagination offset
%% @param PageSize Items per page
%% @param HasMore Whether more items exist after current page
%% @return binary() | undefined
%% @end
-spec generate_next_cursor(non_neg_integer(), pos_integer(), boolean()) ->
    cursor() | undefined.
generate_next_cursor(_Offset, _PageSize, false) ->
    undefined;
generate_next_cursor(Offset, PageSize, true) ->
    NextOffset = Offset + PageSize,
    encode_cursor(NextOffset, PageSize).

%%====================================================================
%% Pagination Application
%%====================================================================

%% @doc Apply pagination to a list of items
%%
%% Extracts items for requested page from complete list and determines
%% if more items are available.
%%
%% Example:
%% ```
%% Items = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
%% Result = erlmcp_pagination:paginate_list(Items, undefined, 3, 100),
%% % Result: {ok, [1,2,3], false}
%% '''
%%
%% @param Items Complete list of items to paginate
%% @param Cursor Optional cursor from client (undefined for first page)
%% @param PageSize Number of items per page
%% @param TotalCount Total count of items (for nextCursor generation)
%% @return {ok, PagedItems :: [term()], HasMore :: boolean()} |
%%         {error, invalid_cursor}
%% @end
-spec paginate_list(list(), cursor(), pos_integer(), non_neg_integer()) ->
    {ok, [term()], boolean()} | {error, invalid_cursor}.
paginate_list(Items, Cursor, PageSize, _TotalCount) when is_list(Items),
                                                          is_integer(PageSize),
                                                          PageSize > 0 ->
    case decode_cursor(Cursor) of
        {ok, {Offset, _}} ->
            %% Extract page from list
            ItemCount = length(Items),
            HasMore = (Offset + PageSize) < ItemCount,
            PagedItems = lists:sublist(Items, Offset + 1, PageSize),
            {ok, PagedItems, HasMore};
        {error, Reason} ->
            {error, Reason}
    end;
paginate_list(_, _, _, _) ->
    {error, invalid_params}.

%% @doc Apply pagination parameters to list and return structured result
%%
%% Combines pagination, next cursor generation, and result formatting.
%%
%% Example:
%% ```
%% Items = [a, b, c, d, e],
%% Params = #{cursor => undefined, limit => 2},
%% Result = erlmcp_pagination:apply_pagination(Items, Params, false),
%% % Result: #{items => [a, b], nextCursor => Cursor, totalCount => 5}
%% '''
%%
%% @param Items List of items to paginate
%% @param Params Pagination parameters (cursor, limit)
%% @param IncludeTotalCount Whether to include totalCount in result
%% @return paginated_result()
%% @end
-spec apply_pagination(list(), pagination_params(), boolean()) ->
    paginated_result() | {error, term()}.
apply_pagination(Items, Params, IncludeTotalCount) when is_list(Items), is_map(Params) ->
    Cursor = maps:get(cursor, Params, undefined),
    PageSize = maps:get(limit, Params, ?DEFAULT_PAGE_SIZE),
    TotalCount = length(Items),

    %% Validate page size
    case validate_bounds(0, PageSize) of
        ok ->
            case decode_cursor(Cursor) of
                {ok, {Offset, _}} ->
                    %% Extract page from items
                    HasMore = (Offset + PageSize) < TotalCount,
                    PagedItems = lists:sublist(Items, Offset + 1, PageSize),
                    NextCursor = generate_next_cursor(Offset, PageSize, HasMore),

                    Result = #{
                        items => PagedItems,
                        nextCursor => NextCursor
                    },

                    %% Optionally include total count
                    case IncludeTotalCount of
                        true -> Result#{totalCount => TotalCount};
                        false -> Result
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end;
apply_pagination(_, _, _) ->
    {error, invalid_params}.

%%====================================================================
%% Validation Helpers
%%====================================================================

%% @doc Validate offset and page size bounds
-spec validate_bounds(non_neg_integer(), pos_integer()) ->
    ok | {error, invalid_bounds}.
validate_bounds(Offset, PageSize) when is_integer(Offset), Offset >= 0,
                                        is_integer(PageSize),
                                        PageSize >= ?MIN_PAGE_SIZE,
                                        PageSize =< ?MAX_PAGE_SIZE ->
    ok;
validate_bounds(Offset, _) when is_integer(Offset), Offset < 0 ->
    {error, invalid_bounds};
validate_bounds(_, PageSize) when is_integer(PageSize), PageSize < ?MIN_PAGE_SIZE ->
    {error, invalid_bounds};
validate_bounds(_, PageSize) when is_integer(PageSize), PageSize > ?MAX_PAGE_SIZE ->
    {error, invalid_bounds};
validate_bounds(_, _) ->
    {error, invalid_bounds}.

%%====================================================================
%% Private Helper Functions
%%====================================================================

%% @private @doc Ensure item is valid for encoding
-spec ensure_valid_item(term()) -> term().
ensure_valid_item(Item) when is_map(Item) -> Item;
ensure_valid_item(Item) when is_list(Item) -> Item;
ensure_valid_item(Item) when is_binary(Item) -> Item;
ensure_valid_item(Item) when is_atom(Item) -> Item;
ensure_valid_item(Item) when is_number(Item) -> Item;
ensure_valid_item(Item) -> Item.
