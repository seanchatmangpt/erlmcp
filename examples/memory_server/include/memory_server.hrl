-ifndef(MEMORY_SERVER_HRL).
-define(MEMORY_SERVER_HRL, true).



-record(entity, {
    name :: binary(),
    entity_type :: binary(),
    observations :: [binary()]
}).

-record(relation, {
    from :: binary(),
    to :: binary(),
    relation_type :: binary()
}).

-record(knowledge_graph, {
    entities :: [#entity{}],
    relations :: [#relation{}]
}).

-record(state, {
    knowledge_graph :: #knowledge_graph{},
    memory_file_path :: binary(),
    mcp_server :: pid()
}).

-endif.
