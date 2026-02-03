-module(erlmcp_dev_portal_users).

-behaviour(gen_server).

%% API exports
-export([start_link/0, authenticate/2, create/3, get_user/1, get_user_by_email/1, update_user/2, delete_user/1]).

%%====================================================================
%% API Functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

authenticate(Email, Password) ->
    %% Authenticate user with email and password
    gen_server:call(?MODULE, {authenticate, Email, Password}).

create(Name, Email, Password) ->
    %% Create new user
    gen_server:call(?MODULE, {create, Name, Email, Password}).

get_user(UserId) ->
    %% Get user by ID
    gen_server:call(?MODULE, {get_user, UserId}).

get_user_by_email(Email) ->
    %% Get user by email
    gen_server:call(?MODULE, {get_user_by_email, Email}).

update_user(UserId, Updates) ->
    %% Update user information
    gen_server:call(?MODULE, {update_user, UserId, Updates}).

delete_user(UserId) ->
    %% Delete user (mark as deleted)
    gen_server:call(?MODULE, {delete_user, UserId}).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    %% Initialize user management
    case mnesia:create_table(users, [
        {attributes, record_info(fields, user)},
        {disc_copies, [node()]},
        {type, set},
        {index, #user.email}
    ]) of
        {atomic, ok} ->
            ok;
        {aborted, {already_exists, users}} ->
            ok
    end,

    {ok, #{}}.

handle_call({authenticate, Email, Password}, _From, State) ->
    %% Authenticate user
    case mnesia:dirty_index_read(users, Email, #user.email) of
        [User = #user{password_hash = Hash}] ->
            case verify_password(Password, Hash) of
                true ->
                    %% Update last login
                    UpdatedUser = User#user{
                        last_login = ?TIMESTAMP()
                    },
                    save_user(UpdatedUser),
                    {reply, {ok, User}, State};
                false ->
                    {reply, {error, invalid_credentials}, State}
            end;
        [] ->
            {reply, {error, invalid_credentials}, State}
    end;

handle_call({create, Name, Email, Password}, _From, State) ->
    %% Create new user
    case validate_user_input(Name, Email, Password) of
        ok ->
            UserId = ?GENERATE_ID(),
            PasswordHash = hash_password(Password),
            User = #user{
                id = UserId,
                name = Name,
                email = Email,
                password_hash = PasswordHash,
                created = ?TIMESTAMP(),
                last_login = undefined,
                api_keys = [],
                preferences = #{}
            },
            case save_user(User) of
                {atomic, ok} ->
                    {reply, {ok, User}, State};
                {atomic, {error, Reason}} ->
                    {reply, {error, Reason}, State}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({get_user, UserId}, _From, State) ->
    %% Get user by ID
    case mnesia:dirty_read(users, UserId) of
        [User] ->
            {reply, {ok, User}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_user_by_email, Email}, _From, State) ->
    %% Get user by email
    case mnesia:dirty_index_read(users, Email, #user.email) of
        [User] ->
            {reply, {ok, User}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call({update_user, UserId, Updates}, _From, State) ->
    %% Update user information
    case mnesia:dirty_read(users, UserId) of
        [User] ->
            UpdatedUser = apply_updates(User, Updates),
            case save_user(UpdatedUser) of
                {atomic, ok} ->
                    {reply, {ok, UpdatedUser}, State};
                {atomic, {error, Reason}} ->
                    {reply, {error, Reason}, State}
            end;
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call({delete_user, UserId}, _From, State) ->
    %% Delete user (mark as deleted)
    case mnesia:dirty_read(users, UserId) of
        [User] ->
            DeletedUser = User#user{
                email = list_to_binary([User#user.email, "_deleted_", ?TIMESTAMP()])
            },
            case save_user(DeletedUser) of
                {atomic, ok} ->
                    {reply, {ok, deleted}, State};
                {atomic, {error, Reason}} ->
                    {reply, {error, Reason}, State}
            end;
        [] ->
            {reply, {error, not_found}, State}
    end;

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

validate_user_input(Name, Email, Password) ->
    %% Validate user input
    case Name of
        undefined -> {error, name_required};
        _ when length(Name) < 3 -> {error, name_too_short};
        _ -> ok
    end,

    case Email of
        undefined -> {error, email_required};
        _ when not ?IS_VALID_EMAIL(Email) -> {error, invalid_email};
        _ when email_exists(Email) -> {error, email_exists};
        _ -> ok
    end,

    case Password of
        undefined -> {error, password_required};
        _ when not ?IS_VALID_PASSWORD(Password) -> {error, password_too_short};
        _ -> ok
    end.

email_exists(Email) ->
    %% Check if email already exists
    case mnesia:dirty_index_read(users, Email, #user.email) of
        [] -> false;
        [_] -> true
    end.

hash_password(Password) ->
    %% Hash password using bcrypt
    case bcrypt:hash(Password) of
        {ok, Hash} -> Hash;
        {error, _} -> error
    end.

verify_password(Password, Hash) ->
    %% Verify password against hash
    case bcrypt:verify(Password, Hash) of
        true -> true;
        false -> false
    end.

save_user(User) ->
    %% Save user to mnesia
    F = fun() ->
        mnesia:write(User)
    end,
    mnesia:transaction(F).

apply_updates(User, Updates) ->
    %% Apply updates to user record
    maps:fold(fun(Key, Value, Acc) ->
        case Key of
            name -> Acc#user{name = Value};
            email -> Acc#user{email = Value};
            password -> Acc#user{password_hash = hash_password(Value)};
            preferences -> Acc#user{preferences = Value};
            _ -> Acc
        end
    end, User, Updates).