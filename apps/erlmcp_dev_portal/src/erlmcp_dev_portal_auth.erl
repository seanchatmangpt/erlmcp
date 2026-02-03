-module(erlmcp_dev_portal_auth).

-behaviour(cowboy_handler).

%% API exports
-export([init/2, handle/2, terminate/3]).

%%====================================================================
%% API Functions
%%====================================================================

init(Req, _Opts) ->
    %% Initialize auth state
    State = #{},
    {ok, Req, State}.

handle(Req, State) ->
    %% Handle auth requests
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),

    case Method of
        <<"GET">> ->
            handle_get_auth(Path, Req, State);
        <<"POST">> ->
            handle_post_auth(Path, Req, State);
        _ ->
            cowboy_req:reply(405, #{}, <<"Method not allowed">>, Req)
    end.

terminate(_Reason, _Req, _State) ->
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

handle_get_auth(<<"/login">>, Req, State) ->
    %% Return login page
    {ok, Body} = render_login_page(),
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Req);

handle_get_auth(<<"/register">>, Req, State) ->
    %% Return registration page
    {ok, Body} = render_register_page(),
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Req);

handle_get_auth(<<"/logout">>, Req, State) ->
    %% Handle logout
    NewReq = handle_logout(Req),
    cowboy_req:reply(302, #{<<"location">> => <<"/">>}, NewReq);

handle_get_auth(Path, Req, State) ->
    cowboy_req:reply(404, #{}, <<"Not found">>, Req).

handle_post_auth(<<"/login">>, Req, State) ->
    %% Handle login request
    {ok, Body, Req} = cowboy_req:read_body(Req),
    LoginData = jsx:decode(Body, [{labels, atom}]),

    case authenticate_user(LoginData) of
        {ok, User} ->
            SessionId = create_session(User),
            {ok, NewReq} = set_session_cookie(Req, SessionId),
            cowboy_req:reply(302, #{<<"location">> => <<"/dashboard">>}, NewReq);
        {error, invalid_credentials} ->
            cowboy_req:reply(401, #{<<"content-type">> => <<"text/html">>}, render_login_error(), Req)
    end;

handle_post_auth(<<"/register">>, Req, State) ->
    %% Handle registration request
    {ok, Body, Req} = cowboy_req:read_body(Req),
    RegistrationData = jsx:decode(Body, [{labels, atom}),

    case register_user(RegistrationData) of
        {ok, User} ->
            SessionId = create_session(User),
            {ok, NewReq} = set_session_cookie(Req, SessionId),
            cowboy_req:reply(302, #{<<"location">> => <<"/dashboard">>}, NewReq);
        {error, Reason} ->
            cowboy_req:reply(400, #{<<"content-type">> => <<"text/html">>}, render_register_error(Reason), Req)
    end;

handle_post_auth(Path, Req, State) ->
    cowboy_req:reply(404, #{}, <<"Not found">>, Req).

render_login_page() ->
    %% Render login page HTML
    Html = <<
        "<!DOCTYPE html>"
        "<html>"
        "<head><title>Login - erlmcp</title></head>"
        "<body>"
        "<h1>Login</h1>"
        "<div class='login-form'>"
        "<form id='login' method='POST'>"
        "<input type='text' name='email' placeholder='Email' required>"
        "<input type='password' name='password' placeholder='Password' required>"
        "<button type='submit'>Login</button>"
        "</form>"
        "<div class='links'>"
        "<a href='/register'>Don't have an account? Register</a>"
        "</div>"
        "</div>"
        "<script>"
        "document.getElementById('login').addEventListener('submit', function(e) {"
        "  e.preventDefault();"
        "  const email = this.querySelector('input[name=\"email\"]').value;"
        "  const password = this.querySelector('input[name=\"password\"]').value;"
        "  "
        "  fetch('/login', {"
        "    method: 'POST',"
        "    headers: {'Content-Type': 'application/json'},"
        "    body: JSON.stringify({email, password})"
        "  })"
        "    .then(response => {"
        "      if (response.redirected) {"
        "        window.location.href = response.url;"
        "      } else {"
        "        return response.text();"
        "      }"
        "    })"
        "    .then(html => {"
        "      if (html) {"
        "        document.querySelector('.login-form').innerHTML = html;"
        "      }"
        "    });"
        "});"
        "</script>"
        "</body>"
        "</html>"
    >>,
    {ok, Html}.

render_register_page() ->
    %% Render registration page HTML
    Html = <<
        "<!DOCTYPE html>"
        "<html>"
        "<head><title>Register - erlmcp</title></head>"
        "<body>"
        "<h1>Register</h1>"
        "<div class='register-form'>"
        "<form id='register' method='POST'>"
        "<input type='text' name='name' placeholder='Name' required>"
        "<input type='email' name='email' placeholder='Email' required>"
        "<input type='password' name='password' placeholder='Password' required>"
        "<input type='password' name='confirm_password' placeholder='Confirm Password' required>"
        "<button type='submit'>Register</button>"
        "</form>"
        "<div class='links'>"
        "<a href='/login'>Already have an account? Login</a>"
        "</div>"
        "</div>"
        "<script>"
        "document.getElementById('register').addEventListener('submit', function(e) {"
        "  e.preventDefault();"
        "  const name = this.querySelector('input[name=\"name\"]').value;"
        "  const email = this.querySelector('input[name=\"email\"]').value;"
        "  const password = this.querySelector('input[name=\"password\"]').value;"
        "  const confirmPassword = this.querySelector('input[name=\"confirm_password\"]').value;"
        "  "
        "  if (password !== confirmPassword) {"
        "    alert('Passwords do not match!');"
        "    return;"
        "  }"
        "  "
        "  fetch('/register', {"
        "    method: 'POST',"
        "    headers: {'Content-Type': 'application/json'},"
        "    body: JSON.stringify({name, email, password})"
        "  })"
        "    .then(response => {"
        "      if (response.redirected) {"
        "        window.location.href = response.url;"
        "      } else {"
        "        return response.text();"
        "      }"
        "    })"
        "    .then(html => {"
        "      if (html) {"
        "        document.querySelector('.register-form').innerHTML = html;"
        "      }"
        "    });"
        "});"
        "</script>"
        "</body>"
        "</html>"
    >>,
    {ok, Html}.

render_login_error() ->
    %% Render login error
    Html = <<
        "<!DOCTYPE html>"
        "<html>"
        "<head><title>Login Error - erlmcp</title></head>"
        "<body>"
        "<h1>Login Failed</h1>"
        "<p>Invalid email or password. Please try again.</p>"
        "<a href='/login'>Back to Login</a>"
        "</body>"
        "</html>"
    >>,
    {ok, Html}.

render_register_error(Reason) ->
    %% Render registration error
    Html = list_to_binary([
        "<!DOCTYPE html>"
        "<html>"
        "<head><title>Registration Error - erlmcp</title></head>"
        "<body>"
        "<h1>Registration Failed</h1>"
        "<p>", atom_to_list(Reason), "</p>"
        "<a href='/register'>Back to Registration</a>"
        "</body>"
        "</html>"
    ]),
    {ok, Html}.

authenticate_user(LoginData) ->
    %% Authenticate user
    Email = proplists:get_value(email, LoginData),
    Password = proplists:get_value(password, LoginData),

    %% Validate credentials (in production, use proper password hashing)
    case erlmcp_dev_portal_users:authenticate(Email, Password) of
        {ok, User} -> {ok, User};
        {error, invalid_credentials} -> {error, invalid_credentials}
    end.

register_user(RegistrationData) ->
    %% Register new user
    Name = proplists:get_value(name, RegistrationData),
    Email = proplists:get_value(email, RegistrationData),
    Password = proplists:get_value(password, RegistrationData),

    %% Validate input
    case validate_registration(Name, Email, Password) of
        ok ->
            %% Create user
            case erlmcp_dev_portal_users:create(Name, Email, Password) of
                {ok, User} -> {ok, User};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} -> {error, Reason}
    end.

validate_registration(Name, Email, Password) ->
    %% Validate registration data
    case Name of
        undefined -> {error, name_required};
        _ when length(Name) < 3 -> {error, name_too_short};
        _ -> ok
    end,

    case Email of
        undefined -> {error, email_required};
        _ -> case validate_email(Email) of
                true -> ok;
                false -> {error, invalid_email}
            end
    end,

    case Password of
        undefined -> {error, password_required};
        _ when length(Password) < 8 -> {error, password_too_short};
        _ -> ok
    end.

validate_email(Email) ->
    %% Simple email validation
    case re:run(Email, "^[^@]+@[^@]+\\.[^@]+$") of
        nomatch -> false;
        _ -> true
    end.

create_session(User) ->
    %% Create user session
    UserId = User#user.id,
    SessionId = generate_session_id(),
    Session = #{
        id => SessionId,
        user_id => UserId,
        created => erlang:system_time(second),
        expires => erlang:system_time(second) + 86400 % 24 hours
    },

    %% Store session
    erlmcp_dev_portal_sessions:create(Session),

    SessionId.

generate_session_id() ->
    %% Generate unique session ID
    uuid:to_string(uuid:new()).

set_session_cookie(Req, SessionId) ->
    %% Set session cookie
    Cookie = list_to_binary(["session=", SessionId, "; Path=/; HttpOnly; SameSite=Lax"]),
    cowboy_req:set_resp_cookie(<<"session">>, SessionId, #{}, Req).

handle_logout(Req) ->
    %% Handle logout
    case cowboy_req:cookie(<<"session">>, Req) of
        undefined -> Req;
        SessionId ->
            %% Remove session
            erlmcp_dev_portal_sessions:delete(SessionId),
            %% Clear cookie
            cowboy_req:set_resp_cookie(<<"session">>, <<>>, #{max_age => 0}, Req)
    end.