-module(erlmcp_mfa_provider).
-export([start_link/0, generate_code/1, verify_code/2]).
-export([setup_mfa/2, disable_mfa/2]).
-export([get_mfa_status/1, list_mfa_methods/1]).
-export([send_totp/1, send_sms/2, send_email/2]).
-export([backup_codes/1, verify_backup_code/2]).

-record.mfa_device, {
    id :: binary(),
    user_id :: binary(),
    type :: totp | sms | email | backup,
    secret :: binary(),
    enabled :: boolean(),
    created_at :: integer(),
    last_used :: integer() | undefined,
    backup_codes :: list()
}.

-record.state, {
    devices :: map(),
    config :: map()
}.

-define(TIMEOUT, 30000).
-define(TOTP_ISSUER, "erlmcp").
-define(TOTP_DIGITS, 6).
-define(TOTP_INTERVAL, 30).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

generate_code(UserId) ->
    gen_server:call(?MODULE, {generate_code, UserId}, ?TIMEOUT).

verify_code(UserId, Code) ->
    gen_server:call(?MODULE, {verify_code, UserId, Code}, ?TIMEOUT).

setup_mfa(UserId, MfaType) ->
    gen_server:call(?MODULE, {setup_mfa, UserId, MfaType}, ?TIMEOUT).

disable_mfa(UserId, Reason) ->
    gen_server:call(?MODULE, {disable_mfa, UserId, Reason}, ?TIMEOUT).

get_mfa_status(UserId) ->
    gen_server:call(?MODULE, {get_mfa_status, UserId}, ?TIMEOUT).

list_mfa_methods(UserId) ->
    gen_server:call(?MODULE, {list_mfa_methods, UserId}, ?TIMEOUT).

send_totp(UserId) ->
    gen_server:call(?MODULE, {send_totp, UserId}, ?TIMEOUT).

send_sms(UserId, PhoneNumber) ->
    gen_server:call(?MODULE, {send_sms, UserId, PhoneNumber}, ?TIMEOUT).

send_email(UserId, EmailAddress) ->
    gen_server:call(?MODULE, {send_email, UserId, EmailAddress}, ?TIMEOUT).

backup_codes(UserId) ->
    gen_server:call(?MODULE, {backup_codes, UserId}, ?TIMEOUT).

verify_backup_code(UserId, Code) ->
    gen_server:call(?MODULE, {verify_backup_code, UserId, Code}, ?TIMEOUT).

init([]) ->
    State = #state{
        devices = load_mfa_devices(),
        config = load_config()
    },
    erlmcp_mfa_provider:initialize(),
    {ok, State}.

handle_call({generate_code, UserId}, _From, State) ->
    case generate_totp_code(UserId, State) of
        {ok, Code} ->
            {reply, {ok, Code}, State};
        {error, not_found} ->
            {reply, {error, not_found}, State}
    end;

handle_call({verify_code, UserId, Code}, _From, State) ->
    case verify_totp_code(UserId, Code, State) of
        {ok, verified} ->
            {reply, {ok, verified}, State};
        {error, not_verified} ->
            {reply, {error, not_verified}, State}
    end;

handle_call({setup_mfa, UserId, MfaType}, _From, State) ->
    case setup_mfa_device(UserId, MfaType, State) of
        {ok, DeviceId} ->
            {reply, {ok, DeviceId}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({disable_mfa, UserId, Reason}, _From, State) ->
    case disable_mfa_device(UserId, Reason, State) of
        {ok, disabled} ->
            {reply, {ok, disabled}, State};
        {error, not_found} ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_mfa_status, UserId}, _From, State) ->
    case get_user_mfa_devices(UserId, State) of
        {ok, Devices} ->
            Status = calculate_mfa_status(Devices),
            {reply, {ok, Status}, State};
        {error, not_found} ->
            {reply, {error, not_found}, State}
    end;

handle_call({list_mfa_methods, UserId}, _From, State) ->
    case get_user_mfa_devices(UserId, State) of
        {ok, Devices} ->
            Methods = lists:map(fun(Device) ->
                #{type => Device#mfa_device.type, enabled => Device#mfa_device.enabled}
            end, Devices),
            {reply, {ok, Methods}, State};
        {error, not_found} ->
            {reply, {error, not_found}, State}
    end;

handle_call({send_totp, UserId}, _From, State) ->
    case send_totp_code(UserId, State) of
        {ok, sent} ->
            {reply, {ok, sent}, State};
        {error, not_found} ->
            {reply, {error, not_found}, State}
    end;

handle_call({send_sms, UserId, PhoneNumber}, _From, State) ->
    case send_sms_code(UserId, PhoneNumber, State) of
        {ok, sent} ->
            {reply, {ok, sent}, State};
        {error, not_found} ->
            {reply, {error, not_found}, State}
    end;

handle_call({send_email, UserId, EmailAddress}, _From, State) ->
    case send_email_code(UserId, EmailAddress, State) of
        {ok, sent} ->
            {reply, {ok, sent}, State};
        {error, not_found} ->
            {reply, {error, not_found}, State}
    end;

handle_call({backup_codes, UserId}, _From, State) ->
    case generate_backup_codes(UserId, State) of
        {ok, BackupCodes} ->
            {reply, {ok, BackupCodes}, State};
        {error, not_found} ->
            {reply, {error, not_found}, State}
    end;

handle_call({verify_backup_code, UserId, Code}, _From, State) ->
    case verify_backup_code_1(UserId, Code, State) of
        {ok, verified} ->
            {reply, {ok, verified}, State};
        {error, not_verified} ->
            {reply, {error, not_verified}, State}
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

%% Internal functions
initialize() ->
    %% Initialize MFA provider
    %% Load MFA configuration
    ok.

load_config() ->
    #{
        totp_interval => ?TOTP_INTERVAL,
        totp_digits => ?TOTP_DIGITS,
        issuer => ?TOTP_ISSUER,
        backup_code_count => 10,
        code_expiry => 300000 %% 5 minutes
    }.

load_mfa_devices() ->
    %% Load MFA devices from storage
    #{}.

generate_totp_code(UserId, State) ->
    case get_user_mfa_devices(UserId, State) of
        {ok, [Device|_]} when Device#mfa_device.type == totp, Device#mfa_device.enabled ->
            Secret = Device#mfa_device.secret,
            TOTP = authenticator:totp(Secret, ?TOTP_DIGITS, ?TOTP_INTERVAL),
            {ok, TOTP};
        _ ->
            {error, not_found}
    end.

verify_totp_code(UserId, Code, State) ->
    case get_user_mfa_devices(UserId, State) of
        {ok, [Device|_]} when Device#mfa_device.type == totp, Device#mfa_device.enabled ->
            Secret = Device#mfa_device.secret,
            case authenticator:verify_totp(Secret, Code, ?TOTP_DIGITS, ?TOTP_INTERVAL) of
                true ->
                    %% Update last used time
                    UpdatedDevice = Device#mfa_device{last_used = timestamp()},
                    {ok, verified};
                false ->
                    {error, not_verified}
            end;
        _ ->
            {error, not_found}
    end.

setup_mfa_device(UserId, MfaType, State) ->
    case MfaType of
        totp ->
            Secret = generate_totp_secret(),
            Device = #mfa_device{
                id = generate_device_id(),
                user_id = UserId,
                type = totp,
                secret = Secret,
                enabled = true,
                created_at = timestamp(),
                last_used = undefined,
                backup_codes = []
            },
            NewState = State#state{
                devices = maps:put(Device#mfa_device.id, Device, State#state.devices)
            },
            {ok, Device#mfa_device.id};
        sms ->
            %% SMS setup requires phone number
            PhoneNumber = get_user_phone(UserId),
            case PhoneNumber of
                undefined ->
                    {error, phone_number_required};
                _ ->
                    Device = #mfa_device{
                        id = generate_device_id(),
                        user_id = UserId,
                        type = sms,
                        secret = PhoneNumber,
                        enabled = true,
                        created_at = timestamp(),
                        last_used = undefined,
                        backup_codes = []
                    },
                    NewState = State#state{
                        devices = maps:put(Device#mfa_device.id, Device, State#state.devices)
                    },
                    {ok, Device#mfa_device.id}
            end;
        email ->
            %% Email setup requires email address
            EmailAddress = get_user_email(UserId),
            case EmailAddress of
                undefined ->
                    {error, email_required};
                _ ->
                    Device = #mfa_device{
                        id = generate_device_id(),
                        user_id = UserId,
                        type = email,
                        secret = EmailAddress,
                        enabled = true,
                        created_at = timestamp(),
                        last_used = undefined,
                        backup_codes = []
                    },
                    NewState = State#state{
                        devices = maps:put(Device#mfa_device.id, Device, State#state.devices)
                    },
                    {ok, Device#mfa_device.id}
            end;
        _ ->
            {error, unsupported_mfa_type}
    end.

disable_mfa_device(UserId, Reason, State) ->
    case maps:filter(fun(_, Device) -> Device#mfa_device.user_id == UserId end, State#state.devices) of
        Devices when map_size(Devices) > 0 ->
            %% Disable all MFA devices for user
            DisabledDevices = maps:map(fun(_, Device) ->
                Device#mfa_device{enabled = false}
            end, Devices),
            NewState = State#state{
                devices = maps:merge(State#state.devices, DisabledDevices)
            },
            {ok, disabled};
        _ ->
            {error, not_found}
    end.

get_user_mfa_devices(UserId, State) ->
    UserDevices = maps:filter(fun(_, Device) -> Device#mfa_device.user_id == UserId end, State#state.devices),
    case map_size(UserDevices) of
        0 ->
            {error, not_found};
        _ ->
            {ok, maps:values(UserDevices)}
    end.

calculate_mfa_status(Devices) ->
    EnabledDevices = lists:filter(fun(D) -> D#mfa_device.enabled end, Devices),
    case length(EnabledDevices) of
        0 ->
            disabled;
        1 ->
            enabled_single_factor;
        _ ->
            enabled_multi_factor
    end.

send_totp_code(UserId, State) ->
    case generate_totp_code(UserId, State) of
        {ok, Code} ->
            %% Store code for verification
            store_verification_code(UserId, Code),
            {ok, sent};
        {error, not_found} ->
            {error, not_found}
    end.

send_sms_code(UserId, PhoneNumber, State) ->
    %% Generate SMS code
    Code = generate_sms_code(),
    %% Store code for verification
    store_verification_code(UserId, Code),
    %% Send SMS via external service
    erlmcp_notification_service:send_sms(PhoneNumber, Code),
    {ok, sent}.

send_email_code(UserId, EmailAddress, State) ->
    %% Generate email code
    Code = generate_email_code(),
    %% Store code for verification
    store_verification_code(UserId, Code),
    %% Send email via external service
    erlmcp_notification_service:send_email(EmailAddress, "MFA Code", Code),
    {ok, sent}.

generate_backup_codes(UserId, State) ->
    case get_user_mfa_devices(UserId, State) of
        {ok, Devices} ->
            %% Generate backup codes
            BackupCodes = [generate_backup_code() || _ <- lists:seq(1, ?TOTP_DIGITS)],
            %% Update device with backup codes
            UpdatedDevices = lists:map(fun(Device) ->
                Device#mfa_device{backup_codes = BackupCodes}
            end, Devices),
            NewState = State#state{
                devices = maps:merge(State#state.devices, lists:foldl(fun(D, Acc) -> maps:put(D#mfa_device.id, D, Acc) end, #{}, UpdatedDevices))
            },
            {ok, BackupCodes};
        {error, not_found} ->
            {error, not_found}
    end.

verify_backup_code_1(UserId, Code, State) ->
    case get_user_mfa_devices(UserId, State) of
        {ok, [Device|_]} when Device#mfa_device.type == backup, Device#mfa_device.enabled ->
            case lists:member(Code, Device#mfa_device.backup_codes) of
                true ->
                    %% Remove used backup code
                    RemainingCodes = lists:delete(Code, Device#mfa_device.backup_codes),
                    UpdatedDevice = Device#mfa_device{backup_codes = RemainingCodes},
                    NewState = State#state{
                        devices = maps:put(Device#mfa_device.id, UpdatedDevice, State#state.devices)
                    },
                    {ok, verified};
                false ->
                    {error, not_verified}
            end;
        _ ->
            {error, not_found}
    end.

generate_totp_secret() ->
    Base32 = binary_to_list(crypto:strong_rand_bytes(16)),
    authenticator:generate_secret(Base32).

generate_sms_code() ->
    integer_to_list(random:uniform(900000) + 99999).

generate_email_code() ->
    integer_to_list(random:uniform(900000) + 99999).

generate_backup_code() ->
    integer_to_list(random:uniform(900000000) + 99999999).

generate_device_id() ->
    crypto:strong_rand_bytes(16).

get_user_phone(UserId) ->
    %% Get user phone number from identity provider
    case erlmcp_identity_provider:get_user_info(UserId) of
        {ok, UserInfo} ->
            maps:get(phone, UserInfo, undefined);
        {error, _} ->
            undefined
    end.

get_user_email(UserId) ->
    %% Get user email from identity provider
    case erlmcp_identity_provider:get_user_info(UserId) of
        {ok, UserInfo} ->
            maps:get(email, UserInfo, undefined);
        {error, _} ->
            undefined
    end.

store_verification_code(UserId, Code) ->
    %% Store verification code in temporary storage
    erlmcp_verification_cache:store(UserId, Code, ?TOTP_INTERVAL * 1000).

timestamp() ->
    erlang:system_time(millisecond).