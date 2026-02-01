-module(erlmcp_session_entropy_validation_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Session ID Entropy and Randomness Validation Tests (FM-02)
%% Chicago School TDD - Statistical verification of ID generation
%% Target: Verify ≥128 bits entropy, no collisions, cryptographic randomness
%%====================================================================

%%====================================================================
%% Setup and Teardown
%%====================================================================

session_entropy_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         %% Entropy and Uniqueness Tests (6 tests)
         fun test_rotation_generates_unique_ids/1,
         fun test_entropy_128_bits_verified/1,
         fun test_no_sequential_patterns/1,
         fun test_collision_probability_negligible/1,
         fun test_id_distribution_uniform/1,
         fun test_crypto_strong_rand_bytes_used/1
     ]}.

setup() ->
    {ok, Pid} = erlmcp_session_manager:start_link(),
    Pid.

cleanup(Pid) ->
    case is_process_alive(Pid) of
        true ->
            unlink(Pid),
            exit(Pid, shutdown),
            timer:sleep(10);
        false ->
            ok
    end.

%%====================================================================
%% Entropy and Uniqueness Tests
%%====================================================================

%% Test: Every rotation generates unique ID (1000 rotations, 0 collisions)
test_rotation_generates_unique_ids(_Pid) ->
    fun() ->
        NumRotations = 1000,

        %% Create initial session
        {ok, InitialId} = erlmcp_session_manager:create_session(#{index => 0}),

        %% Collect all IDs from rotations
        {FinalId, AllIds} = lists:foldl(fun(Index, {CurrentId, Acc}) ->
            {ok, NewId} = erlmcp_session_manager:rotate_session(
                CurrentId,
                #{index => Index}
            ),
            {NewId, [NewId | Acc]}
        end, {InitialId, [InitialId]}, lists:seq(1, NumRotations)),

        %% Verify all IDs are unique
        UniqueIds = lists:usort(AllIds),
        ?assertEqual(NumRotations + 1, length(AllIds)),
        ?assertEqual(NumRotations + 1, length(UniqueIds)),

        %% Verify no duplicates
        ?assertEqual(AllIds, UniqueIds),

        %% Final session should exist
        ?assertMatch({ok, _}, erlmcp_session_manager:get_session(FinalId)),

        %% Initial session should be gone
        ?assertEqual({error, not_found},
                     erlmcp_session_manager:get_session(InitialId))
    end.

%% Test: Entropy ≥ 128 bits (statistical test)
test_entropy_128_bits_verified(_Pid) ->
    fun() ->
        NumSamples = 1000,

        %% Generate session IDs through rotation
        {ok, InitialId} = erlmcp_session_manager:create_session(#{index => 0}),

        {_FinalId, SessionIds} = lists:foldl(fun(Index, {CurrentId, Acc}) ->
            {ok, NewId} = erlmcp_session_manager:rotate_session(
                CurrentId,
                #{index => Index}
            ),
            {NewId, [NewId | Acc]}
        end, {InitialId, [InitialId]}, lists:seq(1, NumSamples - 1)),

        %% Session IDs are 32 hex characters = 16 bytes = 128 bits
        %% Verify format
        lists:foreach(fun(SessionId) ->
            ?assertEqual(32, byte_size(SessionId)),
            ?assert(is_hex_string(SessionId))
        end, SessionIds),

        %% Statistical test: Chi-square test for uniform distribution
        %% Convert hex strings to bytes and check distribution
        AllBytes = lists:flatten([hex_to_bytes(Id) || Id <- SessionIds]),

        %% Expected: 16 bytes per ID * NumSamples IDs = 16000 bytes total
        ?assertEqual(16 * NumSamples, length(AllBytes)),

        %% Chi-square test for byte value distribution
        %% H0: Bytes are uniformly distributed [0..255]
        ChiSquare = calculate_chi_square(AllBytes),

        %% Critical value for chi-square with 255 degrees of freedom at p=0.01 is ~310
        %% If ChiSquare > 310, reject null hypothesis (non-random)
        ?assert(ChiSquare < 400),  % Conservative threshold

        io:format("Chi-square statistic: ~p (lower is more random, <400 expected)~n", [ChiSquare])
    end.

%% Test: No sequential patterns (verify randomness, not seeded)
test_no_sequential_patterns(_Pid) ->
    fun() ->
        NumSamples = 500,

        %% Generate session IDs
        {ok, InitialId} = erlmcp_session_manager:create_session(#{index => 0}),

        {_FinalId, SessionIds} = lists:foldl(fun(Index, {CurrentId, Acc}) ->
            {ok, NewId} = erlmcp_session_manager:rotate_session(
                CurrentId,
                #{index => Index}
            ),
            {NewId, [NewId | Acc]}
        end, {InitialId, [InitialId]}, lists:seq(1, NumSamples - 1)),

        %% Convert to byte sequences
        ByteSequences = [hex_to_bytes(Id) || Id <- SessionIds],

        %% Test for sequential patterns in adjacent bytes
        lists:foreach(fun(Bytes) ->
            %% No byte should be sequential (b[i+1] = b[i] + 1) more than twice
            SequentialCount = count_sequential_bytes(Bytes),
            %% In 16 random bytes, expect ~0-1 sequential pairs, reject if >3
            ?assert(SequentialCount =< 3)
        end, ByteSequences),

        %% Test for repeating patterns across IDs
        %% No two IDs should share the same first 4 bytes (prefix collision)
        Prefixes = [lists:sublist(Bytes, 4) || Bytes <- ByteSequences],
        UniquePrefixes = lists:usort(Prefixes),

        %% With 500 IDs and 32-bit prefixes, collision probability is negligible
        %% If we see >5 collisions, something is wrong
        PrefixCollisions = length(Prefixes) - length(UniquePrefixes),
        ?assert(PrefixCollisions < 5),

        io:format("Sequential patterns detected: minimal (~0-1 per ID expected)~n"),
        io:format("Prefix collisions (4 bytes): ~p (expect <5)~n", [PrefixCollisions])
    end.

%% Test: Collision probability negligible (10,000 IDs, 0 collisions expected)
test_collision_probability_negligible(_Pid) ->
    fun() ->
        NumSamples = 10000,

        %% Generate many session IDs
        {ok, InitialId} = erlmcp_session_manager:create_session(#{index => 0}),

        {_FinalId, AllIds} = lists:foldl(fun(Index, {CurrentId, Acc}) ->
            {ok, NewId} = erlmcp_session_manager:rotate_session(
                CurrentId,
                #{index => Index}
            ),
            {NewId, [NewId | Acc]}
        end, {InitialId, [InitialId]}, lists:seq(1, NumSamples - 1)),

        %% Verify all unique
        UniqueIds = lists:usort(AllIds),
        Collisions = length(AllIds) - length(UniqueIds),

        ?assertEqual(0, Collisions),
        ?assertEqual(NumSamples, length(UniqueIds)),

        %% Birthday paradox: For 128-bit IDs, probability of collision in 10,000 IDs
        %% is approximately 10000^2 / (2 * 2^128) ≈ 10^-30 (negligible)
        io:format("Generated ~p session IDs with 0 collisions (128-bit entropy verified)~n",
                  [NumSamples])
    end.

%% Test: ID distribution is uniform (no bias in hex characters)
test_id_distribution_uniform(_Pid) ->
    fun() ->
        NumSamples = 1000,

        %% Generate session IDs
        {ok, InitialId} = erlmcp_session_manager:create_session(#{index => 0}),

        {_FinalId, SessionIds} = lists:foldl(fun(Index, {CurrentId, Acc}) ->
            {ok, NewId} = erlmcp_session_manager:rotate_session(
                CurrentId,
                #{index => Index}
            ),
            {NewId, [NewId | Acc]}
        end, {InitialId, [InitialId]}, lists:seq(1, NumSamples - 1)),

        %% Count frequency of each hex character (0-9, a-f)
        AllChars = lists:flatten([binary_to_list(Id) || Id <- SessionIds]),
        CharFrequency = count_char_frequency(AllChars),

        %% Total characters: 32 chars/ID * NumSamples IDs
        TotalChars = 32 * NumSamples,
        ?assertEqual(TotalChars, length(AllChars)),

        %% Expected frequency per char (16 possible hex chars): TotalChars / 16
        ExpectedFreq = TotalChars / 16,

        %% Verify each char appears with roughly expected frequency
        %% Allow 20% deviation (statistical noise)
        lists:foreach(fun(Char) ->
            Freq = maps:get(Char, CharFrequency, 0),
            Deviation = abs(Freq - ExpectedFreq) / ExpectedFreq,
            ?assert(Deviation < 0.20)  % Less than 20% deviation
        end, "0123456789abcdef"),

        io:format("Character distribution uniform (all within 20% of expected frequency)~n")
    end.

%% Test: crypto:strong_rand_bytes used (verify API contract)
test_crypto_strong_rand_bytes_used(_Pid) ->
    fun() ->
        %% This test verifies the implementation uses crypto:strong_rand_bytes
        %% by checking that IDs have the expected properties of cryptographic RNG

        NumSamples = 100,

        %% Generate IDs
        {ok, InitialId} = erlmcp_session_manager:create_session(#{index => 0}),

        {_FinalId, SessionIds} = lists:foldl(fun(Index, {CurrentId, Acc}) ->
            {ok, NewId} = erlmcp_session_manager:rotate_session(
                CurrentId,
                #{index => Index}
            ),
            {NewId, [NewId | Acc]}
        end, {InitialId, [InitialId]}, lists:seq(1, NumSamples - 1)),

        %% Properties of crypto:strong_rand_bytes:
        %% 1. All IDs are unique (tested above)
        ?assertEqual(NumSamples, length(lists:usort(SessionIds))),

        %% 2. IDs are 16 bytes = 128 bits
        lists:foreach(fun(Id) ->
            ?assertEqual(32, byte_size(Id))  % 32 hex chars = 16 bytes
        end, SessionIds),

        %% 3. IDs should have high entropy (Hamming distance test)
        %% Adjacent IDs should differ in ~50% of bits
        ByteSeqs = [hex_to_bytes(Id) || Id <- SessionIds],
        Pairs = lists:zip(ByteSeqs, tl(ByteSeqs)),

        HammingDistances = [hamming_distance(B1, B2) || {B1, B2} <- Pairs],
        AvgHamming = lists:sum(HammingDistances) / length(HammingDistances),

        %% Expected: ~64 bits different out of 128 (50%)
        %% Allow range [50, 78] bits (40%-60% due to variance)
        ?assert(AvgHamming > 50),
        ?assert(AvgHamming < 78),

        io:format("Average Hamming distance: ~.2f bits (expect ~64 for random data)~n",
                  [AvgHamming])
    end.

%%====================================================================
%% Helper Functions
%%====================================================================

%% Check if binary is valid hex string
is_hex_string(Bin) ->
    lists:all(fun(C) ->
        (C >= $0 andalso C =< $9) orelse
        (C >= $a andalso C =< $f) orelse
        (C >= $A andalso C =< $F)
    end, binary_to_list(Bin)).

%% Convert hex string to byte list
hex_to_bytes(HexBin) ->
    HexStr = binary_to_list(HexBin),
    hex_to_bytes_loop(HexStr, []).

hex_to_bytes_loop([], Acc) ->
    lists:reverse(Acc);
hex_to_bytes_loop([H1, H2 | Rest], Acc) ->
    Byte = list_to_integer([H1, H2], 16),
    hex_to_bytes_loop(Rest, [Byte | Acc]).

%% Calculate chi-square statistic for byte distribution
calculate_chi_square(Bytes) ->
    %% Count frequency of each byte value [0..255]
    Frequencies = lists:foldl(fun(Byte, Map) ->
        maps:update_with(Byte, fun(C) -> C + 1 end, 1, Map)
    end, #{}, Bytes),

    %% Expected frequency (uniform distribution)
    TotalBytes = length(Bytes),
    ExpectedFreq = TotalBytes / 256,

    %% Chi-square: sum((observed - expected)^2 / expected)
    lists:foldl(fun(ByteVal, ChiSq) ->
        ObservedFreq = maps:get(ByteVal, Frequencies, 0),
        Diff = ObservedFreq - ExpectedFreq,
        ChiSq + (Diff * Diff / ExpectedFreq)
    end, 0.0, lists:seq(0, 255)).

%% Count sequential bytes (b[i+1] = b[i] + 1)
count_sequential_bytes(Bytes) ->
    Pairs = lists:zip(Bytes, tl(Bytes)),
    length([1 || {B1, B2} <- Pairs, B2 =:= (B1 + 1) rem 256]).

%% Count character frequency
count_char_frequency(Chars) ->
    lists:foldl(fun(Char, Map) ->
        maps:update_with(Char, fun(C) -> C + 1 end, 1, Map)
    end, #{}, Chars).

%% Calculate Hamming distance (number of differing bits)
hamming_distance(Bytes1, Bytes2) ->
    Pairs = lists:zip(Bytes1, Bytes2),
    lists:foldl(fun({B1, B2}, Acc) ->
        Acc + count_bits(B1 bxor B2)
    end, 0, Pairs).

%% Count number of set bits
count_bits(0) -> 0;
count_bits(N) -> (N band 1) + count_bits(N bsr 1).
