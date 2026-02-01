%%%====================================================================
%%% @doc Comprehensive Test Suite for erlmcp_mermaid_renderer
%%%
%%% Chicago School TDD: Real renderer processes, output verification
%%%
%%% Tests:
%%% - SVG generation from AST
%%% - PNG generation from AST
%%% - Responsive SVG sizing
%%% - Color scheme application
%%% - Theme customization
%%% - Font rendering
%%% - Edge label positioning
%%% - Subgraph rendering
%%% - Animation support
%%% - Accessibility attributes
%%% - Error handling for invalid AST
%%% - Performance benchmarks
%%%
%%% Target: 85%+ coverage (Core module)
%%% @end
%%%====================================================================
-module(erlmcp_mermaid_renderer_tests).
-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Test Generators
%%%====================================================================

mermaid_renderer_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"SVG Generation", {spawn, fun svg_generation_tests/0}},
          {"PNG Generation", {spawn, fun png_generation_tests/0}},
          {"Responsive Sizing", {spawn, fun responsive_tests/0}},
          {"Color Schemes", {spawn, fun color_scheme_tests/0}},
          {"Theme Customization", {spawn, fun theme_tests/0}},
          {"Font Rendering", {spawn, fun font_tests/0}},
          {"Edge Labels", {spawn, fun edge_label_tests/0}},
          {"Subgraph Rendering", {spawn, fun subgraph_tests/0}},
          {"Animation Support", {spawn, fun animation_tests/0}},
          {"Accessibility", {spawn, fun accessibility_tests/0}},
          {"Error Handling", {spawn, fun error_handling_tests/0}},
          {"Performance", {spawn, fun performance_tests/0}}
         ]
     end}.

%%%====================================================================
%%% Setup and Cleanup
%%%====================================================================

setup() ->
    application:ensure_all_started(erlmcp_core),
    ok.

cleanup(_) ->
    ok.

%%%====================================================================
%%% SVG Generation Tests
%%%====================================================================

svg_generation_tests() ->
    %% Simple flowchart SVG
    SimpleFlowAst = #{
        <<"type">> => <<"flowchart">>,
        <<"direction">> => <<"TD">>,
        <<"nodes">> => [
            #{<<"id">> => <<"A">>, <<"shape">> => <<"rectangle">>, <<"label">> => <<"Start">>},
            #{<<"id">> => <<"B">>, <<"shape">> => <<"rectangle">>, <<"label">> => <<"End">>}
        ],
        <<"edges">> => [
            #{<<"from">> => <<"A">>, <<"to">> => <<"B">>, <<"label">> => <<>>}
        ]
    },
    {ok, Svg1} = erlmcp_mermaid_renderer:render_svg(SimpleFlowAst),
    ?assert(is_binary(Svg1)),
    ?assert(<<">= 0, binary:match(Svg1, <<"<svg">>)),
    ?assert(<<">= 0, binary:match(Svg1, <<"xmlns">>)),
    ?assert(<<">= 0, binary:match(Svg1, <<"version">>)),

    %% Verify SVG contains node elements
    ?assert(<<">= 0, binary:match(Svg1, <<"Start">>)),
    ?assert(<<">= 0, binary:match(Svg1, <<"End">>)),

    %% Sequence diagram SVG
    SeqAst = #{
        <<"type">> => <<"sequenceDiagram">>,
        <<"participants">> => [
            #{<<"id">> => <<"Alice">>},
            #{<<"id">> => <<"Bob">>}
        ],
        <<"messages">> => [
            #{<<"from">> => <<"Alice">>, <<"to">> => <<"Bob">>, <<"label">> => <<"Hello">>}
        ]
    },
    {ok, SeqSvg} = erlmcp_mermaid_renderer:render_svg(SeqAst),
    ?assert(<<">= 0, binary:match(SeqSvg, <<"Alice">>)),
    ?assert(<<">= 0, binary:match(SeqSvg, <<"Bob">>)),
    ?assert(<<">= 0, binary:match(SeqSvg, <<"Hello">>)),

    %% State diagram SVG
    StateAst = #{
        <<"type">> => <<"stateDiagram-v2">>,
        <<"states">> => [
            #{<<"id">> => <<"Idle">>},
            #{<<"id">> => <<"Running">>}
        ],
        <<"transitions">> => [
            #{<<"from">> => <<"Idle">>, <<"to">> => <<"Running">>}
        ]
    },
    {ok, StateSvg} = erlmcp_mermaid_renderer:render_svg(StateAst),
    ?assert(<<">= 0, binary:match(StateSvg, <<"Idle">>)),
    ?assert(<<">= 0, binary:match(StateSvg, <<"Running">>)),

    ok.

%%%====================================================================
%%% PNG Generation Tests
%%%====================================================================

png_generation_tests() ->
    %% Simple flowchart PNG
    FlowAst = #{
        <<"type">> => <<"flowchart">>,
        <<"direction">> => <<"TD">>,
        <<"nodes">> => [
            #{<<"id">> => <<"A">>, <<"shape">> => <<"rectangle">>, <<"label">> => <<"Start">>},
            #{<<"id">> => <<"B">>, <<"shape">> => <<"rectangle">>, <<"label">> => <<"End">>}
        ],
        <<"edges">> => [
            #{<<"from">> => <<"A">>, <<"to">> => <<"B">>}
        ]
    },
    {ok, PngData} = erlmcp_mermaid_renderer:render_png(FlowAst, #{
        <<"width">> => 800,
        <<"height">> => 600
    }),
    ?assert(is_binary(PngData)),
    ?assert(byte_size(PngData) > 0),

    %% Verify PNG header
    <<137:8, $P, $N, $G, 13:8, 10:8, 26:8, 10:8, _Rest/binary>> = PngData,

    %% PNG with custom DPI
    {ok, PngHiRes} = erlmcp_mermaid_renderer:render_png(FlowAst, #{
        <<"width">> => 1600,
        <<"height">> => 1200,
        <<"dpi">> => 300
    }),
    ?assert(byte_size(PngHiRes) > byte_size(PngData)),

    ok.

%%%====================================================================
%%% Responsive Sizing Tests
%%%====================================================================

responsive_tests() ->
    %% Viewbox generation
    FlowAst = #{
        <<"type">> => <<"flowchart">>,
        <<"direction">> => <<"TD">>,
        <<"nodes">> => [
            #{<<"id">> => <<"A">>, <<"shape">> => <<"rectangle">>, <<"label">> => <<"Start">>},
            #{<<"id">> => <<"B">>, <<"shape">> => <<"rectangle">>, <<"label">> => <<"End">>}
        ],
        <<"edges">> => [
            #{<<"from">> => <<"A">>, <<"to">> => <<"B">>}
        ]
    },
    {ok, Svg1} = erlmcp_mermaid_renderer:render_svg(FlowAst, #{<<"responsive">> => true}),
    ?assert(<<">= 0, binary:match(Svg1, <<"viewBox">>)),
    ?assert(<<">= 0, binary:match(Svg1, <<"preserveAspectRatio">>)),

    %% Fixed size SVG
    {ok, Svg2} = erlmcp_mermaid_renderer:render_svg(FlowAst, #{
        <<"width">> => 800,
        <<"height">> => 600
    }),
    ?assert(<<">= 0, binary:match(Svg2, <<"width=\"800\"">>)),
    ?assert(<<">= 0, binary:match(Svg2, <<"height=\"600\"">>)),

    %% Percentage-based sizing
    {ok, Svg3} = erlmcp_mermaid_renderer:render_svg(FlowAst, #{
        <<"width">> => <<"100%">>,
        <<"height">> => <<"auto">>
    }),
    ?assert(<<">= 0, binary:match(Svg3, <<"width=\"100%\"">>)),
    ?assert(<<">= 0, binary:match(Svg3, <<"height=\"auto\"">>)),

    ok.

%%%====================================================================
%%% Color Scheme Tests
%%%====================================================================

color_scheme_tests() ->
    FlowAst = #{
        <<"type">> => <<"flowchart">>,
        <<"direction">> => <<"TD">>,
        <<"nodes">> => [
            #{<<"id">> => <<"A">>, <<"shape">> => <<"rectangle">>, <<"label">> => <<"Start">>},
            #{<<"id">> => <<"B">>, <<"shape">> => <<"rectangle">>, <<"label">> => <<"End">>}
        ],
        <<"edges">> => [
            #{<<"from">> => <<"A">>, <<"to">> => <<"B">>}
        ]
    },

    %% Default color scheme
    {ok, DefaultSvg} = erlmcp_mermaid_renderer:render_svg(FlowAst, #{
        <<"theme">> => <<"default">
    }),
    ?assert(<<">= 0, binary:match(DefaultSvg, <<"stroke:#333">>)),

    %% Dark theme
    {ok, DarkSvg} = erlmcp_mermaid_renderer:render_svg(FlowAst, #{
        <<"theme">> => <<"dark">
    }),
    ?assert(<<">= 0, binary:match(DarkSvg, <<"stroke:#fff">>)),

    %% Forest theme
    {ok, ForestSvg} = erlmcp_mermaid_renderer:render_svg(FlowAst, #{
        <<"theme">> => <<"forest">
    }),
    ?assert(<<">= 0, binary:match(ForestSvg, <<"stroke:#">>)),  % Has some color

    %% Custom colors
    {ok, CustomSvg} = erlmcp_mermaid_renderer:render_svg(FlowAst, #{
        <<"colors">> => #{
            <<"primary">> => <<"#FF5733">>,
            <<"secondary">> => <<"#33FF57">>,
            <<"background">> => <<"#3357FF">>
        }
    }),
    ?assert(<<">= 0, binary:match(CustomSvg, <<"#FF5733">>)),

    ok.

%%%====================================================================
%%% Theme Customization Tests
%%%====================================================================

theme_tests() ->
    FlowAst = #{
        <<"type">> => <<"flowchart">>,
        <<"direction">> => <<"TD">>,
        <<"nodes">> => [
            #{<<"id">> => <<"A">>, <<"shape">> => <<"rectangle">>, <<"label">> => <<"Start">>}
        ],
        <<"edges">> => []
    },

    %% Base theme
    {ok, BaseSvg} = erlmcp_mermaid_renderer:render_svg(FlowAst, #{
        <<"theme_variables">> => #{
            <<"primaryColor">> => <<"#ff0000">>,
            <<"primaryTextColor">> => <<"#ffffff">>,
            <<"primaryBorderColor">> => <<"#000000">>,
            <<"lineColor">> => <<"#333333">>,
            <<"secondaryColor">> => <<"#00ff00">>,
            <<"tertiaryColor">> => <<"#0000ff">>
        }
    }),
    ?assert(<<">= 0, binary:match(BaseSvg, <<"#ff0000">>)),

    %% Font family customization
    {ok, FontSvg} = erlmcp_mermaid_renderer:render_svg(FlowAst, #{
        <<"font_family">> => <<"Arial, sans-serif">>
    }),
    ?assert(<<">= 0, binary:match(FontSvg, <<"font-family">>)),

    %% Border radius customization
    {ok, RadiusSvg} = erlmcp_mermaid_renderer:render_svg(FlowAst, #{
        <<"border_radius">> => 10
    }),
    ?assert(<<">= 0, binary:match(RadiusSvg, <<"rx">>)),
    ?assert(<<">= 0, binary:match(RadiusSvg, <<"ry">>)),

    ok.

%%%====================================================================
%%% Font Rendering Tests
%%%====================================================================

font_tests() ->
    FlowAst = #{
        <<"type">> => <<"flowchart">>,
        <<"direction">> => <<"TD">>,
        <<"nodes">> => [
            #{<<"id">> => <<"A">>, <<"shape">> => <<"rectangle">>, <<"label">> => <<"Test">>}
        ],
        <<"edges">> => []
    },

    %% Font size
    {ok, SizeSvg} = erlmcp_mermaid_renderer:render_svg(FlowAst, #{
        <<"font_size">> => 18
    }),
    ?assert(<<">= 0, binary:match(SizeSvg, <<"font-size">>)),
    ?assert(<<">= 0, binary:match(SizeSvg, <<"18">>)),

    %% Font weight
    {ok, WeightSvg} = erlmcp_mermaid_renderer:render_svg(FlowAst, #{
        <<"font_weight">> => <<"bold">>
    }),
    ?assert(<<">= 0, binary:match(WeightSvg, <<"font-weight">>)),
    ?assert(<<">= 0, binary:match(WeightSvg, <<"bold">>)),

    %% Font style
    {ok, StyleSvg} = erlmcp_mermaid_renderer:render_svg(FlowAst, #{
        <<"font_style">> => <<"italic">>
    }),
    ?assert(<<">= 0, binary:match(StyleSvg, <<"font-style">>)),
    ?assert(<<">= 0, binary:match(StyleSvg, <<"italic">>)),

    %% Multiline text
    MultilineAst = FlowAst#{
        <<"nodes">> => [
            #{<<"id">> => <<"A">>, <<"shape">> => <<"rectangle">>, <<"label">> => <<"Line 1\nLine 2\nLine 3">>}
        ]
    },
    {ok, MultiSvg} = erlmcp_mermaid_renderer:render_svg(MultilineAst),
    ?assert(<<">= 0, binary:match(MultiSvg, <<"Line 1">>)),
    ?assert(<<">= 0, binary:match(MultiSvg, <<"Line 2">>)),
    ?assert(<<">= 0, binary:match(MultiSvg, <<"Line 3">>)),

    ok.

%%%====================================================================
%%% Edge Label Tests
%%%====================================================================

edge_label_tests() ->
    FlowAst = #{
        <<"type">> => <<"flowchart">>,
        <<"direction">> => <<"TD">>,
        <<"nodes">> => [
            #{<<"id">> => <<"A">>, <<"shape">> => <<"rectangle">>, <<"label">> => <<"Start">>},
            #{<<"id">> => <<"B">>, <<"shape">> => <<"rectangle">>, <<"label">> => <<"End">>}
        ],
        <<"edges">> => [
            #{<<"from">> => <<"A">>, <<"to">> => <<"B">>, <<"label">> => <<"Yes">>}
        ]
    },

    {ok, Svg} = erlmcp_mermaid_renderer:render_svg(FlowAst),
    ?assert(<<">= 0, binary:match(Svg, <<"Yes">>)),

    %% Edge with label on both sides
    BidirectionalAst = FlowAst#{
        <<"edges">> => [
            #{<<"from">> => <<"A">>, <<"to">> => <<"B">>, <<"label_start">> => <<"Request">>, <<"label_end">> => <<"Response">>}
        ]
    },
    {ok, BiSvg} = erlmcp_mermaid_renderer:render_svg(BidirectionalAst),
    ?assert(<<">= 0, binary:match(BiSvg, <<"Request">>)),
    ?assert(<<">= 0, binary:match(BiSvg, <<"Response">>)),

    ok.

%%%====================================================================
%%% Subgraph Rendering Tests
%%%====================================================================

subgraph_tests() ->
    FlowAst = #{
        <<"type">> => <<"flowchart">>,
        <<"direction">> => <<"TD">>,
        <<"nodes">> => [
            #{<<"id">> => <<"A">>, <<"shape">> => <<"rectangle">>, <<"label">> => <<"Start">>},
            #{<<"id">> => <<"B">>, <<"shape">> => <<"rectangle">>, <<"label">> => <<"Process">>},
            #{<<"id">> => <<"C">>, <<"shape">> => <<"rectangle">>, <<"label">> => <<"End">>}
        ],
        <<"edges">> => [
            #{<<"from">> => <<"A">>, <<"to">> => <<"B">>},
            #{<<"from">> => <<"B">>, <<"to">> => <<"C">>}
        ],
        <<"subgraphs">> => [
            #{
                <<"id">> => <<"Sub1">>,
                <<"label">> => <<"Group 1">>,
                <<"nodes">> => [<<"B">>]
            }
        ]
    },

    {ok, Svg} = erlmcp_mermaid_renderer:render_svg(FlowAst),
    ?assert(<<">= 0, binary:match(Svg, <<"Group 1">>)),

    %% Nested subgraphs
    NestedAst = FlowAst#{
        <<"subgraphs">> => [
            #{
                <<"id">> => <<"Outer">>,
                <<"label">> => <<"Outer">>,
                <<"nodes">> => [<<"A">>, <<"B">>],
                <<"subgraphs">> => [
                    #{
                        <<"id">> => <<"Inner">>,
                        <<"label">> => <<"Inner">>,
                        <<"nodes">> => [<<"B">>]
                    }
                ]
            }
        ]
    },
    {ok, NestedSvg} = erlmcp_mermaid_renderer:render_svg(NestedAst),
    ?assert(<<">= 0, binary:match(NestedSvg, <<"Outer">>)),
    ?assert(<<">= 0, binary:match(NestedSvg, <<"Inner">>)),

    ok.

%%%====================================================================
%%% Animation Tests
%%%====================================================================

animation_tests() ->
    FlowAst = #{
        <<"type">> => <<"flowchart">>,
        <<"direction">> => <<"TD">>,
        <<"nodes">> => [
            #{<<"id">> => <<"A">>, <<"shape">> => <<"rectangle">>, <<"label">> => <<"Start">>},
            #{<<"id">> => <<"B">>, <<"shape">> => <<"rectangle">>, <<"label">> => <<"End">>}
        ],
        <<"edges">> => [
            #{<<"from">> => <<"A">>, <<"to">> => <<"B">>}
        ]
    },

    %% Fade-in animation
    {ok, FadeSvg} = erlmcp_mermaid_renderer:render_svg(FlowAst, #{
        <<"animation">> => <<"fade-in">>,
        <<"duration">> => 500
    }),
    ?assert(<<">= 0, binary:match(FadeSvg, <<"animation">>)),
    ?assert(<<">= 0, binary:match(FadeSvg, <<"500ms">>)),

    %% Path animation
    {ok, PathSvg} = erlmcp_mermaid_renderer:render_svg(FlowAst, #{
        <<"animation">> => <<"path">>,
        <<"duration">> => 1000
    }),
    ?assert(<<">= 0, binary:match(PathSvg, <<"stroke-dasharray">>)),
    ?assert(<<">= 0, binary:match(PathSvg, <<"stroke-dashoffset">>)),

    ok.

%%%====================================================================
%%% Accessibility Tests
%%%====================================================================

accessibility_tests() ->
    FlowAst = #{
        <<"type">> => <<"flowchart">>,
        <<"direction">> => <<"TD">>,
        <<"nodes">> => [
            #{<<"id">> => <<"A">>, <<"shape">> => <<"rectangle">>, <<"label">> => <<"Start">>}
        ],
        <<"edges">> => []
    },

    %% Title and desc tags
    {ok, AccessSvg} = erlmcp_mermaid_renderer:render_svg(FlowAst, #{
        <<"accessibility">> => true,
        <<"title">> => <<"Flowchart showing process">>,
        <<"description">> => <<"A simple flowchart with one node">>
    }),
    ?assert(<<">= 0, binary:match(AccessSvg, <<"<title>">>)),
    ?assert(<<">= 0, binary:match(AccessSvg, <<"Flowchart showing process">>)),
    ?assert(<<">= 0, binary:match(AccessSvg, <<"<desc>">>)),
    ?assert(<<">= 0, binary:match(AccessSvg, <<"A simple flowchart">>)),

    %% ARIA labels
    {ok, AriaSvg} = erlmcp_mermaid_renderer:render_svg(FlowAst, #{
        <<"aria_labels">> => true
    }),
    ?assert(<<">= 0, binary:match(AriaSvg, <<"aria-label">>)),
    ?assert(<<">= 0, binary:match(AriaSvg, <<"role">>)),

    ok.

%%%====================================================================
%%% Error Handling Tests
%%%====================================================================

error_handling_tests() ->
    %% Invalid AST (missing type)
    InvalidAst1 = #{<<"nodes">> => []},
    ?assertMatch({error, {invalid_ast, _}}, erlmcp_mermaid_renderer:render_svg(InvalidAst1)),

    %% Invalid AST (missing nodes)
    InvalidAst2 = #{<<"type">> => <<"flowchart">>},
    ?assertMatch({error, {invalid_ast, _}}, erlmcp_mermaid_renderer:render_svg(InvalidAst2)),

    %% Invalid diagram type
    InvalidAst3 = #{
        <<"type">> => <<"unknown">>,
        <<"nodes">> => []
    },
    ?assertMatch({error, {unsupported_type, _}}, erlmcp_mermaid_renderer:render_svg(InvalidAst3)),

    %% Invalid render options
    ValidAst = #{
        <<"type">> => <<"flowchart">>,
        <<"nodes">> => [#{<<"id">> => <<"A">>, <<"shape">> => <<"rectangle">>, <<"label">> => <<"Test">>}],
        <<"edges">> => []
    },
    ?assertMatch({error, {invalid_options, _}}, erlmcp_mermaid_renderer:render_svg(ValidAst, #{
        <<"width">> => <<"invalid">>
    })),

    ok.

%%%====================================================================
%%% Performance Tests
%%%====================================================================

performance_tests() ->
    %% Large diagram rendering
    Nodes = [#{<<"id">> => list_to_binary(["node", integer_to_list(N)]),
               <<"shape">> => <<"rectangle">>,
               <<"label">> => list_to_binary(["Node ", integer_to_list(N)])}
              || N <- lists:seq(1, 100)],
    Edges = [#{<<"from">> => list_to_binary(["node", integer_to_list(N)]),
               <<"to">> => list_to_binary(["node", integer_to_list(N+1)])}
              || N <- lists:seq(1, 99)],

    LargeAst = #{
        <<"type">> => <<"flowchart">>,
        <<"direction">> => <<"TD">>,
        <<"nodes">> => Nodes,
        <<"edges">> => Edges
    },

    %% Measure render time
    Start = erlang:monotonic_time(microsecond),
    {ok, _Svg} = erlmcp_mermaid_renderer:render_svg(LargeAst),
    End = erlang:monotonic_time(microsecond),
    Duration = End - Start,

    %% Should render in reasonable time (< 5 seconds for 100 nodes)
    ?assert(Duration < 5_000_000),

    %% Memory check (SVG size should be reasonable)
    {ok, LargeSvg} = erlmcp_mermaid_renderer:render_svg(LargeAst),
    SvgSize = byte_size(LargeSvg),
    ?assert(SvgSize < 1_000_000),  % Less than 1MB

    ok.
