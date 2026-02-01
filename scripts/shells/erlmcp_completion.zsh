#compdef erlmcp-validate
# Zsh completion for erlmcp-validate
# Installation: Copy to ~/.zsh/completion/_erlmcp-validate or add to fpath

_erlmcp_validate() {
    local curcontext="$curcontext" state line
    typeset -A opt_args

    local -a commands
    commands=(
        'validate:Validate a running MCP server'
        'spec-check:Check spec compliance'
        'transport-check:Verify transport behavior'
        'report:Generate compliance report'
        'spec:Validate against MCP spec'
        'protocol:Validate protocol message'
        'transport:Validate transport'
        'compliance:Full compliance suite'
        'all:Run all validators'
        'run:Run validation tests'
        'quick-check:Quick validation check'
        'status:Show validation status'
        '--help:Show help message'
        '--version:Show version'
        '--gen-completions:Generate completion script'
        'install-completions:Install shell completions'
    )

    _arguments -C \
        '1: :->command' \
        '*:: :->args'

    case $state in
        command)
            _describe 'command' commands
            ;;
        args)
            case $words[1] in
                validate)
                    _arguments \
                        '1:url:_urls' \
                        '--format[Output format]:format:(text json markdown)' \
                        '--output[Output file]:file:_files' \
                        '--verbose[Verbose output]'
                    ;;
                spec-check)
                    _arguments \
                        '--format[Output format]:format:(text json markdown)' \
                        '--output[Output file]:file:_files' \
                        '--verbose[Verbose output]'
                    ;;
                transport-check)
                    _arguments \
                        '1:transport:(stdio tcp http websocket)' \
                        '--format[Output format]:format:(text json markdown)' \
                        '--output[Output file]:file:_files' \
                        '--verbose[Verbose output]'
                    ;;
                report)
                    _arguments \
                        '--format[Output format]:format:(text json markdown html)' \
                        '--output[Output file]:file:_files'
                    ;;
                protocol)
                    _arguments \
                        '--file[Input file]:file:_files' \
                        '--format[Output format]:format:(text json markdown)'
                    ;;
                transport)
                    _arguments \
                        '1:transport:(stdio tcp http websocket)' \
                        '--format[Output format]:format:(text json markdown)'
                    ;;
                run)
                    _arguments \
                        '--all[Run all sections]' \
                        '--section[Run section]:section:(protocol transport security error_handling performance)' \
                        '--transport[Transport]:transport:(stdio tcp http websocket)' \
                        '--format[Output format]:format:(text json markdown)' \
                        '--verbose[Verbose output]' \
                        '--quiet[Quiet output]'
                    ;;
                --gen-completions)
                    _arguments \
                        '1:shell:(bash zsh fish)'
                    ;;
                install-completions)
                    _arguments \
                        '1:shell:(bash zsh fish)'
                    ;;
            esac
            ;;
    esac
}

_erlmcp_validate "$@"
