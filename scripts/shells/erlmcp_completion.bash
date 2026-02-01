# Bash completion for erlmcp-validate
# Installation: source this file or copy to ~/.bash_completion.d/

_erlmcp_validate_completion() {
    local cur prev opts
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    # Main commands
    local commands="validate spec-check transport-check report spec protocol transport compliance all run quick-check status --help --version --gen-completions install-completions"

    # Transports
    local transports="stdio tcp http websocket"

    # Formats
    local formats="text json markdown html"

    # Sections
    local sections="protocol transport security error_handling performance"

    # If completing first argument after command name
    if [ $COMP_CWORD -eq 1 ]; then
        COMPREPLY=( $(compgen -W "${commands}" -- ${cur}) )
        return 0
    fi

    # Get the command (first argument)
    local command="${COMP_WORDS[1]}"

    # Command-specific completions
    case "${prev}" in
        --format)
            COMPREPLY=( $(compgen -W "${formats}" -- ${cur}) )
            return 0
            ;;
        --transport)
            COMPREPLY=( $(compgen -W "${transports}" -- ${cur}) )
            return 0
            ;;
        --section)
            COMPREPLY=( $(compgen -W "${sections}" -- ${cur}) )
            return 0
            ;;
        --file|--output)
            COMPREPLY=( $(compgen -f -- ${cur}) )
            return 0
            ;;
        --gen-completions)
            COMPREPLY=( $(compgen -W "bash zsh fish" -- ${cur}) )
            return 0
            ;;
    esac

    # Command-specific options
    case "${command}" in
        validate)
            if [ $COMP_CWORD -eq 2 ]; then
                # Complete URL or transport scheme
                COMPREPLY=( $(compgen -W "stdio:// tcp:// http:// https:// ws:// wss://" -- ${cur}) )
            else
                COMPREPLY=( $(compgen -W "--format --output --verbose" -- ${cur}) )
            fi
            ;;
        spec-check)
            COMPREPLY=( $(compgen -W "--format --output --verbose" -- ${cur}) )
            ;;
        transport-check)
            if [ $COMP_CWORD -eq 2 ]; then
                COMPREPLY=( $(compgen -W "${transports}" -- ${cur}) )
            else
                COMPREPLY=( $(compgen -W "--format --output --verbose" -- ${cur}) )
            fi
            ;;
        report)
            COMPREPLY=( $(compgen -W "--format --output" -- ${cur}) )
            ;;
        protocol)
            COMPREPLY=( $(compgen -W "--file --format" -- ${cur}) )
            ;;
        transport)
            if [ $COMP_CWORD -eq 2 ]; then
                COMPREPLY=( $(compgen -W "${transports}" -- ${cur}) )
            else
                COMPREPLY=( $(compgen -W "--format" -- ${cur}) )
            fi
            ;;
        run)
            COMPREPLY=( $(compgen -W "--all --section --transport --format --verbose --quiet" -- ${cur}) )
            ;;
        install-completions)
            COMPREPLY=( $(compgen -W "bash zsh fish" -- ${cur}) )
            ;;
    esac
}

complete -F _erlmcp_validate_completion erlmcp-validate
