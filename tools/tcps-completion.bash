#!/bin/bash
# TCPS CLI Bash Completion Script
#
# Installation:
#   Source this file in your .bashrc or .bash_profile:
#     source /path/to/tcps-completion.bash
#
#   Or copy to bash completion directory:
#     sudo cp tcps-completion.bash /etc/bash_completion.d/tcps
#

_tcps_completion() {
    local cur prev opts base
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    # Main commands
    local commands="work-order andon receipt quality kanban kaizen root-cause tpm example help version"

    # Global options
    local global_opts="--format --color --no-color --config --verbose -v --quiet -q"

    # If we're completing the first argument
    if [ $COMP_CWORD -eq 1 ]; then
        COMPREPLY=( $(compgen -W "${commands}" -- ${cur}) )
        return 0
    fi

    # Get the main command
    local command="${COMP_WORDS[1]}"

    case "${command}" in
        work-order)
            case "${prev}" in
                work-order)
                    local subcommands="create list show complete delete"
                    COMPREPLY=( $(compgen -W "${subcommands}" -- ${cur}) )
                    return 0
                    ;;
                create)
                    local opts="--bucket --priority --title --description"
                    COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
                    return 0
                    ;;
                list)
                    local opts="--status --bucket --limit"
                    COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
                    return 0
                    ;;
                --bucket)
                    local buckets="reliability security cost compliance"
                    COMPREPLY=( $(compgen -W "${buckets}" -- ${cur}) )
                    return 0
                    ;;
                --status)
                    local statuses="pending in_progress completed"
                    COMPREPLY=( $(compgen -W "${statuses}" -- ${cur}) )
                    return 0
                    ;;
            esac
            ;;

        andon)
            case "${prev}" in
                andon)
                    local subcommands="trigger list show resolve status"
                    COMPREPLY=( $(compgen -W "${subcommands}" -- ${cur}) )
                    return 0
                    ;;
                trigger)
                    local opts="--type --sku --stage --message"
                    COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
                    return 0
                    ;;
                list)
                    local opts="--open --all --sku"
                    COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
                    return 0
                    ;;
                resolve)
                    local opts="--root-cause --fix --prevention"
                    COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
                    return 0
                    ;;
                --type)
                    local types="test_failure shacl_violation compilation_failure non_determinism missing_receipt"
                    COMPREPLY=( $(compgen -W "${types}" -- ${cur}) )
                    return 0
                    ;;
                --stage)
                    local stages="compilation testing validation execution integration deployment"
                    COMPREPLY=( $(compgen -W "${stages}" -- ${cur}) )
                    return 0
                    ;;
            esac
            ;;

        receipt)
            case "${prev}" in
                receipt)
                    local subcommands="verify chain list show validate"
                    COMPREPLY=( $(compgen -W "${subcommands}" -- ${cur}) )
                    return 0
                    ;;
                verify)
                    local stages="compilation testing validation execution"
                    COMPREPLY=( $(compgen -W "${stages}" -- ${cur}) )
                    return 0
                    ;;
                chain)
                    local opts="--format"
                    COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
                    return 0
                    ;;
                list)
                    local opts="--recent"
                    COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
                    return 0
                    ;;
            esac
            ;;

        quality)
            case "${prev}" in
                quality)
                    local subcommands="gates metrics dashboard report"
                    COMPREPLY=( $(compgen -W "${subcommands}" -- ${cur}) )
                    return 0
                    ;;
                gates)
                    local opts="--verbose"
                    COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
                    return 0
                    ;;
                metrics)
                    local opts="--period --bucket --compare"
                    COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
                    return 0
                    ;;
                --period)
                    local periods="daily weekly monthly"
                    COMPREPLY=( $(compgen -W "${periods}" -- ${cur}) )
                    return 0
                    ;;
            esac
            ;;

        kanban)
            case "${prev}" in
                kanban)
                    local subcommands="status schedule set-limit pull"
                    COMPREPLY=( $(compgen -W "${subcommands}" -- ${cur}) )
                    return 0
                    ;;
                status)
                    local opts="--bucket"
                    COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
                    return 0
                    ;;
                set-limit)
                    local buckets="reliability security cost compliance"
                    COMPREPLY=( $(compgen -W "${buckets}" -- ${cur}) )
                    return 0
                    ;;
                pull)
                    local opts="--bucket --priority"
                    COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
                    return 0
                    ;;
            esac
            ;;

        kaizen)
            case "${prev}" in
                kaizen)
                    local subcommands="report proposals apply waste trends"
                    COMPREPLY=( $(compgen -W "${subcommands}" -- ${cur}) )
                    return 0
                    ;;
                report)
                    local opts="--weekly --monthly --output"
                    COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
                    return 0
                    ;;
                proposals)
                    local opts="--top --roi-threshold"
                    COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
                    return 0
                    ;;
            esac
            ;;

        root-cause)
            case "${prev}" in
                root-cause)
                    local subcommands="start add-why finalize show list"
                    COMPREPLY=( $(compgen -W "${subcommands}" -- ${cur}) )
                    return 0
                    ;;
                start)
                    local opts="--problem"
                    COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
                    return 0
                    ;;
                finalize)
                    local opts="--root-cause --prevention"
                    COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
                    return 0
                    ;;
            esac
            ;;

        tpm)
            case "${prev}" in
                tpm)
                    local subcommands="maintenance dashboard health metrics"
                    COMPREPLY=( $(compgen -W "${subcommands}" -- ${cur}) )
                    return 0
                    ;;
                maintenance)
                    local opts="--daily --weekly --force"
                    COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
                    return 0
                    ;;
            esac
            ;;

        example)
            case "${prev}" in
                example)
                    local examples="security-patch new-feature andon-resolve full-cycle"
                    COMPREPLY=( $(compgen -W "${examples}" -- ${cur}) )
                    return 0
                    ;;
                *)
                    local opts="--interactive --dry-run"
                    COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
                    return 0
                    ;;
            esac
            ;;

        help)
            COMPREPLY=( $(compgen -W "${commands}" -- ${cur}) )
            return 0
            ;;
    esac

    # Handle --format completion
    if [ "${prev}" == "--format" ]; then
        local formats="json table markdown"
        COMPREPLY=( $(compgen -W "${formats}" -- ${cur}) )
        return 0
    fi

    # Default: complete with global options
    COMPREPLY=( $(compgen -W "${global_opts}" -- ${cur}) )
    return 0
}

# Register the completion function
complete -F _tcps_completion tcps
