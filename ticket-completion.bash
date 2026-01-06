#!/usr/bin/env bash
# Bash completion for ticket command

# Get all ticket IDs from .tickets directory
_ticket_get_ids() {
    local TICKETS_DIR=".tickets"
    if [[ -d "$TICKETS_DIR" ]]; then
        local ids=()
        local file
        for file in "$TICKETS_DIR"/*.md; do
            if [[ -f "$file" ]]; then
                ids+=("$(basename "$file" .md)")
            fi
        done
        echo "${ids[@]}"
    fi
}

_ticket_completion() {
    local cur prev words cword
    _init_completion || return

    local TICKETS_DIR=".tickets"

    # Main commands
    local commands="create start close reopen status dep undep link unlink ls ready blocked closed show edit add-note query migrate-beads help"
    
    # Valid statuses
    local statuses="open in_progress closed"
    
    # Commands that need ticket ID(s)
    local id_commands="start close reopen status show edit add-note"
    local multi_id_commands="link dep undep unlink"

    # Get the main command (first word after 'ticket')
    local cmd=""
    if [[ $cword -ge 1 ]]; then
        cmd="${words[1]}"
    fi

    case $cword in
        1)
            # Complete main commands
            COMPREPLY=($(compgen -W "$commands" -- "$cur"))
            return 0
            ;;
        2)
            case "$cmd" in
                # Commands that need a ticket ID
                start|close|reopen|show|edit|add-note)
                    COMPREPLY=($(compgen -W "$(_ticket_get_ids)" -- "$cur"))
                    return 0
                    ;;
                # status needs ticket ID first
                status)
                    COMPREPLY=($(compgen -W "$(_ticket_get_ids)" -- "$cur"))
                    return 0
                    ;;
                # dep has subcommands or ticket ID
                dep)
                    COMPREPLY=($(compgen -W "tree $(_ticket_get_ids)" -- "$cur"))
                    return 0
                    ;;
                # Commands that need multiple ticket IDs
                link|undep|unlink)
                    COMPREPLY=($(compgen -W "$(_ticket_get_ids)" -- "$cur"))
                    return 0
                    ;;
                # ls can have --status flag
                ls)
                    if [[ "$cur" == -* ]]; then
                        COMPREPLY=($(compgen -W "--status=" -- "$cur"))
                        [[ ${#COMPREPLY[@]} -eq 1 ]] && [[ "${COMPREPLY[0]}" == *= ]] && compopt -o nospace
                    fi
                    return 0
                    ;;
                # closed can have --limit flag
                closed)
                    if [[ "$cur" == -* ]]; then
                        COMPREPLY=($(compgen -W "--limit=" -- "$cur"))
                        [[ ${#COMPREPLY[@]} -eq 1 ]] && [[ "${COMPREPLY[0]}" == *= ]] && compopt -o nospace
                    fi
                    return 0
                    ;;
                # create has many options
                create)
                    if [[ "$cur" == -* ]]; then
                        COMPREPLY=($(compgen -W "-d --description --design --acceptance -t --type -p --priority -a --assignee --external-ref --parent" -- "$cur"))
                    fi
                    return 0
                    ;;
            esac
            ;;
        3)
            case "$cmd" in
                # status needs status value after ticket ID
                status)
                    COMPREPLY=($(compgen -W "$statuses" -- "$cur"))
                    return 0
                    ;;
                # dep can be 'tree' with options or regular dep with ticket ID
                dep)
                    if [[ "${words[2]}" == "tree" ]]; then
                        # dep tree needs ticket ID or --full flag
                        if [[ "$cur" == -* ]]; then
                            COMPREPLY=($(compgen -W "--full" -- "$cur"))
                        else
                            COMPREPLY=($(compgen -W "$(_ticket_get_ids)" -- "$cur"))
                        fi
                    else
                        # Regular dep needs second ticket ID
                        COMPREPLY=($(compgen -W "$(_ticket_get_ids)" -- "$cur"))
                    fi
                    return 0
                    ;;
                # undep and unlink need second ticket ID
                undep|unlink)
                    COMPREPLY=($(compgen -W "$(_ticket_get_ids)" -- "$cur"))
                    return 0
                    ;;
                # link can have more ticket IDs
                link)
                    COMPREPLY=($(compgen -W "$(_ticket_get_ids)" -- "$cur"))
                    return 0
                    ;;
                # create options
                create)
                    case "$prev" in
                        -t|--type)
                            COMPREPLY=($(compgen -W "bug feature task epic chore" -- "$cur"))
                            return 0
                            ;;
                        -p|--priority)
                            COMPREPLY=($(compgen -W "0 1 2 3 4" -- "$cur"))
                            return 0
                            ;;
                        --parent)
                            COMPREPLY=($(compgen -W "$(_ticket_get_ids)" -- "$cur"))
                            return 0
                            ;;
                        -d|--description|--design|--acceptance|-a|--assignee|--external-ref)
                            # These need text input, no completion
                            return 0
                            ;;
                        *)
                            if [[ "$cur" == -* ]]; then
                                COMPREPLY=($(compgen -W "-d --description --design --acceptance -t --type -p --priority -a --assignee --external-ref --parent" -- "$cur"))
                            fi
                            return 0
                            ;;
                    esac
                    ;;
            esac
            ;;
        *)
            # Handle additional arguments
            case "$cmd" in
                # dep tree with --full flag
                dep)
                    if [[ "${words[2]}" == "tree" ]]; then
                        if [[ "${words[3]}" == "--full" ]] && [[ $cword -eq 4 ]]; then
                            COMPREPLY=($(compgen -W "$(_ticket_get_ids)" -- "$cur"))
                        elif [[ "$cur" == -* ]] && [[ $cword -eq 4 ]]; then
                            COMPREPLY=($(compgen -W "--full" -- "$cur"))
                        fi
                    fi
                    return 0
                    ;;
                # link can accept multiple ticket IDs
                link)
                    COMPREPLY=($(compgen -W "$(_ticket_get_ids)" -- "$cur"))
                    return 0
                    ;;
                # create options can appear at any position
                create)
                    case "$prev" in
                        -t|--type)
                            COMPREPLY=($(compgen -W "bug feature task epic chore" -- "$cur"))
                            return 0
                            ;;
                        -p|--priority)
                            COMPREPLY=($(compgen -W "0 1 2 3 4" -- "$cur"))
                            return 0
                            ;;
                        --parent)
                            COMPREPLY=($(compgen -W "$(_ticket_get_ids)" -- "$cur"))
                            return 0
                            ;;
                        -d|--description|--design|--acceptance|-a|--assignee|--external-ref)
                            return 0
                            ;;
                        *)
                            if [[ "$cur" == -* ]]; then
                                COMPREPLY=($(compgen -W "-d --description --design --acceptance -t --type -p --priority -a --assignee --external-ref --parent" -- "$cur"))
                            fi
                            return 0
                            ;;
                    esac
                    ;;
            esac
            ;;
    esac

    return 0
}

# Register the completion function for the ticket command
complete -F _ticket_completion ticket
complete -F _ticket_completion tk
