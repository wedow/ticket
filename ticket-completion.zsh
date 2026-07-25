#compdef ticket tk

# Zsh completion for ticket command (also works with 'tk' alias)

# Get all ticket IDs from .tickets directory
# Locate .tickets the same way ticket does: TICKETS_DIR wins, else walk parents
_ticket_find_dir() {
    [[ -n "${TICKETS_DIR:-}" ]] && { echo "$TICKETS_DIR"; return 0; }

    local dir="$PWD"
    while [[ "$dir" != "/" ]]; do
        if [[ -d "$dir/.tickets" ]]; then
            echo "$dir/.tickets"
            return 0
        fi
        dir="${dir:h}"
    done

    [[ -d "/.tickets" ]] && { echo "/.tickets"; return 0; }
    return 1
}

_ticket_get_ids() {
    local dir
    dir=$(_ticket_find_dir) || return 0
    [[ -d "$dir" ]] || return 0

    local ids=()
    local file
    for file in "$dir"/*.md(N); do
        ids+=("${file:t:r}")
    done
    echo ${ids[@]}
}

_ticket() {
    local line state

    _arguments -C \
        '1: :->command' \
        '*::arg:->args'

    case $state in
        command)
            local commands=(
                'create:Create a new ticket'
                'start:Set status to in_progress'
                'close:Set status to closed'
                'reopen:Set status to open'
                'status:Update ticket status'
                'dep:Add dependency or show dependency tree'
                'undep:Remove dependency'
                'link:Link tickets together'
                'unlink:Remove link between tickets'
                'ls:List tickets'
                'list:List tickets'
                'ready:List ready tickets'
                'blocked:List blocked tickets'
                'closed:List recently closed tickets'
                'show:Display ticket'
                'edit:Open ticket in editor'
                'add-note:Append note to ticket'
                'query:Output tickets as JSON'
                'migrate-beads:Import from beads'
                'super:Bypass plugins and run the built-in command'
                'help:Show help'
            )
            _describe 'command' commands
            ;;
        args)
            local cmd="${line[1]}"
            case "$cmd" in
                start|close|reopen|show|edit|add-note)
                    _arguments "1: :($(_ticket_get_ids))"
                    ;;
                status)
                    case $CURRENT in
                        2)
                            _arguments "1: :($(_ticket_get_ids))"
                            ;;
                        3)
                            _arguments "1: :(open in_progress closed)"
                            ;;
                    esac
                    ;;
                dep)
                    case $CURRENT in
                        2)
                            local subcmds=(
                                'tree:Show dependency tree'
                                'cycle:Find dependency cycles in open tickets'
                            )
                            _describe 'subcommand' subcmds
                            _arguments "1: :($(_ticket_get_ids))"
                            ;;
                        3)
                            if [[ "${line[2]}" == "tree" ]]; then
                                _arguments \
                                    '--full[Show full tree without deduplication]' \
                                    "1: :($(_ticket_get_ids))"
                            else
                                _arguments "1: :($(_ticket_get_ids))"
                            fi
                            ;;
                        4)
                            if [[ "${line[2]}" == "tree" ]]; then
                                _arguments "1: :($(_ticket_get_ids))"
                            fi
                            ;;
                    esac
                    ;;
                undep|unlink)
                    case $CURRENT in
                        2|3)
                            _arguments "1: :($(_ticket_get_ids))"
                            ;;
                    esac
                    ;;
                link)
                    _arguments "*: :($(_ticket_get_ids))"
                    ;;
                ls|list)
                    _arguments \
                        '--status=[Filter by status]:status:(open in_progress closed)' \
                        '-a[Filter by assignee]:assignee:' \
                        '-T[Filter by type]:type:(bug feature task epic chore)'
                    ;;
                ready|blocked)
                    _arguments \
                        '-a[Filter by assignee]:assignee:' \
                        '-T[Filter by type]:type:(bug feature task epic chore)'
                    ;;
                closed)
                    _arguments \
                        '--limit=[Limit number of results]:limit:' \
                        '-a[Filter by assignee]:assignee:' \
                        '-T[Filter by type]:type:(bug feature task epic chore)'
                    ;;
                create)
                    _arguments \
                        '1:title:' \
                        '(-d --description)'{-d,--description}'[Description text]:description:' \
                        '--design[Design notes]:design:' \
                        '--acceptance[Acceptance criteria]:acceptance:' \
                        '(-t --type)'{-t,--type}'[Ticket type]:type:(bug feature task epic chore)' \
                        '(-p --priority)'{-p,--priority}'[Priority]:priority:(0 1 2 3 4)' \
                        '(-a --assignee)'{-a,--assignee}'[Assignee]:assignee:' \
                        '--external-ref[External reference]:reference:' \
                        '--tags[Comma-separated tags]:tags:' \
                        "--parent[Parent ticket]:parent:($(_ticket_get_ids))"
                    ;;
            esac
            ;;
    esac
}

_ticket "$@"