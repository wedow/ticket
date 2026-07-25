#!/usr/bin/env bash
set -euo pipefail

# Install shell completion for ticket/tk into the user's completion directory.
# No sudo, no system paths.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"

install_bash() {
    local dir="$DATA_HOME/bash-completion/completions"
    mkdir -p "$dir"
    cp "$SCRIPT_DIR/ticket-completion.bash" "$dir/ticket"
    ln -sf ticket "$dir/tk"
    echo "bash: installed $dir/ticket"
    echo "      requires the bash-completion package to be loaded in your shell"
}

install_zsh() {
    local dir="$DATA_HOME/zsh/site-functions"
    mkdir -p "$dir"
    cp "$SCRIPT_DIR/ticket-completion.zsh" "$dir/_ticket"
    echo "zsh: installed $dir/_ticket"

    if [[ -f "$HOME/.zshrc" ]] && ! grep -qF "$dir" "$HOME/.zshrc"; then
        cat >> "$HOME/.zshrc" <<EOF

# ticket completion
fpath=($dir \$fpath)
autoload -Uz compinit && compinit
EOF
        echo "     added fpath entry to ~/.zshrc"
    fi
}

for f in ticket-completion.bash ticket-completion.zsh; do
    [[ -f "$SCRIPT_DIR/$f" ]] || { echo "missing $f in $SCRIPT_DIR" >&2; exit 1; }
done

case "${1:-auto}" in
    bash) install_bash ;;
    zsh)  install_zsh ;;
    auto)
        if [[ -f "$HOME/.bashrc" || "$(basename "${SHELL:-}")" == bash ]]; then
            install_bash
        fi
        if [[ -f "$HOME/.zshrc" || "$(basename "${SHELL:-}")" == zsh ]]; then
            install_zsh
        fi
        ;;
    *) echo "usage: $0 [bash|zsh|auto]" >&2; exit 1 ;;
esac

echo
echo "Restart your shell (exec \"\$SHELL\") to pick it up."
