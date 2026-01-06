#!/usr/bin/env bash
set -euo pipefail

# Script d'installation pour l'autocomplétion de ticket

BASH_COMPLETION_FILE="ticket-completion.bash"
ZSH_COMPLETION_FILE="ticket-completion.zsh"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "Installation de l'autocomplétion pour 'ticket'..."
echo

# Détecter le shell actuel
CURRENT_SHELL="$(basename "$SHELL")"
echo "Shell détecté: $CURRENT_SHELL"
echo

# Vérifier que les fichiers de completion existent
if [[ ! -f "$SCRIPT_DIR/$BASH_COMPLETION_FILE" ]]; then
    echo "Erreur: $BASH_COMPLETION_FILE introuvable dans $SCRIPT_DIR" >&2
    exit 1
fi

if [[ ! -f "$SCRIPT_DIR/$ZSH_COMPLETION_FILE" ]]; then
    echo "Erreur: $ZSH_COMPLETION_FILE introuvable dans $SCRIPT_DIR" >&2
    exit 1
fi

# Détecter l'OS
OS="$(uname -s)"

case "$OS" in
    Linux*)
        echo "Système détecté: Linux"
        
        # Option 1: Installation système (nécessite sudo)
        if [[ -d /usr/share/bash-completion/completions ]]; then
            echo
            echo "Option 1: Installation système (recommandé)"
            echo "  Nécessite les privilèges sudo"
            echo "  Fichier: /usr/share/bash-completion/completions/ticket"
            read -p "  Installer en mode système ? (o/N) " -n 1 -r
            echo
            if [[ $REPLY =~ ^[OoYy]$ ]]; then
                sudo cp "$SCRIPT_DIR/$BASH_COMPLETION_FILE" /usr/share/bash-completion/completions/ticket
                echo "  ✓ Installation système réussie (Bash)"
                echo "  Rechargez votre shell avec: exec bash"
                exit 0
            fi
        fi
        
        # Installation pour zsh si disponible
        if [[ -d /usr/share/zsh/site-functions ]]; then
            echo
            echo "Zsh détecté - Installation pour zsh"
            read -p "  Installer l'autocomplétion zsh ? (o/N) " -n 1 -r
            echo
            if [[ $REPLY =~ ^[OoYy]$ ]]; then
                sudo cp "$SCRIPT_DIR/$ZSH_COMPLETION_FILE" /usr/share/zsh/site-functions/_ticket
                echo "  ✓ Installation système réussie (Zsh)"
                echo "  Rechargez votre shell avec: exec zsh"
                exit 0
            fi
        fi
        
        # Option 2: Installation utilisateur
        echo
        echo "Option 2: Installation utilisateur"
        
        # Installation Bash
        if [[ "$CURRENT_SHELL" == "bash" ]] || [[ -f "$HOME/.bashrc" ]]; then
            BASH_COMPLETION_DIR="${XDG_DATA_HOME:-$HOME/.local/share}/bash-completion/completions"
            mkdir -p "$BASH_COMPLETION_DIR"
            cp "$SCRIPT_DIR/$BASH_COMPLETION_FILE" "$BASH_COMPLETION_DIR/ticket"
            echo "  ✓ Fichier Bash copié dans: $BASH_COMPLETION_DIR/ticket"
            
            # Vérifier si bash-completion est activé dans .bashrc
            if ! grep -q "bash-completion" "$HOME/.bashrc" 2>/dev/null; then
                echo
                echo "  Ajout de bash-completion dans ~/.bashrc..."
                cat >> "$HOME/.bashrc" << 'EOF'

# Enable bash completion
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi
if [ -d "$HOME/.local/share/bash-completion/completions" ]; then
    for file in "$HOME/.local/share/bash-completion/completions"/*; do
        [ -r "$file" ] && . "$file"
    done
fi
EOF
                echo "  ✓ Configuration ajoutée à ~/.bashrc"
            fi
        fi
        
        # Installation Zsh
        if [[ "$CURRENT_SHELL" == "zsh" ]] || [[ -f "$HOME/.zshrc" ]]; then
            ZSH_COMPLETION_DIR="${XDG_DATA_HOME:-$HOME/.local/share}/zsh/site-functions"
            mkdir -p "$ZSH_COMPLETION_DIR"
            cp "$SCRIPT_DIR/$ZSH_COMPLETION_FILE" "$ZSH_COMPLETION_DIR/_ticket"
            echo "  ✓ Fichier Zsh copié dans: $ZSH_COMPLETION_DIR/_ticket"
            
            # Vérifier si le fpath inclut ce répertoire
            if ! grep -q "$ZSH_COMPLETION_DIR" "$HOME/.zshrc" 2>/dev/null; then
                echo
                echo "  Ajout de fpath dans ~/.zshrc..."
                cat >> "$HOME/.zshrc" << EOF

# Enable custom completions
fpath=($ZSH_COMPLETION_DIR \$fpath)
autoload -Uz compinit && compinit
EOF
                echo "  ✓ Configuration ajoutée à ~/.zshrc"
            fi
        fi
        ;;
        
    Darwin*)
        echo "Système détecté: macOS"
        
        # Vérifier si Homebrew est installé
        if command -v brew &>/dev/null; then
            echo
            echo "Homebrew détecté"
            
            # Vérifier si bash-completion est installé
            if ! brew list bash-completion@2 &>/dev/null && ! brew list bash-completion &>/dev/null; then
                echo "  bash-completion n'est pas installé"
                read -p "  Installer bash-completion@2 via Homebrew ? (o/N) " -n 1 -r
                echo
                if [[ $REPLY =~ ^[OoYy]$ ]]; then
                    brew install bash-completion@2
                fi
            fi
            
            # Installer le fichier de completion
            BREW_PREFIX="$(brew --prefix)"
            
            # Installation Bash
            if [[ "$CURRENT_SHELL" == "bash" ]] || [[ -f "$HOME/.bash_profile" ]] || [[ -f "$HOME/.bashrc" ]]; then
                BASH_COMPLETION_DIR="$BREW_PREFIX/etc/bash_completion.d"
                if [[ -d "$BASH_COMPLETION_DIR" ]]; then
                    cp "$SCRIPT_DIR/$BASH_COMPLETION_FILE" "$BASH_COMPLETION_DIR/ticket"
                    echo "  ✓ Fichier Bash copié dans: $BASH_COMPLETION_DIR/ticket"
                    
                    # Vérifier la configuration bash-completion dans le profil
                    PROFILE_FILE="$HOME/.bash_profile"
                    [[ ! -f "$PROFILE_FILE" ]] && PROFILE_FILE="$HOME/.bashrc"
                    
                    if ! grep -q "bash_completion" "$PROFILE_FILE" 2>/dev/null; then
                        echo
                        echo "  Ajout de bash-completion dans $PROFILE_FILE..."
                        cat >> "$PROFILE_FILE" << 'EOF'

# Enable bash completion (Homebrew)
if [[ -r "$(brew --prefix)/etc/profile.d/bash_completion.sh" ]]; then
    . "$(brew --prefix)/etc/profile.d/bash_completion.sh"
fi
EOF
                        echo "  ✓ Configuration ajoutée à $PROFILE_FILE"
                    fi
                fi
            fi
            
            # Installation Zsh
            if [[ "$CURRENT_SHELL" == "zsh" ]] || [[ -f "$HOME/.zshrc" ]]; then
                ZSH_COMPLETION_DIR="$BREW_PREFIX/share/zsh/site-functions"
                if [[ -d "$ZSH_COMPLETION_DIR" ]]; then
                    cp "$SCRIPT_DIR/$ZSH_COMPLETION_FILE" "$ZSH_COMPLETION_DIR/_ticket"
                    echo "  ✓ Fichier Zsh copié dans: $ZSH_COMPLETION_DIR/_ticket"
                else
                    # Fallback to user directory
                    ZSH_COMPLETION_DIR="${XDG_DATA_HOME:-$HOME/.local/share}/zsh/site-functions"
                    mkdir -p "$ZSH_COMPLETION_DIR"
                    cp "$SCRIPT_DIR/$ZSH_COMPLETION_FILE" "$ZSH_COMPLETION_DIR/_ticket"
                    echo "  ✓ Fichier Zsh copié dans: $ZSH_COMPLETION_DIR/_ticket"
                    
                    if ! grep -q "$ZSH_COMPLETION_DIR" "$HOME/.zshrc" 2>/dev/null; then
                        echo
                        echo "  Ajout de fpath dans ~/.zshrc..."
                        cat >> "$HOME/.zshrc" << EOF

# Enable custom completions
fpath=($ZSH_COMPLETION_DIR \$fpath)
autoload -Uz compinit && compinit
EOF
                        echo "  ✓ Configuration ajoutée à ~/.zshrc"
                    fi
                fi
            fi
        else
            echo "  Homebrew non détecté"
            echo "  Installation manuelle requise - voir AUTOCOMPLETE.md"
            exit 1
        fi
        ;;
        
    *)
        echo "Système non reconnu: $OS"
        echo "Installation manuelle requise - voir AUTOCOMPLETE.md"
        exit 1
        ;;
esac

echo
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "✓ Installation terminée avec succès !"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo
echo "Pour activer l'autocomplétion:"
if [[ "$CURRENT_SHELL" == "zsh" ]]; then
    echo "  1. Rechargez votre shell: exec zsh"
    echo "  2. Ou ouvrez un nouveau terminal"
    echo
    echo "Pour tester:"
    echo "  ticket <TAB>"
else
    echo "  1. Rechargez votre shell: exec bash"
    echo "  2. Ou ouvrez un nouveau terminal"
    echo
    echo "Pour tester:"
    echo "  ticket <TAB>"
fi
echo
echo "Pour plus d'informations, consultez: AUTOCOMPLETE.md"
echo