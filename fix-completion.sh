#!/usr/bin/env zsh

echo "═══════════════════════════════════════════════════════"
echo "Correction automatique de l'autocomplétion"
echo "═══════════════════════════════════════════════════════"
echo

ZSHRC="$HOME/.zshrc"
BACKUP="$HOME/.zshrc.backup.$(date +%Y%m%d_%H%M%S)"

# Créer une sauvegarde
echo "1. Création d'une sauvegarde de .zshrc..."
cp "$ZSHRC" "$BACKUP"
echo "   ✓ Sauvegarde créée: $BACKUP"
echo

# Vérifier et ajouter fpath
echo "2. Vérification du fpath..."
if ! grep -q "fpath.*homebrew.*zsh.*site-functions" "$ZSHRC" 2>/dev/null; then
    echo "   Ajout du fpath Homebrew..."
    cat >> "$ZSHRC" << 'EOF'

# Homebrew completions
fpath=(/opt/homebrew/share/zsh/site-functions $fpath)
EOF
    echo "   ✓ fpath ajouté"
else
    echo "   ✓ fpath déjà configuré"
fi
echo

# Vérifier et ajouter compinit
echo "3. Vérification de compinit..."
if ! grep -q "compinit" "$ZSHRC" 2>/dev/null; then
    echo "   Ajout de compinit..."
    cat >> "$ZSHRC" << 'EOF'
autoload -Uz compinit && compinit
EOF
    echo "   ✓ compinit ajouté"
else
    echo "   ✓ compinit déjà configuré"
fi
echo

# Vérifier que le fichier de complétion existe
echo "4. Vérification du fichier de complétion..."
if [[ -f /opt/homebrew/share/zsh/site-functions/_ticket ]]; then
    echo "   ✓ Fichier _ticket trouvé"
else
    echo "   ✗ Fichier _ticket non trouvé"
    echo "   Réinstallation..."
    SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
    if [[ -f "$SCRIPT_DIR/ticket-completion.zsh" ]]; then
        cp "$SCRIPT_DIR/ticket-completion.zsh" /opt/homebrew/share/zsh/site-functions/_ticket
        echo "   ✓ Fichier copié"
    else
        echo "   ✗ Impossible de trouver ticket-completion.zsh"
        exit 1
    fi
fi
echo

echo "═══════════════════════════════════════════════════════"
echo "✓ Configuration terminée!"
echo "═══════════════════════════════════════════════════════"
echo
echo "Pour activer les changements:"
echo "  1. Rechargez votre shell: exec zsh"
echo "  2. Testez: tk <TAB>"
echo
echo "Si vous voulez restaurer l'ancien .zshrc:"
echo "  cp $BACKUP ~/.zshrc"
echo