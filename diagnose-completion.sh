#!/usr/bin/env zsh

echo "═══════════════════════════════════════════════════════"
echo "Diagnostic de l'autocomplétion pour tk/ticket"
echo "═══════════════════════════════════════════════════════"
echo

# 1. Vérifier le shell
echo "1. Shell actuel:"
echo "   $SHELL"
echo

# 2. Vérifier si le fichier de complétion existe
echo "2. Fichier de complétion installé:"
if [[ -f /opt/homebrew/share/zsh/site-functions/_ticket ]]; then
    echo "   ✓ /opt/homebrew/share/zsh/site-functions/_ticket existe"
    echo "   Contenu (premières lignes):"
    head -3 /opt/homebrew/share/zsh/site-functions/_ticket | sed 's/^/     /'
else
    echo "   ✗ /opt/homebrew/share/zsh/site-functions/_ticket n'existe pas"
fi
echo

# 3. Vérifier le fpath
echo "3. Répertoires dans fpath:"
echo "$fpath" | tr ' ' '\n' | grep -E "(homebrew|zsh)" | sed 's/^/   /'
echo

# 4. Vérifier si le répertoire est dans fpath
echo "4. Le répertoire homebrew est-il dans fpath?"
if echo "$fpath" | grep -q "homebrew/share/zsh/site-functions"; then
    echo "   ✓ Oui"
else
    echo "   ✗ Non - C'est le problème!"
fi
echo

# 5. Vérifier compinit
echo "5. compinit dans .zshrc:"
if grep -q "compinit" ~/.zshrc 2>/dev/null; then
    echo "   ✓ compinit trouvé dans .zshrc"
    grep "compinit" ~/.zshrc | sed 's/^/     /'
else
    echo "   ✗ compinit non trouvé dans .zshrc"
fi
echo

# 6. Vérifier si la fonction de complétion est chargée
echo "6. Fonction de complétion chargée:"
if type _ticket &>/dev/null; then
    echo "   ✓ _ticket est chargée"
else
    echo "   ✗ _ticket n'est pas chargée"
fi
echo

# 7. Vérifier si compdef fonctionne
echo "7. Commandes avec complétion enregistrée:"
compdef | grep -E "(ticket|tk)" | sed 's/^/   /' || echo "   Aucune complétion enregistrée pour ticket/tk"
echo

# 8. Test manuel
echo "8. Test de chargement manuel:"
if [[ -f /opt/homebrew/share/zsh/site-functions/_ticket ]]; then
    echo "   Tentative de chargement..."
    source /opt/homebrew/share/zsh/site-functions/_ticket 2>&1 | sed 's/^/     /'
    if type _ticket &>/dev/null; then
        echo "   ✓ Chargement réussi"
    else
        echo "   ✗ Échec du chargement"
    fi
else
    echo "   ✗ Fichier introuvable"
fi
echo

echo "═══════════════════════════════════════════════════════"
echo "Recommandations:"
echo "═══════════════════════════════════════════════════════"
echo

# Recommandations basées sur les vérifications
if ! echo "$fpath" | grep -q "homebrew/share/zsh/site-functions"; then
    echo "⚠ Le répertoire Homebrew n'est pas dans fpath"
    echo "  Solution: Ajouter à ~/.zshrc:"
    echo '  fpath=(/opt/homebrew/share/zsh/site-functions $fpath)'
    echo
fi

if ! grep -q "compinit" ~/.zshrc 2>/dev/null; then
    echo "⚠ compinit n'est pas dans .zshrc"
    echo "  Solution: Ajouter à ~/.zshrc:"
    echo '  autoload -Uz compinit && compinit'
    echo
fi

if ! type _ticket &>/dev/null; then
    echo "⚠ La fonction de complétion n'est pas chargée"
    echo "  Solution: Après avoir ajouté fpath et compinit, rechargez avec:"
    echo '  exec zsh'
    echo
fi

echo "Pour appliquer les corrections automatiquement, exécutez:"
echo "  ./fix-completion.sh"
echo