# Autocomplétion pour ticket/tk

Autocomplétion Bash et Zsh pour le système de tickets.

## Installation rapide

```bash
./install-completion.sh
exec zsh  # ou exec bash
```

## Test

```bash
tk <TAB>                    # Toutes les commandes
tk create --type <TAB>      # Types disponibles
tk show <TAB>               # Liste des tickets
tk status <ID> <TAB>        # Statuts disponibles
```

## Fonctionnalités

- ✓ Complétion des commandes
- ✓ Complétion des IDs de tickets
- ✓ Complétion des statuts (`open`, `in_progress`, `closed`)
- ✓ Complétion des types (`bug`, `feature`, `task`, `epic`, `chore`)
- ✓ Complétion des priorités (0-4)
- ✓ Complétion des options (`--type`, `--priority`, `--parent`, etc.)
- ✓ Support des sous-commandes (`dep tree --full`)
- ✓ Fonctionne avec `ticket` et `tk`

## Dépannage

Si l'autocomplétion ne fonctionne pas :

```bash
# 1. Diagnostiquer le problème
./diagnose-completion.sh

# 2. Corriger automatiquement
./fix-completion.sh

# 3. Recharger le shell
exec zsh
```

## Fichiers

- `ticket-completion.bash` - Autocomplétion Bash
- `ticket-completion.zsh` - Autocomplétion Zsh
- `install-completion.sh` - Installation automatique
- `diagnose-completion.sh` - Diagnostic
- `fix-completion.sh` - Correction automatique
