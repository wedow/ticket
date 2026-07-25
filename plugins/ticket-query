#!/usr/bin/env bash
# tk-plugin: Output tickets as JSON, optionally filtered with jq
# tk-plugin-version: 1.0.0
set -euo pipefail

filter="${1:-}"

# Generate all JSON in one awk pass
json_output=$(awk '
BEGIN { FS=": "; in_front=0 }
FNR==1 {
    if (prev_file) emit()
    field_count=0; in_front=0
    prev_file=FILENAME
}
/^---$/ { in_front = !in_front; next }
in_front && /^[a-zA-Z]/ {
    key = $1
    val = substr($0, length($1) + 3)
    gsub(/^ +| +$/, "", val)
    field_count++
    field_keys[field_count] = key
    field_vals[field_count] = val
}
function emit() {
    if (field_count > 0) {
        printf "{"
        for (i = 1; i <= field_count; i++) {
            if (i > 1) printf ","
            key = field_keys[i]
            val = field_vals[i]
            # Handle arrays
            if (val ~ /^\[.*\]$/) {
                gsub(/^\[|\]$/, "", val)
                n = split(val, items, ", *")
                printf "\"%s\":[", key
                for (j = 1; j <= n; j++) {
                    if (j > 1) printf ","
                    gsub(/^ +| +$/, "", items[j])
                    if (items[j] != "") printf "\"%s\"", items[j]
                }
                printf "]"
            } else {
                printf "\"%s\":\"%s\"", key, val
            }
        }
        printf "}\n"
    }
}
END { if (prev_file) emit() }
' "$TICKETS_DIR"/*.md 2>/dev/null)

if [[ -n "$filter" ]]; then
    echo "$json_output" | jq -c "select($filter)"
else
    echo "$json_output"
fi
