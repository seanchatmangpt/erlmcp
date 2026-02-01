#!/usr/bin/env bash
# Analyze Dialyzer warnings by file and category

# Strip ANSI color codes
cat dialyzer_detailed.log | sed 's/\x1b\[[0-9;]*m//g' > dialyzer_clean.txt

echo "=== Warnings by File (Top 20) ==="
awk '
/^apps\// { file = $0; next }
/^Line/ { if (file != "") count[file]++ }
END {
    for (f in count) print count[f], f
}
' dialyzer_clean.txt | sort -rn | head -20

echo ""
echo "=== Warning Categories (Top 20) ==="
awk -F': ' '
/^Line/ {
    # Extract the warning message after "Line X Column Y: "
    msg = $2
    # Remove leading/trailing whitespace
    gsub(/^[ \t]+|[ \t]+$/, "", msg)
    # Count occurrences
    count[msg]++
}
END {
    for (m in count) print count[m], m
}
' dialyzer_clean.txt | sort -rn | head -20

echo ""
echo "=== Total Warnings ==="
grep -c "^Line" dialyzer_clean.txt

echo ""
echo "=== Apps with Warnings ==="
awk '/^apps\// { match($0, /apps\/([^\/]+)/, arr); app=arr[1] }
     /^Line/ { if (app != "") count[app]++ }
     END { for (a in count) print count[a], a }' dialyzer_clean.txt | sort -rn
