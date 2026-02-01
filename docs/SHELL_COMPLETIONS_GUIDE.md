# erlmcp Shell Completions Guide

Install and use shell tab completion for erlmcp CLI commands. Supports bash, zsh, and fish shells.

**Table of Contents**:
- [Bash Completion](#bash-completion)
- [Zsh Completion](#zsh-completion)
- [Fish Completion](#fish-completion)
- [Manual Completion](#manual-completion)
- [Troubleshooting](#troubleshooting)

## Bash Completion

### Installation (bash 3.2+)

**Option 1: Global installation (recommended)**

```bash
# Copy completion script to system completions directory
sudo cp /path/to/erlmcp/completions/erlmcp.bash \
  /etc/bash_completion.d/erlmcp

# Or on macOS with Homebrew bash-completion
sudo cp /path/to/erlmcp/completions/erlmcp.bash \
  $(brew --prefix)/etc/bash_completion.d/erlmcp
```

**Option 2: User installation (no sudo required)**

```bash
# Create user completion directory if it doesn't exist
mkdir -p ~/.bash_completion.d

# Copy completion script
cp /path/to/erlmcp/completions/erlmcp.bash ~/.bash_completion.d/

# Add to ~/.bashrc
cat >> ~/.bashrc << 'EOF'
# erlmcp completions
if [ -d ~/.bash_completion.d ]; then
  for completion in ~/.bash_completion.d/*; do
    source "$completion"
  done
fi
EOF

# Reload bashrc
source ~/.bashrc
```

**Option 3: Direct source (one-liner)**

```bash
source /path/to/erlmcp/completions/erlmcp.bash
```

### Verification

```bash
# Test completion (type and press TAB)
erlmcp [TAB]

# Should show available commands
erlmcp init
erlmcp start
erlmcp stop
erlmcp status
erlmcp doctor
erlmcp test-100k
erlmcp benchmark
erlmcp help
```

### Usage examples

```bash
# Command completion
erlmcp [TAB]        # Shows all commands

# Help with specific command
erlmcp help [TAB]   # Completes to: erlmcp help
erlmcp doctor[TAB]  # Completes to: erlmcp doctor
```

---

## Zsh Completion

### Installation (zsh 4.3+)

**Option 1: System-wide (requires sudo)**

```bash
# Copy completion to zsh completion directory
sudo cp /path/to/erlmcp/completions/erlmcp.zsh \
  /usr/share/zsh/site-functions/_erlmcp

# Reload shell
exec zsh
```

**Option 2: User installation (no sudo)**

```bash
# Create user completion directory
mkdir -p ~/.zsh/completions

# Copy completion script
cp /path/to/erlmcp/completions/erlmcp.zsh ~/.zsh/completions/_erlmcp

# Add to ~/.zshrc if not already there
cat >> ~/.zshrc << 'EOF'
# erlmcp completions
fpath=(~/.zsh/completions $fpath)
autoload -U compinit
compinit
EOF

# Reload zshrc
source ~/.zshrc
```

**Option 3: Using oh-my-zsh (if installed)**

```bash
# Copy to oh-my-zsh plugins directory
cp /path/to/erlmcp/completions/erlmcp.zsh \
  ~/.oh-my-zsh/completions/_erlmcp

# Add erlmcp to ~/.zshrc plugins (if needed)
# plugins=(... erlmcp)

# Reload
exec zsh
```

### Verification

```bash
# Test completion
erlmcp [TAB]

# With zsh, you'll see a formatted list
doctor    -- Run host readiness diagnostics
init      -- Initialize local development environment
start     -- Start local erlmcp cluster
stop      -- Stop local erlmcp cluster
test-100k -- Run 100K concurrent test
benchmark -- Run performance benchmarks
help      -- Show help message
```

### Advanced zsh features

**Argument completion**:
```bash
erlmcp doctor[TAB]       # Completes with description
erlmcp doctor -[TAB]     # Shows available flags (if any)
```

**Partial matching**:
```bash
erlmcp te[TAB]           # Completes to: erlmcp test-100k
erlmcp ben[TAB]          # Completes to: erlmcp benchmark
```

---

## Fish Completion

### Installation (fish 2.3+)

**Option 1: System-wide**

```bash
# Copy to fish completions directory
sudo cp /path/to/erlmcp/completions/erlmcp.fish \
  /usr/share/fish/vendor_completions.d/erlmcp.fish

# Or on macOS with Homebrew
sudo cp /path/to/erlmcp/completions/erlmcp.fish \
  $(brew --prefix)/share/fish/vendor_completions.d/erlmcp.fish
```

**Option 2: User installation**

```bash
# Create user completions directory
mkdir -p ~/.config/fish/completions

# Copy completion script
cp /path/to/erlmcp/completions/erlmcp.fish \
  ~/.config/fish/completions/erlmcp.fish

# Reload fish
exec fish
```

### Verification

```bash
# Test completion
erlmcp [TAB]

# Should show commands with descriptions
init     (Initialize local development environment)
start    (Start local erlmcp cluster)
stop     (Stop local erlmcp cluster)
status   (Show cluster status)
doctor   (Run host readiness diagnostics)
test-100k (Run 100K concurrent test)
benchmark (Run performance benchmarks)
help     (Show help message)
```

### Usage examples

```bash
# Basic completion
erlmcp [TAB]              # Shows all commands

# Partial matching
erlmcp d[TAB]             # Completes to: erlmcp doctor
erlmcp tes[TAB]           # Completes to: erlmcp test-100k
```

---

## Manual Completion

If automated installation doesn't work, you can manually trigger completion:

### Bash manual setup

Create `~/.bash_completion.d/erlmcp`:

```bash
#!/bin/bash

_erlmcp_completions() {
    local cur prev opts
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    opts="init start stop status doctor test-100k benchmark help"

    if [[ ${cur} == -* ]] ; then
        return 0
    fi

    COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
    return 0
}

complete -F _erlmcp_completions erlmcp
```

Then source it:
```bash
source ~/.bash_completion.d/erlmcp
```

### Zsh manual setup

Create `~/.zsh/completions/_erlmcp`:

```zsh
#compdef erlmcp

local -a commands
commands=(
  'init:Initialize local development environment'
  'start:Start local erlmcp cluster'
  'stop:Stop local erlmcp cluster'
  'status:Show cluster status'
  'doctor:Run host readiness diagnostics'
  'test-100k:Run 100K concurrent test'
  'benchmark:Run performance benchmarks'
  'help:Show help message'
)

_arguments "1: :(${(j:|:)${commands[@]%%:*}})"

local curcontext="$curcontext"
case $line[1] in
  (init|start|stop|status|doctor|test-100k|benchmark|help)
    # Command-specific completions can go here
    ;;
esac
```

Then reload:
```bash
source ~/.zsh/completions/_erlmcp
```

### Fish manual setup

Create `~/.config/fish/completions/erlmcp.fish`:

```fish
#!/usr/bin/env fish

complete -c erlmcp -n "__fish_use_subcommand_from_list; true" \
  -x -a "init" -d "Initialize local development environment"
complete -c erlmcp -n "__fish_use_subcommand_from_list; true" \
  -x -a "start" -d "Start local erlmcp cluster"
complete -c erlmcp -n "__fish_use_subcommand_from_list; true" \
  -x -a "stop" -d "Stop local erlmcp cluster"
complete -c erlmcp -n "__fish_use_subcommand_from_list; true" \
  -x -a "status" -d "Show cluster status"
complete -c erlmcp -n "__fish_use_subcommand_from_list; true" \
  -x -a "doctor" -d "Run host readiness diagnostics"
complete -c erlmcp -n "__fish_use_subcommand_from_list; true" \
  -x -a "test-100k" -d "Run 100K concurrent test"
complete -c erlmcp -n "__fish_use_subcommand_from_list; true" \
  -x -a "benchmark" -d "Run performance benchmarks"
complete -c erlmcp -n "__fish_use_subcommand_from_list; true" \
  -x -a "help" -d "Show help message"
```

---

## Dynamic Completion Generation

### Generate completions on the fly

```bash
#!/bin/bash
# save as ~/.local/bin/erlmcp-generate-completions

# Bash
erlmcp help | grep -oP '^\s+\K\w+(?=\s)' > /tmp/erlmcp-commands

# Zsh
erlmcp help | awk '/^  [a-z]/ {printf "%s:%s\n", $1, $3}' > /tmp/erlmcp-zsh-commands

# Fish
erlmcp help | awk '/^  [a-z]/ {printf "complete -c erlmcp -x -a \"%s\" -d \"%s\"\n", $1, $3}' > /tmp/erlmcp-fish-commands
```

Run once to generate:
```bash
~/.local/bin/erlmcp-generate-completions
```

---

## Troubleshooting

### Completions not showing up

**Problem**: Typing `erlmcp [TAB]` doesn't show completions

**Solutions** (in order):

1. **Verify completion script exists**:
   ```bash
   # Bash
   ls -la ~/.bash_completion.d/erlmcp

   # Zsh
   ls -la ~/.zsh/completions/_erlmcp

   # Fish
   ls -la ~/.config/fish/completions/erlmcp.fish
   ```

2. **Verify script is sourced**:
   ```bash
   # Bash
   echo $BASH_COMPLETION_DIR
   grep -r erlmcp ~/.bashrc

   # Zsh
   echo $fpath
   grep -r erlmcp ~/.zshrc

   # Fish
   set -e | grep erlmcp
   ```

3. **Reload shell**:
   ```bash
   # Bash
   exec bash
   source ~/.bashrc

   # Zsh
   exec zsh
   source ~/.zshrc

   # Fish
   exec fish
   ```

4. **Check PATH includes erlmcp**:
   ```bash
   which erlmcp
   ```
   If not found, add to PATH:
   ```bash
   export PATH="/path/to/erlmcp/bin:$PATH"
   ```

### Completions appear but are incomplete

**Problem**: Completion shows only some commands or has errors

**Solution**: Verify completion script is valid:

```bash
# Bash
bash -n ~/.bash_completion.d/erlmcp

# Zsh
zsh -n ~/.zsh/completions/_erlmcp

# Fish
fish -n ~/.config/fish/completions/erlmcp.fish
```

Fix any syntax errors shown.

### erlmcp command not found after installing completions

**Problem**: Completions install but `erlmcp` command itself doesn't work

**Solution**: Add erlmcp to PATH:

```bash
# Check current PATH
echo $PATH

# Add to PATH
export PATH="/path/to/erlmcp/bin:$PATH"

# Make permanent (add to shell config)
echo 'export PATH="/path/to/erlmcp/bin:$PATH"' >> ~/.bashrc  # or ~/.zshrc for zsh
source ~/.bashrc
```

### Completion script is outdated

**Problem**: Completions don't include new commands added to erlmcp

**Solution**: Regenerate completions:

```bash
# Remove old completions
rm ~/.bash_completion.d/erlmcp      # Bash
rm ~/.zsh/completions/_erlmcp       # Zsh
rm ~/.config/fish/completions/erlmcp.fish  # Fish

# Copy fresh ones from repo
cp /path/to/erlmcp/completions/erlmcp.bash ~/.bash_completion.d/
cp /path/to/erlmcp/completions/erlmcp.zsh ~/.zsh/completions/_erlmcp
cp /path/to/erlmcp/completions/erlmcp.fish ~/.config/fish/completions/

# Reload shell
exec bash  # or exec zsh or exec fish
```

## Creating Custom Completions

### Adding new commands to completion

If you extend the CLI with custom commands, update the completion scripts:

**Bash** (`completions/erlmcp.bash`):
```bash
opts="init start stop status doctor test-100k benchmark help mycommand"
```

**Zsh** (`completions/erlmcp.zsh`):
```zsh
commands=(
  'mycommand:Description of mycommand'
)
```

**Fish** (`completions/erlmcp.fish`):
```fish
complete -c erlmcp -x -a "mycommand" -d "Description of mycommand"
```

---

## Testing Completions

### Verify completions work

```bash
# Test bash
bash
erlmcp [TAB]          # Should show commands

# Test zsh
zsh
erlmcp [TAB]          # Should show commands with descriptions

# Test fish
fish
erlmcp [TAB]          # Should show commands with descriptions
```

### Test partial completion

```bash
# Should work in all shells
erlmcp te[TAB]        # Should complete to test-100k
erlmcp be[TAB]        # Should complete to benchmark
erlmcp do[TAB]        # Should complete to doctor
```

---

## Platform-Specific Notes

### macOS

If using Homebrew:
```bash
# Bash (with bash-completion@2)
brew install bash-completion@2

# Zsh (with zsh-completions)
brew install zsh-completions
```

### Linux (Ubuntu/Debian)

```bash
# Bash
sudo apt-get install bash-completion

# Zsh
sudo apt-get install zsh zsh-completions

# Fish
sudo apt-get install fish
```

### Windows (using WSL or Git Bash)

Completions work in WSL using Linux instructions above.

For Git Bash, the bash completion approach should work.

---

## See Also

- [CLI_REFERENCE.md](CLI_REFERENCE.md) - CLI command reference
- [CLI_INTERACTIVE_GUIDE.md](CLI_INTERACTIVE_GUIDE.md) - Interactive REPL guide
- [Bash Completion Official Docs](https://www.gnu.org/software/bash/manual/html_node/Programmable-Completion.html)
- [Zsh Completion Docs](http://zsh.sourceforge.net/Doc/Release/Completion-System.html)
- [Fish Completion Docs](https://fishshell.com/docs/current/completions.html)
