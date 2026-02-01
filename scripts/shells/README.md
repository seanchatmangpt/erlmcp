# Shell Completions for erlmcp-validate

This directory contains shell completion scripts for the `erlmcp-validate` CLI tool.

## Supported Shells

- **Bash** - Bourne Again SHell
- **Zsh** - Z Shell
- **Fish** - Friendly Interactive SHell

## Quick Installation

### Automatic Installation

Use the CLI's built-in installer (auto-detects your shell):

```bash
erlmcp-validate install-completions
```

Or specify a shell explicitly:

```bash
erlmcp-validate install-completions bash
erlmcp-validate install-completions zsh
erlmcp-validate install-completions fish
```

### Manual Installation

#### Bash

1. Copy the completion script:
   ```bash
   mkdir -p ~/.bash_completion.d
   cp erlmcp_completion.bash ~/.bash_completion.d/erlmcp-validate
   ```

2. Add to your `~/.bashrc`:
   ```bash
   source ~/.bash_completion.d/erlmcp-validate
   ```

3. Reload your shell:
   ```bash
   source ~/.bashrc
   ```

#### Zsh

1. Copy the completion script:
   ```bash
   mkdir -p ~/.zsh/completion
   cp erlmcp_completion.zsh ~/.zsh/completion/_erlmcp-validate
   ```

2. Add to your `~/.zshrc` (if not already present):
   ```bash
   fpath=(~/.zsh/completion $fpath)
   autoload -U compinit && compinit
   ```

3. Reload your shell:
   ```bash
   source ~/.zshrc
   ```

#### Fish

1. Copy the completion script:
   ```bash
   mkdir -p ~/.config/fish/completions
   cp erlmcp_completion.fish ~/.config/fish/completions/erlmcp-validate.fish
   ```

2. Reload completions:
   ```bash
   fish_update_completions
   ```

## Generate Completions Dynamically

You can also generate completion scripts on-the-fly:

```bash
# Generate bash completion
erlmcp-validate --gen-completions bash > /tmp/completion.bash
source /tmp/completion.bash

# Generate zsh completion
erlmcp-validate --gen-completions zsh > ~/.zsh/completion/_erlmcp-validate

# Generate fish completion
erlmcp-validate --gen-completions fish > ~/.config/fish/completions/erlmcp-validate.fish
```

## Usage

Once installed, you can use TAB completion for:

### Commands
```bash
erlmcp-validate <TAB>
# Shows: validate, spec-check, transport-check, report, etc.
```

### Command Options
```bash
erlmcp-validate validate <TAB>
# Shows: stdio://, tcp://, http://, https://, --format, --output, --verbose

erlmcp-validate transport-check <TAB>
# Shows: stdio, tcp, http, websocket
```

### Format Types
```bash
erlmcp-validate report --format <TAB>
# Shows: text, json, markdown, html
```

### File Paths
```bash
erlmcp-validate protocol --file <TAB>
# Shows: file browser
```

## Smart Suggestions

The CLI also provides smart suggestions for typos:

```bash
$ erlmcp-validate valdate
Unknown command: 'valdate'. Did you mean 'validate'?

$ erlmcp-validate tranpsort-check
Unknown command: 'tranpsort-check'. Did you mean 'transport-check'?
```

## Features

- **Context-aware completions** - Suggests appropriate options based on command
- **Transport completion** - Lists available transports (stdio, tcp, http, websocket)
- **Format completion** - Lists available formats (text, json, markdown, html)
- **Section completion** - Lists validation sections (protocol, transport, security, etc.)
- **File path completion** - Browser for --file and --output options
- **Smart typo detection** - Suggests similar commands using Levenshtein distance

## Programmatic API

If you're integrating with the CLI from Erlang code:

```erlang
%% Generate bash completion script
{ok, BashScript} = erlmcp_cli_completer:generate(bash).

%% Get available commands
Commands = erlmcp_cli_completer:available_commands().

%% Get command options
Opts = erlmcp_cli_completer:command_options(validate).

%% Suggest commands for typos
Suggestions = erlmcp_cli_suggester:suggest("valdate").
%% => [<<"validate">>]

%% Format suggestion message
Message = erlmcp_cli_suggester:format_suggestion("valdate", Suggestions).
%% => <<"Unknown command: 'valdate'. Did you mean 'validate'?">>
```

## Troubleshooting

### Completions not working in Bash

1. Check if bash-completion is installed:
   ```bash
   brew install bash-completion  # macOS
   apt-get install bash-completion  # Debian/Ubuntu
   ```

2. Verify the script is sourced:
   ```bash
   grep "erlmcp-validate" ~/.bashrc
   ```

### Completions not working in Zsh

1. Check fpath:
   ```zsh
   echo $fpath
   ```

2. Verify compinit is loaded:
   ```zsh
   grep "compinit" ~/.zshrc
   ```

3. Rebuild completion cache:
   ```zsh
   rm ~/.zcompdump*
   compinit
   ```

### Completions not working in Fish

1. Check completions directory:
   ```fish
   ls ~/.config/fish/completions/
   ```

2. Verify the file has correct permissions:
   ```fish
   chmod 644 ~/.config/fish/completions/erlmcp-validate.fish
   ```

## Development

To modify or extend completions:

1. Edit the appropriate module:
   - `/apps/erlmcp_validation/src/erlmcp_cli_completer.erl` - Generation logic
   - `/apps/erlmcp_validation/src/erlmcp_cli_suggester.erl` - Suggestion logic

2. Regenerate scripts:
   ```bash
   erlmcp-validate --gen-completions bash > scripts/shells/erlmcp_completion.bash
   erlmcp-validate --gen-completions zsh > scripts/shells/erlmcp_completion.zsh
   erlmcp-validate --gen-completions fish > scripts/shells/erlmcp_completion.fish
   ```

3. Test changes:
   ```bash
   source scripts/shells/erlmcp_completion.bash
   erlmcp-validate <TAB>
   ```

## See Also

- [erlmcp CLI Documentation](../../apps/erlmcp_validation/src/erlmcp_validate_cli.erl)
- [Bash Completion Documentation](https://github.com/scop/bash-completion)
- [Zsh Completion Documentation](http://zsh.sourceforge.net/Doc/Release/Completion-System.html)
- [Fish Completion Documentation](https://fishshell.com/docs/current/completions.html)
