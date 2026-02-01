# Fish completion for erlmcp-validate
# Installation: Copy to ~/.config/fish/completions/erlmcp-validate.fish

# Commands
complete -c erlmcp-validate -f -n '__fish_use_subcommand' -a 'validate' -d 'Validate a running MCP server'
complete -c erlmcp-validate -f -n '__fish_use_subcommand' -a 'spec-check' -d 'Check spec compliance'
complete -c erlmcp-validate -f -n '__fish_use_subcommand' -a 'transport-check' -d 'Verify transport behavior'
complete -c erlmcp-validate -f -n '__fish_use_subcommand' -a 'report' -d 'Generate compliance report'
complete -c erlmcp-validate -f -n '__fish_use_subcommand' -a 'spec' -d 'Validate against MCP spec'
complete -c erlmcp-validate -f -n '__fish_use_subcommand' -a 'protocol' -d 'Validate protocol message'
complete -c erlmcp-validate -f -n '__fish_use_subcommand' -a 'transport' -d 'Validate transport'
complete -c erlmcp-validate -f -n '__fish_use_subcommand' -a 'compliance' -d 'Full compliance suite'
complete -c erlmcp-validate -f -n '__fish_use_subcommand' -a 'all' -d 'Run all validators'
complete -c erlmcp-validate -f -n '__fish_use_subcommand' -a 'run' -d 'Run validation tests'
complete -c erlmcp-validate -f -n '__fish_use_subcommand' -a 'quick-check' -d 'Quick validation check'
complete -c erlmcp-validate -f -n '__fish_use_subcommand' -a 'status' -d 'Show validation status'
complete -c erlmcp-validate -f -n '__fish_use_subcommand' -a 'install-completions' -d 'Install shell completions'

# Global options
complete -c erlmcp-validate -l help -d 'Show help message'
complete -c erlmcp-validate -l version -d 'Show version'
complete -c erlmcp-validate -l gen-completions -d 'Generate completion script' -a 'bash zsh fish'

# validate command options
complete -c erlmcp-validate -f -n '__fish_seen_subcommand_from validate' -l format -d 'Output format' -a 'text json markdown'
complete -c erlmcp-validate -f -n '__fish_seen_subcommand_from validate' -l output -d 'Output file'
complete -c erlmcp-validate -f -n '__fish_seen_subcommand_from validate' -l verbose -d 'Verbose output'

# spec-check command options
complete -c erlmcp-validate -f -n '__fish_seen_subcommand_from spec-check' -l format -d 'Output format' -a 'text json markdown'
complete -c erlmcp-validate -f -n '__fish_seen_subcommand_from spec-check' -l output -d 'Output file'
complete -c erlmcp-validate -f -n '__fish_seen_subcommand_from spec-check' -l verbose -d 'Verbose output'

# transport-check command options
complete -c erlmcp-validate -f -n '__fish_seen_subcommand_from transport-check; and not __fish_seen_subcommand_from stdio tcp http websocket' -a 'stdio tcp http websocket' -d 'Transport name'
complete -c erlmcp-validate -f -n '__fish_seen_subcommand_from transport-check' -l format -d 'Output format' -a 'text json markdown'
complete -c erlmcp-validate -f -n '__fish_seen_subcommand_from transport-check' -l output -d 'Output file'
complete -c erlmcp-validate -f -n '__fish_seen_subcommand_from transport-check' -l verbose -d 'Verbose output'

# report command options
complete -c erlmcp-validate -f -n '__fish_seen_subcommand_from report' -l format -d 'Output format' -a 'text json markdown html'
complete -c erlmcp-validate -f -n '__fish_seen_subcommand_from report' -l output -d 'Output file'

# protocol command options
complete -c erlmcp-validate -f -n '__fish_seen_subcommand_from protocol' -l file -d 'Input file'
complete -c erlmcp-validate -f -n '__fish_seen_subcommand_from protocol' -l format -d 'Output format' -a 'text json markdown'

# transport command options
complete -c erlmcp-validate -f -n '__fish_seen_subcommand_from transport; and not __fish_seen_subcommand_from stdio tcp http websocket' -a 'stdio tcp http websocket' -d 'Transport name'
complete -c erlmcp-validate -f -n '__fish_seen_subcommand_from transport' -l format -d 'Output format' -a 'text json markdown'

# run command options
complete -c erlmcp-validate -f -n '__fish_seen_subcommand_from run' -l all -d 'Run all sections'
complete -c erlmcp-validate -f -n '__fish_seen_subcommand_from run' -l section -d 'Run section' -a 'protocol transport security error_handling performance'
complete -c erlmcp-validate -f -n '__fish_seen_subcommand_from run' -l transport -d 'Transport' -a 'stdio tcp http websocket'
complete -c erlmcp-validate -f -n '__fish_seen_subcommand_from run' -l format -d 'Output format' -a 'text json markdown'
complete -c erlmcp-validate -f -n '__fish_seen_subcommand_from run' -l verbose -d 'Verbose output'
complete -c erlmcp-validate -f -n '__fish_seen_subcommand_from run' -l quiet -d 'Quiet output'

# install-completions command options
complete -c erlmcp-validate -f -n '__fish_seen_subcommand_from install-completions' -a 'bash zsh fish' -d 'Shell type'
