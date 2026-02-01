#!/usr/bin/env bash
# Installation script for erlmcp-validate shell completions

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
SHELL_TYPE="${1:-auto}"

detect_shell() {
    if [ "$SHELL_TYPE" != "auto" ]; then
        echo "$SHELL_TYPE"
        return
    fi

    # Auto-detect shell
    if [ -n "$BASH_VERSION" ]; then
        echo "bash"
    elif [ -n "$ZSH_VERSION" ]; then
        echo "zsh"
    elif [ -n "$FISH_VERSION" ]; then
        echo "fish"
    else
        # Fallback to $SHELL environment variable
        case "$SHELL" in
            */bash)
                echo "bash"
                ;;
            */zsh)
                echo "zsh"
                ;;
            */fish)
                echo "fish"
                ;;
            *)
                echo "unknown"
                ;;
        esac
    fi
}

install_bash_completion() {
    echo "Installing bash completion..."

    # Try multiple locations
    if [ -d "$HOME/.bash_completion.d" ]; then
        DEST="$HOME/.bash_completion.d/erlmcp-validate"
    elif [ -d "/usr/local/etc/bash_completion.d" ]; then
        DEST="/usr/local/etc/bash_completion.d/erlmcp-validate"
    else
        # Create user directory
        mkdir -p "$HOME/.bash_completion.d"
        DEST="$HOME/.bash_completion.d/erlmcp-validate"
    fi

    cp "$SCRIPT_DIR/erlmcp_completion.bash" "$DEST"
    echo "✓ Installed to: $DEST"
    echo ""
    echo "To activate, add this to your ~/.bashrc:"
    echo "  source $DEST"
    echo ""
    echo "Or reload your shell:"
    echo "  source ~/.bashrc"
}

install_zsh_completion() {
    echo "Installing zsh completion..."

    # Create completion directory if needed
    COMP_DIR="$HOME/.zsh/completion"
    mkdir -p "$COMP_DIR"

    DEST="$COMP_DIR/_erlmcp-validate"
    cp "$SCRIPT_DIR/erlmcp_completion.zsh" "$DEST"
    echo "✓ Installed to: $DEST"
    echo ""
    echo "To activate, add this to your ~/.zshrc (if not already present):"
    echo "  fpath=($COMP_DIR \$fpath)"
    echo "  autoload -U compinit && compinit"
    echo ""
    echo "Then reload your shell:"
    echo "  source ~/.zshrc"
}

install_fish_completion() {
    echo "Installing fish completion..."

    COMP_DIR="$HOME/.config/fish/completions"
    mkdir -p "$COMP_DIR"

    DEST="$COMP_DIR/erlmcp-validate.fish"
    cp "$SCRIPT_DIR/erlmcp_completion.fish" "$DEST"
    echo "✓ Installed to: $DEST"
    echo ""
    echo "Fish completions are automatically loaded."
    echo "Reload completions with:"
    echo "  fish_update_completions"
}

main() {
    DETECTED_SHELL=$(detect_shell)

    case "$DETECTED_SHELL" in
        bash)
            install_bash_completion
            ;;
        zsh)
            install_zsh_completion
            ;;
        fish)
            install_fish_completion
            ;;
        unknown)
            echo "Error: Could not detect shell type."
            echo "Please specify shell explicitly:"
            echo "  $0 bash|zsh|fish"
            exit 1
            ;;
        *)
            echo "Error: Unsupported shell: $DETECTED_SHELL"
            exit 1
            ;;
    esac

    echo ""
    echo "Installation complete!"
}

main
