PATH="$PATH:$HOME/opt"
PATH="$HOME/.local/bin:$PATH"

PATH="$HOME/.local/share/gem/ruby/3.0.0/bin:$PATH"

export FLYCTL_INSTALL="$HOME/.fly"
export PATH="$FLYCTL_INSTALL/bin:$PATH"

[ -f "/usr/share/nvm/init-nvm.sh" ] && source "/usr/share/nvm/init-nvm.sh"

[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env"

[ -f "$HOME/.cargo/env" ] && source "$HOME/.cargo/env"