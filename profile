PATH="$PATH:$HOME/opt"
PATH="$HOME/.local/bin:$PATH"

PATH="/home/martin/.local/share/gem/ruby/3.0.0/bin:$PATH"

source /usr/share/nvm/init-nvm.sh

export FLYCTL_INSTALL="/home/martin/.fly"
export PATH="$FLYCTL_INSTALL/bin:$PATH"

[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env"

[ -f "$HOME/.cargo/env" ] && source "$HOME/.cargo/env"
