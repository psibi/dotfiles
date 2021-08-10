source ~/github/dotfiles/.alias

set -g --export GPG_TTY (tty)

# Disable greeting
set fish_greeting

set PATH /home/sibi/bin /home/sibi/.cargo/bin $PATH

# https://github.com/fish-shell/fish-shell/issues/825
alias hr 'history --merge'  # read and merge history from disk

starship init fish | source
direnv hook fish | source
zoxide init fish | source
