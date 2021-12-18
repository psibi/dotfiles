source ~/github/dotfiles/.alias

# Disable greeting
set fish_greeting

# set PATH /home/sibi/.cargo/bin $PATH

# https://github.com/fish-shell/fish-shell/issues/825
alias hr 'history --merge'  # read and merge history from disk

starship init fish | source
direnv hook fish | source
zoxide init fish | source
any-nix-shell fish --info-right | source
