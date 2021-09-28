# Symlink for NUC home-manager
home-nuc:
    rm $HOME/.config/nixpkgs/home.nix
    ln -v -s $PWD/machines/home-manager/nuc.nix $HOME/.config/nixpkgs/home.nix
