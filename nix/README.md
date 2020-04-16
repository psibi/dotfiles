# Installation

```
$ nix-env -i -f ./packages.nix
```

# Generations

``` shellsession
$ nix-env --list-generations
$ nix-env --rollback
$ nix-env -G 3  # Switch to generation 3
```

## REPL

``` markdown
$ nix repl
nix-repl> x = import <nixpkgs> {}
nix-repl> x.pkgs.hello
«derivation /nix/store/dhmin7wq99aw9f59jm41varj0753va9b-hello-2.10.drv»
nix-repl> x.haskellPackages.xmonad-contrib
«derivation /nix/store/vq4j99s88yxny8mg1s48pv2k0d1l51ma-xmonad-contrib-0.15.drv»
```
