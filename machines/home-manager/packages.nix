{ pkgs, unstable }:
with pkgs; [
  cnx-sibi
  git
  terraform-docs
  ouch
  dogdns
  tgswitch
  file
  aspell
  aspellDicts.en
  rxvt_unicode
  keybase-gui
  keybase
  amber-secret
  wget
  curl
  firefox
  git
  alacritty
  gnucash
  screen
  xclip
  xsel
  xdotool
  xscreensaver
  fish
  bat
  exa
  fd
  procs
  nixfmt
  direnv
  rustup
  pavucontrol
  gnumake
  gcc
  llvm
  xorg.libxcb
  sqlite
  htop
  cabal2nix
  lsof
  ripgrep
  docker
  tree
  nix-prefetch-git
  nix-prefetch-github
  sage
  python3Minimal
  python39Packages.pygments
  xfce.xfce4-screenshooter
  pandoc
  killall
  starship
  ouch
  just
  tfswitch
  jq
  dnsutils
  fzf
  broot
  du-dust
  azure-cli
  awscli2
  ormolu
  hlint
  stylish-haskell
  aws-iam-authenticator
  kustomize
  ipcalc
  bc
  any-nix-shell
  qpdf
  libreoffice
  openssl
  kubergrunt
  wirelesstools
  ifmetric
  tree-sitter
  tree-sitter-grammars.tree-sitter-rust
  watchexec
  jwt-cli
  step-cli
  httplz
  languagetool
  twitter-color-emoji
  kalker
  jless
  jfmt
  xorg.xwininfo
  nixpkgs-fmt
  kube-score
  pinentry-qt
  yq-go
  difftastic
  docker-compose
  shellcheck
  nodePackages.yaml-language-server
  man-pages
  man-pages-posix
  vscode
  gimp
  vokoscreen
  vlc
  ffmpeg
  mdbook
  texlive.combined.scheme-small
  cargo-spellcheck
  cargo-edit
  cargo-outdated
  cargo-expand
  keepassxc
  stack
  emacs28NativeComp
  argo-rollouts

  # From Overlays
  mprocs
  em
  terraform-ls

  unstable.google-chrome
  unstable.rust-analyzer
  # pinentry
  # pinentry-curses
  # texlive.combined.scheme-full
]
