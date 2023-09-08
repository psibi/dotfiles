{ pkgs, unstable }:
with pkgs; [
  git
  terraform-docs
  ouch
  unzip
  dogdns
  tgswitch
  file
  aspell
  aspellDicts.en
  rxvt_unicode
  keybase-gui
  keybase
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
  lsd
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
  just
  tfswitch
  jq
  dnsutils
  fzf
  broot
  du-dust
  azure-cli
  hlint
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
  man-pages
  man-pages-posix
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
  cargo-generate
  cargo-cache
  cargo-udeps
  cargo-llvm-cov
  keepassxc
  stack
  ghc
  argo-rollouts
  haskell-language-server
  paprefs
  sqlx-cli
  grpcurl
  plantuml
  zola
  zlib
  zlib.dev
  # zlib.out
  # zlib.static
  postgresql
  jl
  krew
  kubectl
  yubikey-manager
  yubioath-flutter
  yubico-pam
  flex
  bison
  ccls
  mprocs
  velero
  etcd_3_5
  awscli2
  hwatch
  sibiEmacs
  taplo
  nil
  ormolu
  emacs-all-the-icons-fonts
  kube-capacity
  pdftk
  xfce.thunar
  xfce.thunar-volman
  gparted
  parallel
  jdk
  jdt-language-server
  maven
  gradle
  flutter
  cmake
  ninja
  pkg-config
  usbutils
  eww
  marksman
  amber-secret
  nil
  appimage-run
  gopls
  scc

  nodePackages.yaml-language-server
  nodePackages.vscode-json-languageserver
  nodePackages.dockerfile-language-server-nodejs
  nodejs
  nodePackages.typescript
  nodePackages.typescript-language-server
  haskellPackages.implicit-hie

  # Unfree
  unstable.google-chrome
  vscode

  # From Overlays
  em
  terraform-ls
]
