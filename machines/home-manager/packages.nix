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
  hunspell
  hunspellDicts.en_US-large
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
  fish
  bat
  lsd
  fd
  procs
  dysk
  nixfmt
  direnv
  rustup
  pavucontrol
  gnumake
  gcc
  llvm
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
  openssl.dev
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
  cabal-install
  ghc
  argo-rollouts
  haskell-language-server
  paprefs
  grpcurl
  plantuml
  zola
  zlib
  zlib.dev
  postgresql
  jl
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
  hwatch
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
  # jetbrains.idea-community
  dateutils

  # Sway and Wayland related
  wayland
  xdg-utils
  glib
  swaylock
  swayidle
  grim # Screenshot
  wl-clipboard
  wdisplays
  rofi-wayland
  xdg-desktop-portal
  xdg-desktop-portal-wlr
  wlprop
  wev
  i3status-rust
  wtype
  xorg.xlsclients
  gnome.adwaita-icon-theme
  alsa-utils

  slurp
  libcamera
  sass

  # nodePackages.yaml-language-server
  # https://github.com/redhat-developer/yaml-language-server/issues/881
  sibi-yaml-language-server
  nodePackages.vscode-json-languageserver
  nodePackages.dockerfile-language-server-nodejs
  nodePackages.bash-language-server
  nodejs
  nodePackages.typescript
  nodePackages.typescript-language-server
  haskellPackages.implicit-hie
  python311Packages.python-lsp-server
  poetry
  kooha
  musl.dev
  glibc.static
  awscli2
  vscode
  yarn

  unstable.google-chrome
  unstable.terraform-ls
  zoom-us

  # From Overlays
  em
  flarectl
  sibiEmacs

  libusb1
  libusb1.dev
  libusb1.out

  go_1_19
  gopls
  golangci-lint
]
