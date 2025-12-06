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
  rxvt-unicode-unwrapped
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
  nixfmt-rfc-style
  direnv
  pavucontrol
  gnumake
  gcc
  # llvm
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
  pandoc
  killall
  starship
  tfswitch
  jq
  dnsutils
  fzf
  broot
  dust
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
  # httplz Disable since cache is not there
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
  mdbook-toc
  cargo-spellcheck
  cargo-edit
  cargo-outdated
  cargo-expand
  cargo-generate
  cargo-cache
  cargo-udeps
  #cargo-llvm-cov
  cargo-watch
  cargo-binstall
  cargo-leptos
  cargo-nextest
  cargo-cross
  cargo-autoinherit
  cargo-machete
  keepassxc
  cabal-install
  ghc
  argo-rollouts
  haskell-language-server
  paprefs
  grpcurl
  plantuml
  zola
  zlib.out
  zlib.dev
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
  tokei
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
  rofi
  xdg-desktop-portal
  xdg-desktop-portal-wlr
  wlprop
  rofi
  wev
  i3status-rust
  wtype
  xorg.xlsclients
  adwaita-icon-theme
  alsa-utils

  slurp
  libcamera
  sass

  # nodePackages.yaml-language-server
  # https://github.com/redhat-developer/yaml-language-server/issues
  # sibi-yaml-language-server
  #nodePackages.vscode-json-languageserver
  dockerfile-language-server
  nodePackages.bash-language-server
  nodejs_20
  nodePackages.typescript
  nodePackages.typescript-language-server
  haskellPackages.implicit-hie
  haskellPackages.cabal-gild
  python311Packages.python-lsp-server
  poetry
  kooha
  musl.dev # Causes issue to haskell stack
  glibc.static
  awscli2
  vscode
  yarn
  aws-vault

  google-chrome
  chromium
  just

  terraform-ls
  kubernetes-helm
  rustup
  zoom-us

  # From Overlays
  em
  flarectl
  sibiEmacs

  libusb1
  libusb1.dev
  libusb1.out # for kolme
  systemd.dev # for kolme

  go
  gopls
  golangci-lint

  cilium-cli
  hwinfo
  pciutils
  lshw

  ubuntu-classic
  font-awesome
  symbola
  alegreya
  nerd-fonts.symbols-only
  lm_sensors
  linux.dev
  virt-manager
  virtualbox
  quickemu

  tldr-hs
  whois

  sqlx-cli
  kondo # For cleaning build artifacts
  kubeseal
  google-cloud-sdk
  minijinja
  tflint
  trunk
  leptosfmt
  kubo # For ipfs
  wasm-pack
  biome
  hugo
  texliveMedium
  hurl
  # postgresql.dev
  pnpm
  prisma
  dbeaver-bin
  ghostty
  stack
  haskellPackages.cabal-fmt
  aider-chat-full
  claude-code
  postgresql
  # solana-cli # No cache
  # anchor No cache

  tombi
  mold
  gemini-cli
  wireguard-tools
  pprof
  graphviz
  websocat
  masterpdfeditor
  worker-build
  steam-run
  unstable.goose-cli
]
