{ pkgs, unstable }:
with pkgs; [
  git
  terraform-docs
  ouch
  unzip
  tgswitch
  file
  aspell
  aspellDicts.en
  hunspell
  hunspellDicts.en_US-large
  rxvt-unicode-unwrapped
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
  azure-cli
  hlint
  aws-iam-authenticator
  kustomize
  ipcalc
  bc
  any-nix-shell
  libreoffice
  openssl
  openssl.dev
  wirelesstools
  ifmetric
  tree-sitter
  tree-sitter-grammars.tree-sitter-rust
  watchexec
  jwt-cli
  # httplz
  miniserve # Altearnative to httplz
  twitter-color-emoji
  kalker
  jless
  nixpkgs-fmt
  pinentry-qt
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
  mdbook-mermaid
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
  haskell-language-server
  paprefs
  grpcurl
  zola
  zlib.out
  zlib.dev
  yubikey-manager
  yubioath-flutter
  yubico-pam
  flex
  bison
  ccls
  hwatch
  nil
  ormolu
  emacs-all-the-icons-fonts
  pdftk
  xfce.thunar
  xfce.thunar-volman
  gparted
  parallel
  jdk
  jdt-language-server
  maven
  gradle
  cmake
  ninja
  pkg-config
  usbutils
  marksman
  amber-secret
  appimage-run
  scc
  tokei
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
  vscode-json-languageserver
  dockerfile-language-server
  # nodejs_20 # Manage through fnm
  nodePackages.typescript
  nodePackages.typescript-language-server
  kooha
  musl.dev # Causes issue to haskell stack.
  glibc.static
  awscli2
  vscode
  aws-vault

  google-chrome
  chromium
  just

  terraform-ls
  kubernetes-helm
  rustup

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

  tldr-hs
  whois

  sqlx-cli
  kondo # For cleaning build artifacts
  kubeseal
  minijinja
  tflint
  hurl
  postgresql.dev
  postgresql.lib
  postgresql
  pnpm
  ghostty
  # stack
  claude-code
  postgresql
  tombi
  # mold # Interferes with stack
  websocat
  steam-run
  k3s
  nftables
  nmap
  rustscan
  libgourou
  calibre
  kubectl-cnpg
  barman
  net-tools
  kind
  opencode
  just-lsp
  biff
  cargo-insta
  fnm
  sibi-dpm
  unixtools.xxd
  metals
  scala
  ty
  # unstable.codex
]
