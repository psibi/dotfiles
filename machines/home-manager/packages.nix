{ pkgs, unstable }:
with pkgs; [
  git
  terraform-docs
  ouch
  unzip
  # dogdns
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
  nixfmt-rfc-style
  dysk
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
  tree-sitter
  tree-sitter-grammars.tree-sitter-rust
  watchexec
  jwt-cli
  # httplz
  miniserve # Altearnative to httplz
  twitter-color-emoji
  kalker
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
  #yubioath-flutter
  yubico-pam
  flex
  bison
  ccls
  hwatch
  nil
  ormolu
  emacs-all-the-icons-fonts
  pdftk
  thunar
  thunar-volman
  gparted
  jdk
  jdt-language-server
  maven
  gradle
<<<<<<< HEAD
=======
#	  flutter
>>>>>>> origin/master
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
  xlsclients
  adwaita-icon-theme
  alsa-utils
  slurp
  libcamera
  sass
<<<<<<< HEAD
  vscode-json-languageserver
  dockerfile-language-server
  # nodejs_20 # Manage through fnm
  nodePackages.typescript
  nodePackages.typescript-language-server
=======

  # nodePackages.yaml-language-server
  # https://github.com/redhat-developer/yaml-language-server/issues/881
  # sibi-yaml-language-server
  vscode-json-languageserver
  dockerfile-language-server
  #nodePackages.bash-language-server
  # nodejs_20 # Manage through fnm
  #nodePackages.typescript
  #nodePackages.typescript-language-server
  haskellPackages.implicit-hie
  haskellPackages.cabal-gild
  # python311Packages.python-lsp-server - Trying ty now
  poetry
>>>>>>> origin/master
  kooha
  musl.dev # Causes issue to haskell stack.
  glibc.static
  awscli2
  vscode
<<<<<<< HEAD
=======
  # yarn
>>>>>>> origin/master
  aws-vault

  google-chrome
  chromium
  just

  terraform-ls
  kubernetes-helm
  rustup
<<<<<<< HEAD
=======
  #zoom-us
>>>>>>> origin/master

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
<<<<<<< HEAD
  minijinja
  tflint
=======
  #google-cloud-sdk
  minijinja
  tflint
  trunk
  leptosfmt
  #kubo # For ipfs
  #wasm-pack
  # biome
  # sibi-biome
  hugo
  #texliveMedium
>>>>>>> origin/master
  hurl
  postgresql.dev
  postgresql.lib
  postgresql
  pnpm
<<<<<<< HEAD
  ghostty
  # stack
  claude-code
  postgresql
  tombi
  # mold # Interferes with stack
  websocat
  steam-run
=======
  #prisma
  #dbeaver-bin
  ghostty
  # stack
  #haskellPackages.cabal-fmt
  #claude-code
  postgresql
  #unstable.solana-cli
  # anchor No cache

  tombi
  # mold # Interferes with stack
  #gemini-cli
  #wireguard-tools
  #pprof
  graphviz
  websocat
  #masterpdfeditor
  #worker-build
  steam-run
  #unstable.goose-cli
>>>>>>> origin/master
  k3s
  nftables
  nmap
  rustscan
  #libgourou
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
<<<<<<< HEAD
=======
  # aider-chat-full
  # sibi-aider
>>>>>>> origin/master
  sibi-dpm
  unixtools.xxd
  metals
  scala
  ty
  # unstable.codex
]
