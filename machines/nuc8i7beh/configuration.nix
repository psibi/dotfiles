# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }: {
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  nixpkgs.config.allowUnfree = true;

  nix.trustedUsers = [ "root" "sibi" ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.grub.configurationLimit = 25;

  hardware.enableRedistributableFirmware = true;

  networking.hostName = "arya"; # Define your hostname.
  # networking.wireless.enable = true;
  # networking.wireless.userControlled.enable = true;
  # networking.networkmanager.wifi.backend = "iwd";
  networking.networkmanager.enable = true;
  networking.networkmanager.wifi.powersave = true;

  # Set your time zone.
  time.timeZone = "Asia/Kolkata";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.eno1.useDHCP = true;
  networking.interfaces.wlp0s20f3.useDHCP = true;

  fileSystems."/home" = {
    device = "/dev/disk/by-label/home";
    fsType = "ext4";
  };

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  # services.xserver.desktopManager = {
  #  xfce.enable = true;
  # };

  services.xserver.windowManager.xmonad.enable = true;
  services.xserver.windowManager.xmonad.enableContribAndExtras = true;
  services.xserver.windowManager.xmonad.config =
    /home/sibi/github/dotfiles/xmonad/xmonad.hs;

  services.xserver.windowManager.xmonad.haskellPackages =
    pkgs.haskellPackages.override {
      overrides = haskellPackagesNew: haskellPackagesOld: rec {
        libmpd = haskellPackagesNew.callPackage
          /home/sibi/github/dotfiles/nix/overrides/libmpd.nix { };
        X11 = haskellPackagesNew.callPackage
          /home/sibi/github/dotfiles/nix/overrides/x11.nix { };
        xmonad = let
          pkg = haskellPackagesNew.callPackage
            /home/sibi/github/dotfiles/nix/overrides/xmonad.nix { };
        in pkgs.haskell.lib.dontCheck pkg;
        xmonad-extras = haskellPackagesNew.callPackage
          /home/sibi/github/dotfiles/nix/overrides/xmonad-extras.nix { };
        xmonad-contrib = let
          pkg = haskellPackagesNew.callPackage
            /home/sibi/github/dotfiles/nix/overrides/xmonad-contrib.nix { };
        in pkgs.haskell.lib.dontCheck pkg;
        timezone-olson = haskellPackagesNew.callPackage
          /home/sibi/github/dotfiles/nix/overrides/timezone-olson.nix { };
      };
    };

  # Configure keymap in X11
  services.xserver.layout = "us";
  services.xserver.xkbOptions = "caps:ctrl_modifier";

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.sibi = {
    isNormalUser = true;
    extraGroups = [
      "wheel"
      "audio"
      "sound"
      "video"
      "docker"
      "networkmanager"
    ]; # Enable ‘sudo’ for the user.
  };

  virtualisation.docker.enable = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    # vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    wget
    curl
    firefox
    rxvt_unicode
    git
    emacs
    unzip
    keepassxc
    sqlite
    alacritty
    google-chrome
    gnucash
    screen
    xclip
    xsel
    xdotool
    xscreensaver
    fish
    bat
    unrar
    exa
    fd
    procs
    nixfmt
    direnv
    gitFull
    rustup
    stack
    pavucontrol
    gnumake
    gcc
    llvm
    xorg.libxcb
    pinentry
    pinentry-emacs
    pinentry-curses
    feh
    htop
    cabal2nix
    lsof
    ripgrep
    ripgrep-all
    rust-analyzer
    docker
    cachix
    tree
    nix-prefetch-git
    nix-prefetch-github
    # texlive.combined.scheme-full
    sage
    python3Minimal
    python39Packages.pygments
    networkmanager
    ormolu
    direnv
    awscli2
    azure-cli
    keybase-gui
  ];

  fonts.fonts = with pkgs; [ ubuntu_font_family font-awesome symbola alegreya ];

  environment.homeBinInPath = true;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  security.pam.services.xmonad.gnupg = { enable = true; };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.openssh.knownHosts = {
    elric = {
      publicKey = (import ../passwords.nix).ssh_key.personalPublicKey;
    };
  };

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?

}
