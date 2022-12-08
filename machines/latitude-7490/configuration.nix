# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    <nixos-hardware/dell/latitude/7490>
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  nixpkgs.config.packageOverrides = pkgs: rec {
    tfswitch = pkgs.callPackage ../packages/tfswitch/default.nix { };
    ouch = pkgs.callPackage ../packages/ouch/default.nix { };
  };

  services.udev.packages = [ pkgs.yubikey-personalization ];
  services.pcscd.enable = true;

  nixpkgs.config.allowUnfree = true;
  hardware.enableRedistributableFirmware = true;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelModules = [
    # Virtual Camera
    "v4l2loopback"
    # Virtual Microphone, built-in
    "snd-aloop"
  ];
  boot.extraModulePackages = [ config.boot.kernelPackages.v4l2loopback ];
  # Set initial kernel module settings
  boot.extraModprobeConfig = ''
    # exclusive_caps: Skype, Zoom, Teams etc. will only show device when actually streaming
    # card_label: Name of virtual camera, how it'll show up in Skype, Zoom, Teams
    # https://github.com/umlaeute/v4l2loopback
    options v4l2loopback exclusive_caps=1 card_label="Virtual Camera"
  '';

  boot.loader.grub = {
    device = "/dev/disk/by-id/label/boot";
    configurationLimit = 70;
  };

  nix.settings.trusted-users = [ "root" "sibi" ];

  networking.hostName = "elric"; # Define your hostname.
  networking.networkmanager.enable = true;
  # networking.networkmanager.wifi.powersave = false;
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  # networking.wireless.userControlled.enable  = true;
  # networking.wireless.interfaces = ["wlp2s0"];
  # networking.wireless.networks = {
  #  dd-wrt_5 = {
  #    pskRaw = (import ./passwords.nix).wifi.ddwrt_pskRaw;
  #    priority = 1;
  #  };
  #  ichigokurasaki_5hz = {
  #    pskRaw = (import ./passwords.nix).wifi.ichigokurasaki_pskRaw;
  #    priority = 3;
  #  };
  #  ACT_TP_Link_2Gz = {
  #    pskRaw = (import ./passwords.nix).wifi.ganesh_house.act_tp_link_2Gz;
  #  };
  #  ACT_TP_Link_5Gz = {
  #    pskRaw = (import ./passwords.nix).wifi.ganesh_house.act_tp_link_5Gz;
  #  };
  # };

  # Set your time zone.
  time.timeZone = "Asia/Kolkata";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp0s31f6.useDHCP = true;
  networking.interfaces.wlp2s0.useDHCP = true;
  networking.interfaces.wwp0s20f0u2i12.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # Enable the X11 windowing system.
  services.earlyoom.enable = true;
  services.xserver.enable = true;
  #services.xserver.desktopManager.xterm.enable = false;
  # services.xserver.desktopManager = {
  #  xfce.enable = true;
  # };
  # location.provider = "geoclue2";
  # services.geoclue2.enable = true;
  # services.redshift.enable = true;
  # services.redshift.brightness = {
  #   day = "0.6";
  #   night = "0.8";
  # };

  # Configure keymap in X11
  services.xserver.layout = "us";
  services.xserver.xkbOptions = "caps:ctrl_modifier";

  services.xserver.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = /home/sibi/github/dotfiles/xmonad/xmonad.hs;
    extraPackages = self: [ self.typed-process self.utf8-string ];
  };

  #services.xserver.windowManager.xmonad.enable = true;
  #services.xserver.windowManager.xmonad.enableContribAndExtras = true;
  #services.xserver.windowManager.xmonad.config = /home/sibi/github/dotfiles/xmonad/xmonad.hs;
  #services.xserver.windowManager.xmonad.extraPackages = hpkgs: [
  #      hpkgs.typed-process
  #	hpkgs.utf8-string
  #        hpkgs.containers];

  # For laptop touchpad
  services.xserver.libinput.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.jane = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  };
  users.users.sibi = {
    isNormalUser = true;
    extraGroups = [
      "wheel"
      "audio"
      "sound"
      "video"
      "docker"
    ]; # Enable ‘sudo’ for the user.
  };

  virtualisation.docker.enable = true;

  environment.shellAliases = import ./alias.nix;
  programs.fish.shellAliases = import ./alias.nix;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs;
    [
      #   vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    ];

  fonts.fonts = with pkgs; [
    ubuntu_font_family
    font-awesome
    symbola
    alegreya
    nerdfonts
  ];

  environment.homeBinInPath = true;

  # This makes gnucash preferences work for a specific user
  # More details here: https://github.com/NixOS/nixpkgs/issues/168205
  programs.dconf.enable = true;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # security.pam.services.xmonad.gnupg = {
  #   enable = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.openssh.knownHosts = {
    nuc = { publicKey = (import ./passwords.nix).ssh_key.personalPublicKey; };
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
