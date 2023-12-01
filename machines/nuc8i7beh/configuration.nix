# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, nixpkgs, ... }:
{
  imports =
    [
      # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Yubikey
  services.udev.packages = [ pkgs.yubikey-personalization pkgs.ledger-udev-rules ];
  services.pcscd.enable = true;

  # For sudo/login with Yubikey
  security.pam.yubico = {
    enable = true;
    debug = false;
    mode = "challenge-response";
    control = "required";
  };

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
  boot.extraModulePackages = [
    config.boot.kernelPackages.v4l2loopback
  ];
  # Set initial kernel module settings
  boot.extraModprobeConfig = ''
    # exclusive_caps: Skype, Zoom, Teams etc. will only show device when actually streaming
    # card_label: Name of virtual camera, how it'll show up in Skype, Zoom, Teams
    # https://github.com/umlaeute/v4l2loopback
    options v4l2loopback exclusive_caps=1 card_label="Virtual Camera"
  '';
  boot.initrd.luks.devices.crypted.device = "/dev/disk/by-uuid/fb8ed389-a834-48e6-af5e-9dfbc4724490";
  fileSystems."/home".device = "/dev/mapper/crypted";

  nix.settings = {
    trusted-users = [ "root" "sibi" ];
    experimental-features = [ "nix-command" "flakes" ];
  };

  nix.registry.nixpkgs.flake = nixpkgs;

  networking.hostName = "arya"; # Define your hostname.
  # networking.wireless.enable = true;
  # networking.wireless.userControlled.enable = true;
  # networking.networkmanager.wifi.backend = "iwd";
  networking.networkmanager.enable = true;
  networking.networkmanager.wifi.powersave = false;

  # Set your time zone.
  time.timeZone = "Asia/Kolkata";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.eno1.useDHCP = true;
  networking.interfaces.wlp0s20f3.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  services.earlyoom.enable = true;

  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
  };

  services.pipewire.enable = true;
  xdg = {
    portal = {
      enable = true;
      extraPortals = with pkgs; [
        xdg-desktop-portal-wlr
        xdg-desktop-portal-gtk
      ];
    };
  };

  services.dbus.enable = true;

  # Enable sound.
  sound.enable = false;
  hardware.pulseaudio.enable = false;

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
      "libvirtd"
      "plugdev"
    ];
  };

  virtualisation.docker.enable = true;
  virtualisation.libvirtd.enable = true;

  environment.systemPackages = with pkgs; [
    yubico-pam
    yubikey-manager
    virt-manager
    virtiofsd
    google-chrome
  ];

  fonts.packages = with pkgs; [
    ubuntu_font_family
    font-awesome
    symbola
    alegreya
    nerdfonts
  ];

  security.sudo.enable = false;
  security.sudo-rs.enable = true;

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

  # security.pam.services.xmonad.gnupg = { enable = true; };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # For disk automount
  security.polkit.enable = true;
  services.udisks2.enable = true;
  services.devmon.enable = true;

  services.fwupd.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?

}
