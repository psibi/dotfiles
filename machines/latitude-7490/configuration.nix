# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, nixpkgs, pkgs, enableAnikalScanner, ... }:
{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  services.udev.packages = [ pkgs.yubikey-personalization pkgs.ledger-udev-rules ];
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

  nix.settings = {
    experimental-features = [ "nix-command" "flakes" ];
    trusted-users = [ "root" "sibi" ];
  };

  nix.registry.nixpkgs.flake = nixpkgs;

  networking.hostName = "elric"; # Define your hostname.
  networking.networkmanager.enable = true;
  # networking.networkmanager.wifi.powersave = false;
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  # networking.wireless.userControlled.enable  = true;

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

  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
  };

  services.dbus.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  services.pipewire.enable = true;
  services.pulseaudio.enable = false;

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
      "plugdev"
      "libvirtd"
    ] ++ (if enableAnikalScanner then [ "scanner" "lp" ] else []);
  };

  hardware.sane = {
    enable = enableAnikalScanner;
    extraBackends = if enableAnikalScanner then [ pkgs.hplipWithPlugin ] else [];
  };

  services.avahi.enable = enableAnikalScanner;
  services.avahi.nssmdns4 = enableAnikalScanner;

  virtualisation.docker.enable = true;
  virtualisation.libvirtd.enable = true;

  environment.shellAliases = import ./alias.nix;
  programs.fish.shellAliases = import ./alias.nix;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs;
    [
      virt-manager
      virtiofsd
    ];

  fonts.packages = with pkgs; [
    ubuntu_font_family
    font-awesome
    symbola
    alegreya
    nerd-fonts.ubuntu-mono
    nerd-fonts.ubuntu
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

  # security.pam.services.xmonad.gnupg = {
  #   enable = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.openssh.knownHosts = {
    nuc = { publicKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCfn0B8MF2jsLdndxj/x8jhdK32XTaO8kAowbwUY1slF0TCN+h3mwtFUKdZGgOvryjifzKe9eyCraBd/950cmT0QfjWN/oIW9URiZBsT09jWo2MyPUf38vjVaB4kNA152VZjR1kL40RwuXkETy9q6ZcP4VcQJbH1n5FxMOXnN8DT0pF3/DBgKEZmJ+LOUT0OenYMPlZuxZsZq3owNcODpKWpTvQljKA1fZU1ZihVewYy+OktAD+E7AVqtuPwsEKtyRFyGEn97lUmPpLavG42LhIgftNiT5cIhR54M2AmydSa3WkV64UwyZhHCUleK5B3m9DQM2Zda2a85ZnA+YJNvlRhcZ4qS1XmZPxMz50b8Dr8NXj6V+vKSFnme3XBEYuyL9midP+j/TfHMdQ5GbeDe9qZv/finC1aLKKsZH+w7EiLfsuteYz4/Qsc1NCRpcgCBcuKtzNmU2ow1vGTs5P34+7zw1PZj2J0qRImB5YIumJcixQIMovRkshTwEdj7XPST8k9S9SCYGWo8A6JI0MsPeOIK5fZde0IIEP9SBQyvukGScBeHpaMt0Lu4cqT8qif0xJtEk6spXI+AbEE1CQGxPX7l/sSvB5whDd/QGG0ApBmg9YO/KLSgFUAR2FE/DY/plSRXRHlYhxOvQhJdb+fanOD+xC/hjWiVCMm7UNBK3gDw== sibi@psibi.in"; };
  };

  # For disk automount
  security.polkit.enable = true;
  services.udisks2.enable = true;
  services.devmon.enable = true;

  services.fwupd.enable = true;

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
