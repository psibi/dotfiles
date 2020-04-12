pkgs: {
  # mumble = pkgs.mumble.override { pulseSupport = true; };
  keepassxc = pkgs.keepassxc.overrideAttrs (oldAttrs: {
    version = "2.5.4";
    src = pkgs.fetchFromGitHub {
      owner = "keepassxreboot";
      repo = "keepassxc";
      rev = "2.5.4";
      sha256 = "1xih9q1pxszalc0l29fmjxwn1vrrrrbnhc8gmi8brw5sclhbs6bh";
  };
    patches = [];
  });
}

