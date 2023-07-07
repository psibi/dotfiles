{ stdenv, lib, rustPlatform, fetchFromGitHub, pkgs }:

rustPlatform.buildRustPackage {
  pname = "cnx-sibi";
  version = "0.1";

  src = fetchFromGitHub {
    owner = "psibi";
    repo = "dotfiles";
    rev = "47db9a880244412114a52581b611cab4a9dfa642";
    sha256 = "sha256-wdzZKaiLrleNxuQ7RfqwxE0SXEIcvWtLdCGncCK2/jI=";
  };

  sourceRoot = "source/cnx";

  nativeBuildInputs = with pkgs; [
    pkg-config
    xorg.libxcb
    xorg.xcbutilwm
    python3
    libllvm
    llvmPackages.libclang
    libclang
    clang
    xorg.libxcb
    xorg.libxcb.dev
    wirelesstools
  ];

  buildInputs = with pkgs; [
    rust-bindgen
    wirelesstools
    pango
    cairo
    gobject-introspection
    openssl
    alsa-lib
    glib
    xorg.xcbutilwm
  ];

  # Add glibc, clang, glib headers to bindgen search path
  BINDGEN_EXTRA_CLANG_ARGS =
    # Includes with normal include path
    (builtins.map (a: ''-I"${a}/include"'') [
      pkgs.glibc.dev
    ])
    # Includes with special directory paths
    ++ [
      ''-I"${pkgs.llvmPackages_latest.libclang.lib}/lib/clang/${pkgs.llvmPackages_latest.libclang.version}/include"''
      ''-I"${pkgs.glib.dev}/include/glib-2.0"''
      ''-I${pkgs.glib.out}/lib/glib-2.0/include/''
      ''-I${pkgs.wirelesstools.outPath}/include''
    ];

  LIBCLANG_PATH = "${pkgs.llvmPackages.libclang.lib}/lib";

  cargoSha256 = "sha256-hXNdiPcmzVEeT6DKxwlnBnygy0PFCbmaooeXvvNbXAE=";

  meta = with lib; {
    description = "CNX status bar";
    homepage = "https://github.com/psibi/dotfiles";
    license = licenses.mit;
    maintainers = with maintainers; [ psibi ];
  };
}
