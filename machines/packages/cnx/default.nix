{ stdenv, lib, rustPlatform, fetchFromGitHub, pkgs }:

rustPlatform.buildRustPackage rec {
  pname = "cnx-sibi";
  version = "0.1";

  src = fetchFromGitHub {
    owner = "psibi";
    repo = "dotfiles";
    rev = "f034fe6b15869ea4f7bd6aea888311bdfddd5fa7";
    sha256 = "1z51bycy0f9c8wbywvpyqrzyr7cx6qc3v2hrk03dn7gq4hldq586";
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
  ];

  buildInputs = with pkgs; [
    rust-bindgen
    wirelesstools
    pango
    cairo
    gobject-introspection
    openssl
    alsaLib
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

  cargoSha256 = "sha256:09afj0nxam950rljyjllxy319702l402ss130wibhmlcrafl01f1";

  meta = with lib; {
    description = "CNX status bar";
    homepage = "https://github.com/psibi/dotfiles";
    license = licenses.mit;
    maintainers = with maintainers; [ psibi ];
  };
}
