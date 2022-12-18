{ lib, rustPlatform, fetchFromGitHub, nix}:

rustPlatform.buildRustPackage rec {
  pname = "nil";
  version = "2022-12-01";

  src = fetchFromGitHub {
    owner = "oxalica";
    repo = pname;
    rev = version;
    sha256 = "sha256-E/QGmoL7Q3GDR2/I5o2CAMHMcmPQEJAySke1s+nOaho=l";
  };

  cargoSha256 = "sha256-T3i86L6cF6uFbSs7xtKHGzB6XrE9jn2RZghxFzDleXU=";
  nativeBuildInputs = [
    (lib.getBin nix)
  ];

  meta = with lib; {
    description = "Yet another language server for Nix";
    homepage = "https://github.com/oxalica/nil";
    license = with licenses; [ mit asl20 ];
    maintainers = with maintainers; [ figsoda oxalica ];
  };
}
