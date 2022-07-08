{ lib, fetchFromGitHub, rustPlatform }:

rustPlatform.buildRustPackage rec {
  pname = "mprocs";
  version = "0.6.0";

  src = fetchFromGitHub {
    owner = "pvolok";
    repo = pname;
    rev = "v${version}";
    sha256 = "sha256-q5LTfmOh2bn9S/fQQ3SHFd46lRwPxfD68ooTKuOIz5k=";
  };

  cargoSha256 = "sha256-6fDwy658bANj1u4hc4y9KQ2Wi9/AoghlaIyY6XXO6gs=";

  meta = with lib; {
    description = "A TUI tool to run multiple commands in parallel and show the output of each command separately";
    homepage = "https://github.com/pvolok/mprocs";
    license = licenses.mit;
    maintainers = with maintainers; [ GaetanLepage thehedgeh0g ];
  };
}
