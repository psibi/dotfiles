{ lib, rustPlatform, fetchFromGitHub }:

rustPlatform.buildRustPackage rec {
  pname = "minijinja";
  version = "1.0.10";

  src = fetchFromGitHub {
    owner = "mitsuhiko";
    repo = pname;
    rev = version;
    hash = "sha256-2OujlzRmHin6R9Dujo8wzVUI2IK5H2zyas9cfyQfyOA=";
  };

  cargoSha256 = lib.fakeHash;

  meta = with lib; {
    description = "Yet another language server for Nix";
    homepage = "https://github.com/oxalica/nil";
    license = with licenses; [ mit asl20 ];
    maintainers = with maintainers; [ psibi ];
  };
}
