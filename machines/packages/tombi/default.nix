{ lib, rustPlatform, fetchFromGitHub }:

rustPlatform.buildRustPackage rec {
  pname = "tombi";
  version = "0.4.9";

  src = fetchFromGitHub {
    owner = "tombi-toml";
    repo = "tombi";
    rev = "v${version}";
    sha256 = "sha256-2516aT6zaI5bntjjJ/p/yk0gWW6fzixQx5ESs29aS6Q=";
  };

  # Tests relies on the presence of network
  doCheck = false;
  cargoBuildFlags = [ "--package tombi-cli" ];
  cargoHash = "sha256-Pa8DBsXIJ4BKOMExKJNWbLBU4gt58/XHfe219434uaw=";

  meta = with lib; {
    description = "TOML Formatter / Linter / Language Server";
    homepage = "https://github.com/tombi-toml/tombi";
    license = licenses.mit;
    maintainers = with maintainers; [ psibi ];
    mainProgram = "tombi";
  };
}
