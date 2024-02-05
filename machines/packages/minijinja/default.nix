{ lib, rustPlatform, fetchFromGitHub }:

rustPlatform.buildRustPackage rec {
  pname = "minijinja";
  version = "1.0.12";

  src = fetchFromGitHub {
    owner = "mitsuhiko";
    repo = pname;
    rev = version;
    hash = "sha256-v5YTPcUiCUQvTURqgKepdOjKZ5rFLr+mF7X+s5GvxdM=";
  };

  cargoSha256 = "sha256-OhfrlT2DZU3ahH9PKqpEK8f34J2E6zNUai3hYBPg7v4=";

  # https://github.com/mitsuhiko/minijinja/issues/402
  doCheck = false;

  cargoBuildFlags = "--bin minijinja-cli";

  meta = with lib; {
    description = "Command Line Utility to render MiniJinja/Jinja2 templates";
    homepage = "https://github.com/mitsuhiko/minijinja";
    license = with licenses; [ asl20 ];
    maintainers = with maintainers; [ psibi ];
    mainProgram = "minijinja-cli";
  };
}
