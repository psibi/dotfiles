{ lib, rustPlatform, fetchFromGitHub }:

rustPlatform.buildRustPackage rec {
  pname = "rucredstash";
  version = "0.9.2";

  src = fetchFromGitHub {
    owner = "psibi";
    repo = "rucredstash";
    rev = "v${version}";
    sha256 = "sha256-trupBiinULzD8TAy3eh1MYXhQilO08xu2a4yN7wwhwk=";
  };

  # Disable tests since it requires network access and relies on the
  # presence of certain AWS infrastructure
  doCheck = false;

  cargoSha256 = "sha256-TYobVjjzrK3gprZcYyY98EvdASkq4urB+WiLlbJbwmk=";

  meta = with lib; {
    description =
      "Rust port for credstash. Manages credentials securely in AWS cloud.";
    homepage = "https://github.com/psibi/rucredstash";
    license = licenses.mit;
    maintainers = with maintainers; [ psibi ];
  };
}
