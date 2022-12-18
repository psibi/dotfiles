{ lib, rustPlatform, fetchFromGitHub }:

rustPlatform.buildRustPackage rec {
  # Renaming it to amber-secret because another package named amber exists
  pname = "amber-secret";
  version = "0.1.3";

  src = fetchFromGitHub {
    owner = "fpco";
    repo = "amber";
    rev = "v${version}";
    sha256 = "sha256-kPDNTwsfI+8nOgsLv2aONrLGSRZhw5YzNntJ2tbE0oI=l";
  };

  cargoSha256 = "sha256-fTdTgbeOQXEpLHq9tHiPLkttvaxS/WJ86h3jRdrfbJM=";

  meta = with lib; {
    description = "Manage secret values in-repo via public key cryptography";
    homepage = "https://github.com/fpco/amber";
    license = licenses.mit;
    maintainers = with maintainers; [ psibi ];
    mainProgram = "amber";
  };
}
