{ lib, rustPlatform, fetchFromGitHub, fetchurl, pkgs, git }:

rustPlatform.buildRustPackage rec {
  pname = "amber";
  version = "0.1.0";

  src = fetchFromGitHub {
    owner = "psibi";
    repo = pname;
    rev = "1ad3243559cd1f0c06a4f3930f4976a46583136c";
    sha256 = "0mspv54f46kj9hl1k0615d1xyd8cvzzaq19vpd7byr3lv9k2zrln";
  };

  VERGEN_GIT_SHA = "1ad3243559cd1f0c06a4f3930f4976a46583136c";

  cargoSha256 = "0nngyhdg2rszp0a4alnb38n1y6756llv4w98620nvc1x86sfilkn";

  meta = with lib; {
    description = "Manage secret values in-repo via public key cryptography";
    homepage = "https://github.com/fpco/amber";
    license = licenses.mit;
    maintainers = [ maintainers.psibi ];
  };
}
