{ buildGoPackage, lib, fetchFromGitHub }:
buildGoPackage rec {
  pname = "tgswitch";
  version = "0.4.326";

  src = fetchFromGitHub {
    owner = "warrensbox";
    repo = "tgswitch";
    rev = version;
    sha256 = "190wgmirfbk1pqzpj0j8xzn3argi2w227g5ng9fcrnrw9pvph09v";
  };

  goPackagePath = "github.com/warrensbox/tgswitch";

  meta = with lib; {
    description = "A command line tool to switch between different versions of terragrunt";
    homepage = "https://github.com/warrensbox/tgswitch";
    license = licenses.mit;
    maintainers = with maintainers; [ psibi ];
  };
}
