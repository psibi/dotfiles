{ lib, mkYarnPackage, fetchFromGitHub }:

mkYarnPackage rec {
  pname = "yaml-language-server";
  # https://github.com/redhat-developer/yaml-language-server/issues/881
  version = "1.11.0";

  src = fetchFromGitHub {
    owner = "redhat-developer";
    repo = pname;
    rev = "${version}";
    hash = "sha256-/mM3KyF22mnoOXaxAbKcxrmWiztMM7UdvfTYvDOF3fc=";
  };

  buildPhase = ''
    runHook preBuild
    export HOME=$(mktemp -d)
    yarn --offline build
    runHook postBuild
  '';

  meta = with lib; {
    description = "Yaml language server";
    homepage = "https://github.com/redhat-developer/yaml-language-server/releases/tag/1.11.0";
    license = licenses.gpl3Only;
    maintainers = with maintainers; [ psibi ];
  };
}
