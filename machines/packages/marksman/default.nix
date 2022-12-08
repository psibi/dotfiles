{ stdenv
, lib
, fetchFromGitHub
, buildDotnetModule
, dotnetCorePackages
}:

buildDotnetModule rec {
  pname = "marksman";
  version = "2022-11-25";

  src = fetchFromGitHub {
    owner = "artempyanykh";
    repo = "marksman";
    rev = "${version}";
    sha256 = "sha256-f5vbYp+7Ez96lbK0yvPekt3W3X6kKPXO6Lowb+hLLsc=";
  };

  dotnetFlags = [ "--runtime linux-x64" ];

  projectFile = "Marksman.sln";
  nugetDeps = ./deps.nix;

  meta = with lib; {
    description = "Markdown Language Server";
    license = licenses.mit;
    maintainers = [ maintainers.psibi ];
  };
}
