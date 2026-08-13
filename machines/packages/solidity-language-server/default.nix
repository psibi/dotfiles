{
  lib,
  rustPlatform,
  fetchFromGitHub,
}:

rustPlatform.buildRustPackage (finalAttrs: {
  pname = "solidity-language-server";
  version = "0.1.34";

  src = fetchFromGitHub {
    owner = "asyncswap";
    repo = "solidity-language-server";
    tag = "v${finalAttrs.version}";
    hash = "sha256-FwtSc9tdXeDBXMWqc9zvSNlK7TUAz+JXS0yLVnVbOxc=";
  };

  cargoHash = "sha256-X6Mk3/DwTSPbcJIgRh5zYUshzIDLtIv+l0v97rw/Kfs=";

  # Tests rely on network access and solc
  doCheck = false;

  meta = {
    description = "A fast Solidity language server powered by solc and Foundry";
    homepage = "https://github.com/asyncswap/solidity-language-server";
    changelog = "https://github.com/asyncswap/solidity-language-server/blob/v${finalAttrs.version}/CHANGELOG.md";
    license = lib.licenses.mit;
    maintainers = with lib.maintainers; [ psibi ];
    mainProgram = "solidity-language-server";
  };
})
