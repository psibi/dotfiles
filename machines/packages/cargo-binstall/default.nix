{ lib, rustPlatform, fetchFromGitHub, openssl, pkg-config }:

rustPlatform.buildRustPackage rec {
  pname = "cargo-binstall";
  version = "0.6.2";

  src = fetchFromGitHub {
    owner = "ryankurte";
    repo = pname;
    rev = "v${version}";
    sha256 = "0v86rply3hvazj3jyg03rxd9m6s5p6f90r04c1bkc6wx30wwy35n";
  };

  nativeBuildInputs = [ pkg-config ];

  buildInputs = [ openssl ];

  cargoSha256 = "0dhk08hyvrqmygn96aak572dpikr4r26kzrjl3m1vw8czcw145nl";

  meta = with lib; {
    description = "Binary installation for rust projects";
    homepage = "https://github.com/ryankurte/cargo-binstall";
    license = licenses.gpl3;
    maintainers = [ maintainers.psibi ];
  };
}
