{ lib, fetchFromGitHub, rustPlatform }:

rustPlatform.buildRustPackage rec {
  pname = "mprocs";
  version = "0.5.0";

  src = fetchFromGitHub {
    owner = "pvolok";
    repo = pname;
    rev = "v${version}";
    sha256 = "sha256-in2aCNY86VncbqOWpEnU95FlkUWImNJFobybQI/siOM=";
  };

  cargoSha256 = "sha256-cnFUu5r48vC2emFJSkA8qhmXT3tcZHRj0fMv7plioY8=";

  meta = with lib; {
    description = "A TUI tool to run multiple commands in parallel and show the output of each command separately";
    homepage = "https://github.com/pvolok/mprocs";
    license = licenses.mit;
    maintainers = with maintainers; [ GaetanLepage thehedgeh0g ];
  };
}
