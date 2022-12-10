{
  description = "Latitude 7490 System Setup";

  inputs = {
    nixpkgs = {
      url = "nixpkgs/nixos-22.11";
    };
    nixpkgs-unstable.url = "nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
    };
  };

  outputs = { self, nixpkgs, nixpkgs-unstable, home-manager}:
    let system = "x86_64-linux";
        pkgs = nixpkgs.legacyPackages.${system};
    in {
    homeConfigurations.elric = home-manager.lib.homeManagerConfiguration {
      inherit pkgs;

      modules = [
        ./home.nix
      ];

    };
  };
}
