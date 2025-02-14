{
  description = "NUC System Setup";

  inputs = {
    nixpkgs = { url = "nixpkgs/nixos-24.11"; };
    nixpkgs-unstable.url = "nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/release-24.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-index-database = {
      url = "github:Mic92/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, nixpkgs-unstable, home-manager, nix-index-database }:
    let
      unstable = import nixpkgs-unstable {
        config.allowUnfree = true;
        system = "x86_64-linux";
      };
    in
    {
      homeConfigurations.hask = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages.x86_64-linux;
        modules = [
          ./home.nix
          nix-index-database.hmModules.nix-index
        ];
        extraSpecialArgs = {
          unstable-pkgs = unstable.pkgs;
          nixpkgs = nixpkgs;
        };
      };
    };
}
