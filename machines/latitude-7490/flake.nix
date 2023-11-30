{
  description = "Latitude 7490 System Setup";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-23.11";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
  };

  outputs = { self, nixpkgs, nixos-hardware}: {

    nixosConfigurations.elric = nixpkgs.lib.nixosSystem {

      system = "x86_64-linux";

      modules = [
        ./configuration.nix
        nixos-hardware.nixosModules.dell-latitude-7490
      ];

      specialArgs = { nixpkgs = nixpkgs; };

    };
  };
}
