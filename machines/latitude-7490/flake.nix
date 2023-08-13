{
  description = "Latitude 7490 System Setup";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-23.05";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
  };

  outputs = { self, nixpkgs, nixos-hardware}: {

    nixosConfigurations.elric = nixpkgs.lib.nixosSystem {

      system = "x86_64-linux";

      modules = [
        ./configuration.nix
        nixos-hardware.nixosModules.dell-latitude-7490
      ];

      extraSpecialArgs = { nixpkgs = nixpkgs; };

    };
  };
}
