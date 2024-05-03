{
  description = "Asus System Setup";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-23.11";
  };

  outputs = { self, nixpkgs}: {

    nixosConfigurations.arya = nixpkgs.lib.nixosSystem {

      system = "x86_64-linux";

      modules = [
        ./configuration.nix
      ];

      specialArgs = { inherit nixpkgs; };

    };
  };
}
