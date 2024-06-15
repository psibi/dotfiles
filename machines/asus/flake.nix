{
  description = "Asus System Setup";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-24.05";
  };

  outputs = { self, nixpkgs}: {

    nixosConfigurations.hask = nixpkgs.lib.nixosSystem {

      system = "x86_64-linux";

      modules = [
        ./configuration.nix
      ];

      specialArgs = { inherit nixpkgs; };

    };
  };
}
