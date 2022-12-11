{
  description = "NUC System Setup";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-22.11";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
  };

  outputs = { self, nixpkgs, nixos-hardware}: {

    nixosConfigurations.arya = nixpkgs.lib.nixosSystem {

      system = "x86_64-linux";

      modules = [
        ./configuration.nix
        nixos-hardware.nixosModules.intel-nuc-8i7beh
        ../system-modules/cloudflare-warp.nix
      ];

    };
  };
}
