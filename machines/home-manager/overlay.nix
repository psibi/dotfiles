self: super: {
  em = import ./scripts/em.nix { pkgs = super.pkgs; };
  kubectl-argo-rollouts = super.pkgs.callPackage ../packages/kubectl-argo-rollouts/default.nix {};
}
