with (import ../nixpkgs.nix);

runCommand "dummy" {
  buildInputs = [ pandoc ];
} ""
