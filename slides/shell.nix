with (import ../nixpkgs);

runCommand "dummy" {
  buildInputs = [ pandoc ];
} ""
