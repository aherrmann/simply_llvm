let

  nixpkgs = (import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "5b08a40da92199aaf53e191e28eac0e7bfdd804c";
    sha256 = "1g9hdph72aflc3yk36jfi9cikykr3m76fnk0z29db6511w1krh5f";
  };

in

  import nixpkgs { }
