{ mkDerivation, ansi-terminal, base, doctest, prettyprinter, stdenv
, text
}:
mkDerivation {
  pname = "prettyprinter-ansi-terminal";
  version = "1.1";
  sha256 = "0z2vi26qhrw5z36yy449x5yynv3wyx1c02z4m2lf7la7r9jwwfbj";
  libraryHaskellDepends = [ ansi-terminal base prettyprinter text ];
  testHaskellDepends = [ base doctest ];
  homepage = "http://github.com/quchen/prettyprinter";
  description = "ANSI terminal backend for the »prettyprinter« package";
  license = stdenv.lib.licenses.bsd2;
}
