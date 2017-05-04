{ mkDerivation, ansi-terminal, async, base, bytestring
, concurrent-output, containers, directory, exceptions, mmorph, mtl
, pretty-show, primitive, random, resourcet, stdenv, stm
, template-haskell, text, th-lift, time, transformers
, transformers-base, unix, wl-pprint-annotated
}:
mkDerivation {
  pname = "hedgehog";
  version = "0.3";
  sha256 = "0navcrq51wy7040a8vh9izzln61qpk06z4r4g33j7v6qk2hflwb7";
  libraryHaskellDepends = [
    ansi-terminal async base bytestring concurrent-output containers
    directory exceptions mmorph mtl pretty-show primitive random
    resourcet stm template-haskell text th-lift time transformers
    transformers-base unix wl-pprint-annotated
  ];
  testHaskellDepends = [
    base containers pretty-show text transformers
  ];
  homepage = "https://hedgehog.qa";
  description = "Hedgehog will eat all your bugs";
  license = stdenv.lib.licenses.bsd3;
}
