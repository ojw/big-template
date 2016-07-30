{ mkDerivation, aeson, base, bytestring
, case-insensitive, containers, exceptions, haskell-src-exts
, http-api-data, http-media, network-uri, reflex, reflex-dom, safe
, servant, servant-client, stdenv
, string-conversions, text, transformers
}:
mkDerivation {
  pname = "big-template-client";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring case-insensitive containers exceptions
    haskell-src-exts http-api-data http-media network-uri reflex
    reflex-dom safe servant servant-client
    string-conversions text transformers
  ];
  executableHaskellDepends = [
    base containers haskell-src-exts reflex reflex-dom safe
  ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/ojw/big-template#readme";
  description = "Initial project template from stack";
  license = stdenv.lib.licenses.bsd3;
}
