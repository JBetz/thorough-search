{ mkDerivation, aeson, base, bytestring, config-ini, containers
, directory, exceptions, http-client, lens, lens-aeson, mtl, path
, path-io, smtp-mail, sqlite-simple, stdenv, text, wreq, zip, zlib
}:
mkDerivation {
  pname = "thorough-search";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring config-ini containers directory exceptions
    http-client lens lens-aeson mtl path path-io smtp-mail
    sqlite-simple text wreq zip
  ];
  executableHaskellDepends = [ base lens sqlite-simple ];
  executableSystemDepends = [ zlib ];
  homepage = "https://github.com/JBetz/thorough-search#readme";
  license = stdenv.lib.licenses.bsd3;
}
