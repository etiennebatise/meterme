with import <nixpkgs> {};
with elmPackages;
# let
  # yopo = import ./build.nix { withTestDeps=true; };
# in
stdenv.mkDerivation rec {

  name = "workbook";
  env = buildEnv { name = name; paths = buildInputs; };

  # LD_LIBRARY_PATH =
  #   let
  #     libraries = [imagemagick file openssl ffmpeg];
  #     path = stdenv.lib.makeLibraryPath libraries;
  #   in
  #     "${path}";

  buildInputs = [
  elm
    # libpqxx
    # openssl
    # imagemagick
    # file
    # redis
    # libyaml
    # yopo
  ];

  # shellHook = ''
  #   SOURCE_DATE_EPOCH=$(date +%s)
  # '';
}
