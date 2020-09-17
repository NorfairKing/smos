{ stdenv
}: stdenv.mkDerivation {
  name = "smos-mime-setup";
  buildCommand = ''
    # Set up the types
    mkdir -p $out/share/mime/packages
    cp ${../mime/smos.mime-type} $out/share/mime/packages/smos.xml

    # Set up the .desktop files
    mkdir -p $out/share/applications
    cp ${../mime/smos.desktop} $out/share/applications/smos.desktop
  '';
}
