{ stdenv
}: stdenv.mkDerivation {
  name = "smos-mime-setup";
  buildCommand = ''
    # Set up the types
    mkdir -p $out/share/mime/text
    cp ${../mime/smos-text.mime-type} $out/share/mime/text/smos.xml

    # Set up the .desktop files
    mkdir -p $out/share/applications
    cp ${../mime/smos.desktop} $out/share/applications/smos.desktop
  '';
}
