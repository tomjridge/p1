{ }:
let 
    pkgs = import <nixpkgs> {};
    stdenv = pkgs.stdenv;
    fetchgit = pkgs.fetchgit;
    ocaml=pkgs.ocaml; 
    op = pkgs.ocamlPackages;
    findlib=op.findlib;
in stdenv.mkDerivation {
      name = "p1";
    
  #    src = fetchgit {
  #      url = https://github.com/tomjridge/p3.git;
  #      rev = "0e42a29";
  #      sha256 = "795b8bacbea102021ad4aaa819d578e58fd7d2041eba60e36482e04e01f81c32";
  #    };
      src=./.;
    
      buildInputs = [ ocaml findlib ];
    
      configurePhase = "true"; 	# Skip configure
  
      createFindlibDestdir = true;

      postInstall=''
        cp -R build gen $out
        mkdir -p $out/bin && cp build/*.native $out/bin
      '';
    }
