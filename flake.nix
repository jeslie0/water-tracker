{
  description = "My fitness app";

  inputs = {
    nixpkgs.url =
      "github:nixos/nixpkgs/nixos-unstable";

    flake-utils.url =
      "github:numtide/flake-utils";

    mkSpagoDerivation.url =
      "github:jeslie0/mkSpagoDerivation";

    ps-overlay.url =
      "github:thomashoneyman/purescript-overlay";
  };

  outputs = { self, nixpkgs, flake-utils, mkSpagoDerivation, ps-overlay }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs =
          import nixpkgs {
            inherit system;
            overlays =
              [ mkSpagoDerivation.overlays.default
                ps-overlay.overlays.default
              ];
          };

        haskellPackages =
          pkgs.haskellPackages;

        serverName =
          with builtins;
          let
            cabalFileName =
              head ((filter (pkgs.lib.hasSuffix ".cabal")) (attrNames (readDir ./src/server)));
          in
            head (match "^.*name\:\ *([^[:space:]]*).*$" (readFile "${./src/server}\/${cabalFileName}"));

          server =
            haskellPackages.callCabal2nix serverName ./src/server {};

          site =
            pkgs.mkSpagoDerivation {
              src = ./src/site;
              nativeBuildInputs = [ pkgs.esbuild ];
              buildPhase =
                ''
                spago bundle --outfile public/js/main.min.js;
                cp -r ${patternflyV5} public/css/patternfly;
                '';
              installPhase =
                ''
                mkdir $out; cp -r public/* $out
                '';
            };

          patternflyV5 =
            let
              version =
                "5.1.0";
            in
              pkgs.stdenvNoCC.mkDerivation {
                pname = "patternfly";
                version = version;
                src = builtins.fetchTarball {
                  url = "https://registry.npmjs.org/@patternfly/patternfly/-/patternfly-${version}.tgz";
                  sha256 = "sha256:1cdcd9z263wjvh9jadrry8b4zndc5m1vkxb4hz04xhj69fz9dyli";
                };
                installPhase = "mkdir -p $out; mv * $out;";
              };
      in
        {
          packages = {
            ${serverName} = server;
            ${site.name} = site;
            patternflyV5 = patternflyV5;
          };

          devShell = haskellPackages.shellFor {
            # The packages that the shell is for.
            packages = pkgs: [ server ];

            buildInputs = with haskellPackages;
              [ haskell-language-server
                cabal-install
                pkgs.purs-unstable
                pkgs.spago-unstable
                pkgs.purescript-language-server-unstable
                pkgs.esbuild
                pkgs.purs-backend-es
                pkgs.watchexec
              ];

            # Add build inputs of the following derivations.
            inputsFrom = [ ];

            # Enables Hoogle for the builtin packages.
            withHoogle = true;
          };
        }
    );
}
