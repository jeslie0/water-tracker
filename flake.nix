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

    closure-compiler.url = "github:jeslie0/closure-compiler-acocr";
  };

  outputs = { self, nixpkgs, flake-utils, mkSpagoDerivation, ps-overlay, closure-compiler }:
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
              nativeBuildInputs = [ pkgs.esbuild pkgs.purs-backend-es pkgs.nodePackages.uglify-js ];
              patches = [./patches/spago-purs-backend-es.patch];
              buildPhase =
                ''
                spago build && purs-backend-es bundle-app --no-build --minify --int-tags --to=main.js;
                uglifyjs main.js --compress 'pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output public/js/main.min.js
                cp -r ${patternflyV5} public/css/patternfly;
                '';
              installPhase =
                ''
                mkdir $out;
                cp -r public/* $out;
                cp spago.yaml $out
                '';
            };

          water-tracker =
          pkgs.mkSpagoDerivation {
            name = "water-tracker";
            version = "0.1.0";
            src = ./src/site;
            nativeBuildInputs = with pkgs;
              [ purs-backend-es-unstable
                esbuild
                closure-compiler.packages.${system}.default
                spago-unstable
                purs-unstable
                nodePackages.uglify-js
              ];
            # patches = [ ./patches/spago-purs-backend-es.patch ];
            buildPhase =
              ''
                mv .spago/packages .spago/p
                spago build
                purs-backend-es bundle-app --minify --int-tags --to=main.es.js
                closure-compiler-acocr -O SIMPLE --assume_function_wrapper true --isolation_mode IIFE --emit_use_strict --js_output_file main.cc.js main.es.js
                uglifyjs --compress --mangle --output main.min.js main.cc.js
              '';
            installPhase = ''
                         mkdir $out;
                         cp -r public/* $out
                         mkdir -p $out/css/patternfly
                         cp -r ${patternflyV5}/* $out/css/patternfly
                         cp main.min.js $out/js
                         rm $out/js/.gitignore
                         rm $out/css/.gitignore
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

          fitnessMonad = pkgs.stdenv.mkDerivation {
            pname = "FitnessMonad";
            version = "0.1.0";
            src = ./.;
            nativeBuildInputs = [ pkgs.makeWrapper ];
            installPhase =
              ''
              mkdir -p $out

              makeWrapper ${server}/bin/FitnessMonad $out/bin/FitnessMonad \
                --set FITNESS_MONAD_HTML_DIR $out/usr/share/FitnessMonad/www/

              mkdir -p $out/usr/share/FitnessMonad/www/

              cp -r ${site}/* $out/usr/share/FitnessMonad/www/
              '';
          };
      in
        {
          packages = {
            ${serverName} = server;
            water-tracker = water-tracker;
            patternflyV5 = patternflyV5;
            fitnessMonad = fitnessMonad;
            default = fitnessMonad;
          };

          devShell = haskellPackages.shellFor {
            # The packages that the shell is for.
            packages = pkgs: [ server ];

            buildInputs = with haskellPackages;
              [ haskell-language-server
                cabal-install
                pkgs.purs-unstable
                pkgs.purs-tidy
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
