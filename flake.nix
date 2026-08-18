{
  description = "Reproducible development environment for qualtdict";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs =
    { nixpkgs, ... }:
    let
      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
    in
    {
      devShells = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };

          pkgcheck = pkgs.rPackages.buildRPackage {
            name = "pkgcheck";
            src = pkgs.fetchFromGitHub {
              owner = "ropensci-review-tools";
              repo = "pkgcheck";
              rev = "edef80ac8544b2b5d002f4ceafbd4cb45adf05ef";
              sha256 = "14irbaaz5fxf06ygc0cs64ap83k9jal0m0bz3vc2bjsjl33h8b5w";
            };
            propagatedBuildInputs = with pkgs.rPackages; [
              cli
              covr
              curl
              fs
              gert
              gh
              glue
              goodpractice
              httr2
              magrittr
              pkgstats
              praise
              rappdirs
              rmarkdown
              rprojroot
              rvest
              visNetwork
              withr
              srr
            ];
          };

          rPackages = with pkgs.rPackages; [
            # Package Imports and Suggests from DESCRIPTION.
            covr
            devtools
            dplyr
            glue
            haven
            jsonlite
            knitr
            lintr
            openNLP
            pkgcheck
            purrr
            qualtRics
            rmarkdown
            rlang
            roxygen2
            sjlabelled
            slowraker
            SnowballC
            spelling
            stringi
            stringr
            testthat
            tibble
            tidyr
            usethis
            xml2
          ];

          texlive = pkgs.texliveSmall.withPackages (ps: [ ps.inconsolata ]);
        in
        {
          default = pkgs.mkShell {
            JAVA_HOME = "${pkgs.jdk}/lib/openjdk";
            V8_PKG_CFLAGS = "-I${pkgs.nodejs-slim_22.libv8}/include";
            V8_PKG_LIBS = "-L${pkgs.nodejs-slim_22.libv8}/lib -lv8 -pthread -licui18n -licuuc";

            LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
              pkgs.bzip2
              pkgs.icu
              pkgs.jdk
              pkgs.libarchive
              pkgs.libdeflate
              pkgs.openssl
              pkgs.xz
              pkgs.zlib
            ];

            packages = [
              pkgs.R
              pkgs.air-formatter
              pkgs.bzip2
              pkgs.curl
              pkgs.icu
              pkgs.jdk
              pkgs.libarchive
              pkgs.libdeflate
              pkgs.libxml2
              pkgs.libuv
              pkgs.nodejs
              pkgs.nodejs-slim_22.libv8
              pkgs.openssl
              pkgs.pandoc
              pkgs.pkg-config
              pkgs.pre-commit
              pkgs.qpdf
              texlive
              pkgs.xz
              pkgs.zlib
            ] ++ rPackages;

            shellHook = ''
              PROJECT_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
              export PROJECT_R_LIB="$PROJECT_ROOT/.R-lib"
              export R_LIBS_USER="$PROJECT_R_LIB"
              mkdir -p "$PROJECT_R_LIB" "$PROJECT_ROOT/.nix"

              export R_PROFILE_USER="$PROJECT_ROOT/.nix/Rprofile"
              cat > "$R_PROFILE_USER" <<'EOF'
              local_lib <- Sys.getenv("PROJECT_R_LIB")
              if (nzchar(local_lib)) {
                dir.create(local_lib, recursive = TRUE, showWarnings = FALSE)
                local_lib <- normalizePath(local_lib, winslash = "/", mustWork = TRUE)
                user_lib <- normalizePath(Sys.getenv("R_LIBS_USER"), winslash = "/", mustWork = FALSE)
                paths <- normalizePath(.libPaths(), winslash = "/", mustWork = FALSE)
                Sys.setenv(R_LIBS_USER = local_lib)
                .libPaths(c(local_lib, paths[paths != user_lib & paths != local_lib]))
              }
              EOF

              echo "R development environment for qualtdict loaded"
              echo "Pinned nixpkgs revision: ${nixpkgs.rev or "dirty"}"
              echo "Project R library: $PROJECT_R_LIB"
              echo "Package and development dependencies are provided by flake.lock."
            '';
          };
        }
      );
    };
}
