{ pkgs ? import <nixpkgs> {} }:

let
  treesitter-r = pkgs.rPackages.buildRPackage {
    name = "treesitter.r";
    src = pkgs.fetchurl {
      url = "https://cloud.r-project.org/src/contrib/treesitter.r_1.2.0.tar.gz";
      sha256 = "14wvxbzd3a9bkgpracyxprb9b028lm46v4qxdcyiz02i3c91npqp";
    };
  };

  goodpractice = pkgs.rPackages.buildRPackage {
    name = "goodpractice";
    src = pkgs.fetchurl {
      url = "https://ropensci.r-universe.dev/src/contrib/goodpractice_1.1.0.001.tar.gz";
      sha256 = "1vkjk05zycr50h79kv8q67jk6kxzalpahxczzn4cf07z28c5draz";
    };
    propagatedBuildInputs = with pkgs.rPackages; [
      cli
      covr
      curl
      cyclocomp
      desc
      jsonlite
      lintr
      praise
      rcmdcheck
      roxygen2
      rstudioapi
      spelling
      treesitter
      treesitter-r
      urlchecker
      whoami
      withr
    ];
  };

  pkgstats = pkgs.rPackages.buildRPackage {
    name = "pkgstats";
    src = pkgs.fetchurl {
      url = "https://ropensci.r-universe.dev/src/contrib/pkgstats_0.2.3.001.tar.gz";
      sha256 = "0xwr7v5cnsjh3x21bnv2c7qnh0c2h5711csk0fzvxhdjxfi9mrb4";
    };
    propagatedBuildInputs = with pkgs.rPackages; [
      brio
      checkmate
      cpp11
      dplyr
      fs
      igraph
      memoise
      readr
      roxygen2
      rprojroot
      sys
      withr
    ];
  };

  srr = pkgs.rPackages.buildRPackage {
    name = "srr";
    src = pkgs.fetchurl {
      url = "https://ropensci.r-universe.dev/src/contrib/srr_0.1.5.013.tar.gz";
      sha256 = "00grccl4v75pv4nz2sy4sg6gkhc77pyg8ddc4s3vfmvy3c88sf22";
    };
    propagatedBuildInputs = with pkgs.rPackages; [
      cli
      clipr
      fs
      gert
      here
      Rcpp
      roxygen2
      rprojroot
    ];
  };

  pkgcheck = pkgs.rPackages.buildRPackage {
    name = "pkgcheck";
    src = pkgs.fetchurl {
      url = "https://ropensci.r-universe.dev/src/contrib/pkgcheck_0.1.3.003.tar.gz";
      sha256 = "1rcv7r8anw7vqrdjjxhgfacgfrw02b1fj4ywiphhp39xfvfs1wyi";
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
    # Development and review tools.
    covr
    devtools
    knitr
    lintr
    pkgcheck
    rmarkdown
    roxygen2
    spelling
    testthat
    usethis
    vcr
  ];

in
pkgs.mkShell {
  LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
    pkgs.bzip2
    pkgs.icu
    pkgs.jdk
    pkgs.libarchive
    pkgs.openssl
    pkgs.xz
    pkgs.zlib
  ];

  buildInputs = [
    pkgs.R
    pkgs.bzip2
    pkgs.curl
    pkgs.icu
    pkgs.jdk
    pkgs.libarchive
    pkgs.libxml2
    pkgs.libuv
    pkgs.nodejs
    pkgs.openssl
    pkgs.pandoc
    pkgs.pkg-config
    pkgs.pre-commit
    pkgs.qpdf
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
    echo "Project R library: $PROJECT_R_LIB"
    echo "Install package dependencies with:"
    echo "  Rscript -e 'devtools::install_dev_deps(dependencies = TRUE, upgrade = \"never\")'"
  '';
}
