{ pkgs ? import <nixpkgs> {} }:

let
  rPackages = with pkgs.rPackages; [
    # Core dependencies from DESCRIPTION
    crul
    dplyr
    glue
    haven
    magrittr
    openNLP
    purrr
    qualtRics
    rlang
    sjlabelled
    slowraker
    SnowballC
    stringi
    stringr
    tibble
    tidyr
    xml2

    # Suggests
    covr
    knitr
    rmarkdown
    testthat
    vcr

    # Development tools
    devtools
    roxygen2
    usethis
  ];
in
pkgs.mkShell {
  buildInputs = [
    pkgs.R
    pkgs.pandoc
    pkgs.qpdf
  ] ++ rPackages;

  shellHook = ''
    echo "R development environment for qualtdict loaded"
    echo "qualtRics and all dependencies are available"
  '';
}
