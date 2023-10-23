## Test environments

* local macOS install, R 4.1.1
* win-builder (release, devel, oldrel)
* Rhub via 
    devtools::check_rhub(email = "mylesstokowski@gmail.com", interactive = FALSE,
      platforms = rhub::platforms() %>% dplyr::filter(!is.na(`cran-name`)) %>% dplyr::pull(name),
      env_vars=c(R_COMPILE_AND_INSTALL_PACKAGES = "always")
    )
    + Debian Linux, R-devel, clang, ISO-8859-15 locale
    + Debian Linux, R-devel, GCC
    + Debian Linux, R-patched, GCC
    + Debian Linux, R-release, GCC
    + Fedora Linux, R-devel, clang, gfortran
    + Fedora Linux, R-devel, GCC
    + Windows Server 2022, R-devel, 64 bit
    + Windows Server 2022, R-oldrel, 32/64 bit
    + Windows Server 2022, R-release, 32/64 bit

## R CMD check results

NOTES:

I got the following message. The DOI links may be slow, but they do load.

  URL: https://doi.org/10.1139/z86-105
    From: man/winter_tick.Rd
    Status: 403
    Message: Forbidden
  URL: https://doi.org/10.2307/3282188
    From: man/winter_tick.Rd
    Status: 403
    Message: Forbidden
    
