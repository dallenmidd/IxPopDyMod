## Resubmission
This is a resubmission. In this version I have: 

* added a reference describing ogden_feed_fun() in that function's documentation. There are no existing references that comprehensively describe the methods in our package. We plan to write a paper describing the package, and when available, cite it in the description field of the DESCRIPTION file.

* added executable examples to exported functions

* added \value to .Rd files regarding exported methods

## Test environments

* local macOS install, R 4.1.1
* local Manjaro Linux 21.1.2 install, R 4.1.1
* win-builder (release, devel, oldrel)
* Rhub via 
    devtools::check_rhub(email = "mylesstokowski@gmail.com", interactive = FALSE,
      platforms = rhub::platforms() %>% filter(!is.na(`cran-name`)) %>% pull(name), 
      env_vars=c(R_COMPILE_AND_INSTALL_PACKAGES = "always")
    )
    + Debian Linux, R-devel, clang, ISO-8859-15 locale
    + Debian Linux, R-devel, GCC
    + Debian Linux, R-patched, GCC
    + Debian Linux, R-release, GCC
    + Fedora Linux, R-devel, clang, gfortran
    + Fedora Linux, R-devel, GCC
    + macOS 10.13.6 High Sierra, R-release, CRAN's setup
    + Apple Silicon (M1), macOS 11.6 Big Sur, R-release
    + Oracle Solaris 10, x86, 32 bit, R-release
    + Oracle Solaris 10, x86, 32 bit, R release, Oracle Developer Studio 12.6
    + Windows Server 2008 R2 SP1, R-devel, 32/64 bit
    + Windows Server 2008 R2 SP1, R-oldrel, 32/64 bit
    + Windows Server 2008 R2 SP1, R-release, 32/64 bit

## R CMD check results

NOTES:

+ Possibly mis-spelled words in DESCRIPTION: Ixodidae (16:42)

 
