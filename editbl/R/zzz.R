.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Breaking change in `editbl`  >= 0.8.0!
    The output of `eDT()` is now a list instead of a single reactive.
    Please adjust to `eDT()$result` for old behavior.", domain = NULL, appendLF = TRUE)
}