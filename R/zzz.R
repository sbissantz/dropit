.onLoad <- function(libname, pkgname) { }

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(sprintf("%s\nVersion: %s", pkgname, utils::packageVersion(pkgname)))
}