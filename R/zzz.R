.onLoad <- function(libname, pkgname) { }

.onAttach <- function(libname, pkg) {
  intro <- "dropit"
  version <- utils::packageVersion(pkg)
  packageStartupMessage(paste0(intro, "\n", "Version: ", version))
}