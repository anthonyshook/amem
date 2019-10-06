
.onAttach <- function(libname, pkgname){

  pkg_version <- as.character(utils::packageVersion("amem"))

  if (length(strsplit(pkg_version, split = ".", fixed = TRUE)[[1]]) == 4) {
    startup_msg <- paste0("Warning: You are using a development build of AMEM; stability issues may occur.")
    packageStartupMessage(startup_msg)
  }

}
