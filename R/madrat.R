
.onAttach <- function(libname, pkgname) {
  madrat::madratAttach(pkgname)
}

.onDetach <- function(libpath) {
  madrat::madratDetach(libpath)
}

# redirect cat to message to have messages
# properly logged
cat <- function(...) message(...)
