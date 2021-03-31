# On load function
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2021-03-30

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.arfpam <- list(
    arfpam.author = "Kevin Potter",
    arfpam.email = "kevin.w.potter@gmail.com"
  )
  toset <- !(names(op.arfpam) %in% names(op))
  if(any(toset)) options(op.arfpam[toset])

  invisible()
}
