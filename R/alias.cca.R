"alias.cca" <-
  function(object, ...)
{
  if (is.null(object$CCA$alias))
    cat("No aliased constraints.\n")
  else {
    cat("Aliased constraints (redundant, collinear):\n")
    cat(object$CCA$alias, "\n")
  }
  invisible(object$CCA$alias)
}
