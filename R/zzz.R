.First.lib <- function(lib, pkg)  {
    library.dynam("vegan", pkg, lib)
}
if (R.version$major == 1 && R.version$minor < 8)
    printCoefmat <- print.coefmat


