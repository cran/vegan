"identify.ordiplot" <-
    function(x, what = c("sites","species", "constraints"), ...)
{
    what = match.arg(what)
    if (what == "sites") {
        if (is.null(x$sites))
            stop("No site scores available in ", deparse(substitute(x)))
        out <- identify(x$sites, labels=rownames(x$sites), cex=0.7)
    }
    if (what == "species") {
        if (is.null(x$species))
            stop("No species scores available in ", deparse(substitute(x)))
        out <- identify(x$species, labels=rownames(x$species), cex=0.7, col=2) 
    }
    if (what == "constraints") {
        if (is.null(x$constraints))
            stop("No constraints available in ", deparse(substitute(x)))
        out <- identify(x$constraints, labels=rownames(x$constraints), cex=0.7,
                        col="darkgreen") 
    }
    out
}
