"plot.envfit" <-
    function (x, choices = c(1, 2), arrow.mul = 1, col = "blue", add = TRUE, ...) 
{
    if (!is.null(x$vectors)) {
        vect <- arrow.mul * sqrt(x$vectors$r) * x$vectors$arrows[,choices]
        if (!any(dim(vect))) 
            dim(vect) <- c(1, length(vect))
    }
    if (!add) {
        xlim <- range(x$vectors$arrows[, choices[2]], x$factors$centroids[, choices[2]])
        ylim <- range(x$vectors$arrows[, choices[1]], x$factors$centroids[, choices[1]])
        if (!is.null(x$vectors)) 
            plot(x$vectors$arrows[, choices], xlim = xlim, ylim = ylim, 
                 asp = 1, type = "n", ...)
        else if (!is.null(x$factors)) 
            plot(x$factors$centroids[, choices], asp = 1, xlim = xlim, 
                 ylim = ylim, type = "n", ...)
        else stop("Nothing to plot")
    }
    if (!is.null(x$vectors)) {
        arrows(0, 0, vect[, 1], vect[, 2], len = 0.05, col = col)
        text(1.1 * vect, rownames(x$vectors$arrows), col = col, ...)
    }
    if (!is.null(x$factors)) {
        text(x$factors$centroids[,choices], rownames(x$factors$centroids),
             col = col, ...)
    }
    invisible()
}
