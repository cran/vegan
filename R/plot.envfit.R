"plot.envfit" <-
    function (x, choices = c(1, 2), arrow.mul = 1, p.max = NULL, 
              col = "blue", add = TRUE, ...) 
{
    formals(arrows) <- c(formals(arrows), alist(... = ))
    if (!is.null(p.max)) {
        if (!is.null(x$vectors)) {
            take <- x$vectors$pvals <= p.max
            x$vectors$arrows <- x$vectors$arrows[take, , drop = FALSE]
            x$vectors$r <- x$vectors$r[take]
            if (nrow(x$vectors$arrows) == 0) 
                x$vectors <- NULL
        }
        if (!is.null(x$factors)) {
            tmp <- x$factors$pvals <= p.max
            nam <- names(tmp)[tmp]
            take <- x$factors$var.id %in% nam
            x$factors$centroids <- x$factors$centroids[take, 
                                                       , drop = FALSE]
            if (nrow(x$factors$centroids) == 0) 
                x$factors <- NULL
        }
    }
    if (!is.null(x$vectors)) {
        vect <- arrow.mul * sqrt(x$vectors$r) *
            x$vectors$arrows[, choices, drop=FALSE]
    }
    if (!add) {
        xlim <- range(x$vectors$arrows[, choices[2]], x$factors$centroids[, 
                                                                          choices[2]])
        ylim <- range(x$vectors$arrows[, choices[1]], x$factors$centroids[, 
                                                                          choices[1]])
        if (!is.null(x$vectors)) 
            plot(x$vectors$arrows[, choices, drop=FALSE], xlim = xlim, ylim = ylim, 
                 asp = 1, type = "n", ...)
        else if (!is.null(x$factors)) 
            plot(x$factors$centroids[, choices, drop=FALSE], asp = 1, xlim = xlim, 
                 ylim = ylim, type = "n", ...)
        else stop("Nothing to plot")
    }
    if (!is.null(x$vectors)) {
        arrows(0, 0, vect[, 1], vect[, 2], len = 0.05, col = col)
        text(1.1 * vect, rownames(x$vectors$arrows), col = col, 
             ...)
    }
    if (!is.null(x$factors)) {
        text(x$factors$centroids[, choices, drop=FALSE], rownames(x$factors$centroids), 
             col = col, ...)
    }
    invisible()
}
