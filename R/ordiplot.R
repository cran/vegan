"ordiplot" <-
    function (ord, choices = c(1, 2), type="points", ...) 
{
    if (!is.null(attr(ord, "class")) && (class(ord) == "decorana" || 
                                 any(class(ord) == "cca"))) {
        out <- plot(ord, choices, type = type, ...)
    }
    else {
        type <- match.arg(type, c("points","none"))
        X <- scores(ord, choices = choices, display = "sites")
        options(show.error.messages = FALSE)
        Y <- try(scores(ord, choices = choices, display = "species"))
        options(show.error.messages = TRUE)
        if (inherits(Y, "try-error")) {
            warning("Species scores not available")
            Y <- NULL
        }
        else if (nrow(X) == nrow(Y) && all.equal.numeric(X, Y)) {
            Y <- NULL
            warning("Species scores not available")
        }
        xlim <- range(X[, 1], Y[, 1])
        ylim <- range(X[, 2], Y[, 2])
        plot(X, xlim = xlim, ylim = ylim, asp = 1, type = "n", 
             ...)
        if (type != "none") {
            points(X, pch = 1, col = 1, cex = 0.7, ...)
            if (!is.null(Y)) 
                points(Y, pch = "+", col = "red", cex = 0.7, ...)
        }
        out <- list(sites = X, species = Y)
    }
    class(out) <- "ordiplot"
    invisible(out)
}
