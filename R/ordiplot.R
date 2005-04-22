"ordiplot" <-
    function (ord, choices = c(1, 2), type = "points", display, ...) 
{
    if (!is.null(attr(ord, "class")) && (class(ord) == "decorana" || 
                                         any(class(ord) == "cca"))) {
        if (missing(display))
            out <- plot(ord, choices, type = type, ...)
        else
            out <- plot(ord, choices, type = type, display = display, ...)
    }
    else {
        type <- match.arg(type, c("points", "text", "none"))
        X <- Y <- NULL
        if (missing(display) || "sites" %in% display)
            X <- scores(ord, choices = choices, display = "sites")
        if (missing(display) || "species" %in% display) {
            options(show.error.messages = FALSE)
            Y <- try(scores(ord, choices = choices, display = "species"))
            options(show.error.messages = TRUE)
            if (inherits(Y, "try-error")) {
                warning("Species scores not available")
                Y <- NULL
            }
            else if (!is.null(X) && nrow(X) == nrow(Y) && all.equal.numeric(X, Y)) {
                Y <- NULL
                warning("Species scores not available")
            }
        }
        xlim <- range(X[, 1], Y[, 1])
        ylim <- range(X[, 2], Y[, 2])
        tmp <- X
        if (is.null(tmp))
            tmp <- Y
        plot(tmp, xlim = xlim, ylim = ylim, asp = 1, type = "n", 
             ...)
        if (type == "points") {
            if (!is.null(X))
                points(X, pch = 1, col = 1, cex = 0.7, ...)
            if (!is.null(Y)) 
                points(Y, pch = "+", col = "red", cex = 0.7, 
                       ...)
        }
        if (type == "text") {
            if (!is.null(X))
                text(X, labels = rownames(X), col = 1, cex = 0.7, 
                 ...)
            if (!is.null(Y)) 
                text(Y, labels = rownames(Y), col = "red", cex = 0.7, 
                     ...)
        }
        out <- list(sites = X, species = Y)
    }
    class(out) <- "ordiplot"
    invisible(out)
}