"scores.default" <-
    function (x, display = c("sites", "species"), choices, ...) 
{
    display <- match.arg(display)
    att <- attributes(x)$names
    if (is.list(x) && display == "sites") {
        if ("points" %in% att) 
            X <- x$points
        else if ("rproj" %in% att) 
            X <- x$rproj
        else if ("x" %in% att) 
            X <- x$x
        else if ("scores" %in% att) 
            X <- x$scores
        else if ("sites" %in% att)
            X <- x$sites
        else stop("Can't find scores")
    }
    else if (is.list(x) && display == "species") {
        if ("species" %in% att)
            X <- x$species
        else if ("cproj" %in% att) 
            X <- x$cproj
        else if ("rotation" %in% att) 
            X <- x$rotation
        else if ("loadings" %in% att) 
            X <- x$loadings
        else stop("Can't find scores")
    }
    else if (is.matrix(x)) 
        X <- x
    if (is.null(rownames(X))) {
        root <- substr(display, 1, 4)
        rownames(X) <- paste(root, 1:nrow(X), sep = "")
    }
    if (is.null(colnames(X))) 
        colnames(X) <- paste("Dim", 1:ncol(X), sep = "")
    if (!missing(choices)) 
        X <- X[, choices]
    X <- as.matrix(X)
    X
}
