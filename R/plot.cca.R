"plot.cca" <-
function (x, choices = c(1, 2), display = c("sp", "wa", "bp"), 
    scaling = 2, type, ...) 
{
    TYPES <- c("text", "points", "none")
    g <- scores.cca(x, choices, display, scaling)
    if (!is.list(g)) 
       g <- list(default = g)
    if (missing(type)) {
        nitlimit <- 80
        nit <- max(nrow(g$spe), nrow(g$sit), nrow(g$con), nrow(g$def))
        if (nit > nitlimit) 
            type <- "points"
        else type <- "text"
    }
    else type <- match.arg(type, TYPES)
    xran <- range(g$spe[, 1], g$sit[, 1], g$con[, 1], g$bip[, 
        1], g$default[,1])
    yran <- range(g$spe[, 2], g$sit[, 2], g$con[, 2], g$bip[, 
        2], g$default[,2])
    plot(g[[1]], xlim = xran, ylim = yran, type = "n", asp = 1, 
        ...)
    abline(h = 0, lty = 3)
    abline(v = 0, lty = 3)
    if (!is.null(g$species)) {
        if (type == "text") 
            text(g$species, rownames(g$species), col = "red", 
                cex = 0.7)
        else if (type == "points") 
            points(g$species, pch = "+", col = "red", cex = 0.7)
    }
    if (!is.null(g$sites)) {
        if (type == "text") 
            text(g$sites, rownames(g$sites), cex = 0.7)
        else if (type == "points") 
            points(g$sites, pch = 1, cex = 0.7)
    }
    if (!is.null(g$constraints)) {
        if (type == "text") 
            text(g$constraints, rownames(g$constraints), cex = 0.7, 
                col = "darkgreen")
        else if (type == "points") 
            points(g$constraints, pch = 2, cex = 0.7, col = "darkgreen")
    }
    if (!is.null(g$biplot) && type != "none") {
        if (length(display) > 1) 
            mul <- min(abs(xran), abs(yran))
        else mul <- 1
        arrows(0, 0, mul * g$biplot[, 1], mul * g$biplot[, 2], 
            len = 0.05, col = "blue")
        text(1.1 * mul * g$biplot, rownames(g$biplot), col = "blue")
        axis(3, at = c(-mul, 0, mul), labels = rep("", 3))
        axis(4, at = c(-mul, 0, mul), labels = c(-1, 0, 1))
    }
    if (!is.null(g$default) && type != "none") {
        if (type == "text")
            text(g$default, rownames(g$default), cex = 0.7)
        else if (type == "points")
            points(g$default, pch = 1, cex = 0.7) 
    }
    invisible()
}
