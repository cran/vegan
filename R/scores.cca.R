"scores.cca" <-
function (x, choices = c(1, 2), display = c("sp", "wa", "bp"), 
    scaling = 2, ...) 
{
    tabula <- c("species", "sites", "constraints", "biplot")
    if (is.null(x$CCA)) 
        names(tabula) <- c("sp", "wa", NA, NA)
    else names(tabula) <- c("sp", "wa", "lc", "bp")
    if (length(display) == 1) {
        display <- match.arg(display, c("sites", "species", "wa", 
            "lc", "bp"))
        if (display == "sites") 
            display <- "wa"
        else if (display == "species") 
            display <- "sp"
    }
    take <- tabula[display]
    max.ax <- max(choices)
    sol <- summary(x, max.ax, scaling = scaling)[take]
    if ("species" %in% take && any(choices != 1)) 
        sol$species <- sol$species[, choices]
    if ("sites" %in% take && any(choices != 1)) 
        sol$sites <- sol$sites[, choices]
    if ("constraints" %in% take && any(choices != 1)) {
        sol$constraints <- as.matrix(sol$constraints)
        nc <- ncol(sol$constraints)
        nr <- nrow(sol$constraints)
        if (nc < max.ax) {
            tmp <- matrix(0, nrow = nr, ncol = (max.ax - nc))
            sol$constraints <- cbind(sol$constraints, tmp)
        }
        sol$constraints <- sol$constraints[, choices]
    }
    if ("biplot" %in% take && any(choices != 1)) {
        sol$biplot <- as.matrix(sol$biplot)
        nc <- ncol(sol$biplot)
        nr <- nrow(sol$biplot)
        if (nc < max.ax) {
            tmp <- matrix(0, nrow = nr, ncol = (max.ax - nc))
            sol$biplot <- cbind(sol$biplot, tmp)
        }
        sol$biplot <- sol$biplot[, choices]
        if (nr == 1) 
            dim(sol$biplot) <- c(nr, length(choices))
    }
    if (length(sol) == 1) 
        sol <- sol[[1]]
    return(sol)
}
