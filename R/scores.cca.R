"scores.cca" <-
    function (x, choices = c(1, 2), display = c("sp", "wa", "cn"), 
              scaling = 2, ...) 
{
    tabula <- c("species", "sites", "constraints", "biplot", 
                "centroids")
    names(tabula) <- c("sp", "wa", "lc", "bp", "cn")
    if (is.null(x$CCA)) 
        tabula <- tabula[1:2]
    if (length(display) == 1) {
        display <- match.arg(display, c("sites", "species", "wa", 
                                        "lc", "bp", "cn"))
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
        sol$biplot <- sol$biplot[, choices, drop = FALSE]
    }
    if ("centroids" %in% take && any(choices != 1) && !is.na(sol$centroids)) {
        sol$centroids <- as.matrix(sol$centroids)
        nc <- ncol(sol$centroids)
        nr <- nrow(sol$centroids)
        if (nc < max.ax) {
            tmp <- matrix(0, nrow = nr, ncol = (max.ax - nc))
            sol$centroids <- cbind(sol$centroids, tmp)
        }
        sol$centroids <- sol$centroids[, choices, drop = FALSE]
    }
    if (length(sol) == 1) 
        sol <- sol[[1]]
    return(sol)
}
