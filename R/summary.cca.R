`summary.cca` <- function (object, scaling = "species", axes = 6,
                           display=c("sp","wa","lc","bp","cn"),
                           digits = max(3, getOption("digits") - 3),
                           correlation = FALSE, hill = FALSE, ...)
{
    axes <- min(axes, sum(object$CCA$rank, object$CA$rank))
    summ <- list()
    ## scaling is stored in return object so must be in numeric format
    scaling <- scalingType(scaling = scaling, correlation = correlation,
                           hill = hill)
    if (axes && length(display) && !anyNA(display) && !is.null(display))
        summ <- scores(object, scaling = scaling, choices = 1:axes, display = display,
                       ...)
    ## scores() drops list to a matrix if there is only one item: workaround below.
    if (!is.list(summ) && length(display) == 1) {
        nms <- c("species", "sites", "constraints", "biplot", "centroids")
        names(nms) <- c("sp","wa","lc","bp","cn")
        summ <- list(summ)
        names(summ) <- nms[display]
    }
    if (length(display) > 0) {
        for (i in seq_along(summ)) {
            if (is.matrix(summ[[i]]))
                rownames(summ[[i]]) <-
                    rownames(summ[[i]], do.NULL = FALSE,
                             prefix = substr(names(summ)[i], 1, 3))
        }
    }
    summ$call <- object$call
    summ$tot.chi <- object$tot.chi
    ## only the Real component for capscale() with negative eigenvalues
    if (!is.null(object$CA$imaginary.chi))
        summ$tot.chi <- summ$tot.chi - object$CA$imaginary.chi
    summ$partial.chi <- object$pCCA$tot.chi
    summ$constr.chi <- object$CCA$tot.chi
    summ$unconst.chi <- object$CA$tot.chi
    ## nested list cont$importance needed to keep vegan pre-2.5-0 compatibility
    summ$cont$importance <- summary(eigenvals(object))
    if (!is.null(object$CCA) && object$CCA$rank > 0)
        summ$concont$importance <- summary(eigenvals(object, model = "constrained"))
    summ$ev.head <- c(summ$ev.con, summ$ev.uncon)[seq_len(axes)]
    summ$scaling <- scaling
    summ$digits <- digits
    summ$inertia <- object$inertia
    summ$method <- object$method
    class(summ) <- "summary.cca"
    summ
}
