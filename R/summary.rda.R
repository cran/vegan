"summary.rda" <-
    function (object, scaling = 2, axes = 6, digits = max(3, getOption("digits") - 
                                             3), ...) 
{
    axes <- min(axes, sum(object$CCA$rank, object$CA$rank))
    summ <- object[c("call", "tot.chi")]
    summ$partial.chi <- object$pCCA$tot.chi
    summ$constr.chi <- object$CCA$tot.chi
    summ$unconst.chi <- object$CA$tot.chi
    summ$ev.con <- object$CCA$eig
    summ$ev.uncon <- object$CA$eig
    ev.account <- summ$tot.chi
    if (!is.null(object$pCCA)) 
        ev.account <- ev.account - summ$partial.chi
    summ$ev.con.account <- cumsum(summ$ev.con)/ev.account
    summ$ev.uncon.account <- cumsum(summ$ev.uncon)/ev.account
    summ$ev.head <- c(summ$ev.con, summ$ev.uncon)[1:axes]
    summ$scaling <- scaling
    cc.dim <- min(object$CCA$rank, axes)
    if (is.null(object$CCA)) 
        cc.dim <- 0
    biplot <- object$CCA$biplot[, 1:cc.dim, drop = FALSE]
    add.dim <- axes - cc.dim
    species <- object$CCA$v[, 1:cc.dim, drop = FALSE]
    sites <- object$CCA$wa[, 1:cc.dim, drop = FALSE]
    centroids <- NA
    if (!is.null(object$CCA$centroids) && !is.na(object$CCA$centroids[1])) 
        centroids <- object$CCA$centroids[, 1:cc.dim, drop = FALSE]
    site.constr <- object$CCA$u[, 1:cc.dim, drop = FALSE]
    sum.ev <- object$tot.chi
    if (is.null(object$CCA$u)) 
        nr <- nrow(object$CA$u)
    else nr <- nrow(object$CCA$u)
    const <- sqrt(sqrt((nr - 1) * sum.ev))
    evscale <- sqrt(summ$ev.con[1:cc.dim]/sum.ev)
    if (abs(scaling) == 2) {
        if (cc.dim) {
            species <- sweep(species, 2, evscale, "*")
        }
        if (add.dim) {
            evscale0 <- sqrt(summ$ev.uncon[1:add.dim]/sum.ev)
            tmp <- object$CA$v[, 1:add.dim, drop = FALSE]
            tmp <- sweep(tmp, 2, evscale0, "*")
            species <- cbind(species, tmp)
            sites <- cbind(sites, object$CA$u[, 1:add.dim, drop = FALSE])
        }
    }
    if (abs(scaling) == 1) {
        if (cc.dim) {
            site.constr <- sweep(site.constr, 2, evscale, "*")
            sites <- sweep(sites, 2, evscale, "*")
            if (!is.na(centroids)[1]) 
                centroids <- sweep(centroids, 2, evscale, "*")
        }
        if (add.dim) {
            evscale0 <- sqrt(summ$ev.uncon[1:add.dim]/sum.ev)
            species <- cbind(species, object$CA$v[, 1:add.dim, 
                                                  drop = FALSE])
            tmp <- object$CA$u[, 1:add.dim, drop = FALSE]
            tmp <- sweep(tmp, 2, evscale0, "*")
            sites <- cbind(sites, tmp)
        }
    }
    if (abs(scaling) == 3) {
        if (cc.dim) {
            species <- sweep(species, 2, sqrt(evscale), "*")
            sites <- sweep(sites, 2, sqrt(evscale), "*")
            if (!is.na(centroids)[1]) 
                centroids <- sweep(centroids, 2, sqrt(evscale), 
                                   "*")
            site.constr <- sweep(site.constr, 2, sqrt(evscale), 
                                 "*")
        }
        if (add.dim) {
            evscale0 <- sqrt(sqrt(summ$ev.uncon[1:add.dim]/sum.ev))
            tmp <- object$CA$u[, 1:add.dim, drop = FALSE]
            tmp <- sweep(tmp, 2, evscale0, "*")
            sites <- cbind(sites, tmp)
            tmp <- object$CA$v[, 1:add.dim, drop = FALSE]
            tmp <- sweep(tmp, 2, evscale0, "*")
            species <- cbind(species, tmp)
        }
    }
    summ$species <- const * species
    summ$sites <- const * sites
    summ$constraints <- const * site.constr
    summ$biplot <- biplot
    summ$centroids <- const * centroids
    summ$digits <- digits
    summ$inertia <- object$inertia
    summ$method <- object$method
    class(summ) <- "summary.cca"
    summ
}
