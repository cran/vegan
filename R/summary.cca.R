"summary.cca" <-
function (object, scaling = 2, axes = 6, digits = max(3, getOption("digits") - 
    3), ...) 
{
    axes <- min(axes, sum(object$CCA$rank, object$CA$rank))
    summ <- object[c("Call", "tot.chi")]
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
    biplot <- object$CCA$biplot[, 1:cc.dim]
    add.dim <- axes - cc.dim
    if (scaling == 2) {
        species <- object$CCA$v.eig[, 1:cc.dim]
        sites <- object$CCA$wa[, 1:cc.dim]
        site.constr <- object$CCA$u[, 1:cc.dim]
        if (add.dim) {
            species <- cbind(species, object$CA$v.eig[, 1:add.dim])
            sites <- cbind(sites, object$CA$u[, 1:add.dim])
        }
    }
    if (scaling == 1) {
        species <- object$CCA$v[, 1:cc.dim]
        sites <- object$CCA$wa.eig[, 1:cc.dim]
        site.constr <- object$CCA$u.eig[, 1:cc.dim]
        if (add.dim) {
            species <- cbind(species, object$CA$v[, 1:add.dim])
            sites <- cbind(sites, object$CA$u.eig[, 1:add.dim])
        }
    }
    if (cc.dim == 1) {
        colnames(species)[1] <- "CCA1"
        colnames(sites)[1] <- "CCA1"
    }
    if (add.dim == 1) {
        colnames(species)[axes] <- "CA1"
        colnames(sites)[axes] <- "CA1"
    }
    summ$species <- species
    summ$sites <- sites
    summ$constraints <- site.constr
    summ$biplot <- biplot
    summ$digits <- digits
    class(summ) <- "summary.cca"
    summ
}
