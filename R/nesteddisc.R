"nesteddisc" <-
function(comm)
{
    comm <- ifelse(comm > 0, 1, 0)
    j <- rev(order(colSums(comm)))
    comm <- comm[, j] 
    Ad <- sum(comm[col(comm) <= rowSums(comm)] == 0)
    out <- list(statistic=Ad)
    class(out) <- "nesteddisc"
    out
}

