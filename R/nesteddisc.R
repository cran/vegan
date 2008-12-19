"nesteddisc" <-
function(comm)
{
    comm <- ifelse(comm > 0, 1, 0)
    rs <- colSums(comm)
    j <- rev(order(rs))
    comm <- comm[, j] 
    Ad <- sum(comm[col(comm) <= rowSums(comm)] == 0)
    out <- list(statistic=Ad, ties = length(unique(rs)) < length(rs))
    class(out) <- "nesteddisc"
    out
}
