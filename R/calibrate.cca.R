"calibrate.cca" <-
    function(object, newdata, rank = "full", ...)
{
    if (!is.null(object$pCCA))
        stop("does not work with conditioned (partial) models")
    if (is.null(object$CCA))
        stop("needs constrained model")
    if (rank != "full")
        rank <- min(rank, object$CCA$rank)
    else
        rank <- object$CCA$rank
    if (missing(newdata))
        wa <- object$CCA$wa        
    else
        wa <- predict(object, type="wa", newdata=newdata)
    b <- (coef(object))[object$CCA$QR$pivot[1:object$CCA$rank], , drop=FALSE]
    b <- solve(b)
    pred <- wa[ , 1:rank, drop=FALSE]  %*% b[1:rank, , drop =FALSE]
    pred <- sweep(pred, 2, object$CCA$envcentre, "+")
    pred
}

