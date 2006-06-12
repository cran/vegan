intersetcor <- function(object) 
{
    w <- weights(object)
    wa <- sweep(object$CCA$wa, 1, sqrt(w), "*")
    cor(qr.X(object$CCA$QR), wa)
}
