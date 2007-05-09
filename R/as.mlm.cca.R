`as.mlm.cca` <-
function(x)
{
    w <- weights(x)
    wa <- x$CCA$wa
    wa <- sweep(wa, 1, sqrt(w), "*")
    lm(wa ~ . - 1, data=as.data.frame(qr.X(x$CCA$QR)))
}

