`as.mlm.rda` <-
function(x)
{
    lm(x$CCA$wa ~ . - 1, data = as.data.frame(qr.X(x$CCA$QR)))
}

