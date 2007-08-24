bgdispersal <- function (mat, PAonly = FALSE) 
{
    mat <- as.matrix(mat)
    if (sum((mat - decostand(mat, "pa"))) == 0) {
        PAonly <- TRUE
        mat1 <- mat
    }
    else {
        mat1 <- decostand(mat, "pa")
        if (PAonly == FALSE) mat2 <- mat
    }
    n <- nrow(mat)
    p <- ncol(mat)
    a <- mat1 %*% t(mat1)
    b <- mat1 %*% (1 - t(mat1))
    c <- (1 - mat1) %*% t(mat1)
    d <- ncol(mat1) - a - b - c
    DD1 <- (a * (b - c))/((a + b + c)^2)
    DD2 <- (2 * a * (b - c))/((2 * a + b + c) * (a + b + c))
    McNemar <- (abs(b - c) - 1)^2/(b + c)
    diag(McNemar) <- 0
    pP.Mc <- matrix(NA, n, n)
    for (j in 1:(n - 1)) {
        for (jj in (j + 1):n) {
            pP.Mc[j, jj] <- pchisq(McNemar[j, jj], 1, lower.tail = FALSE)
            if ((b[j, jj] + c[j, jj]) == 0) 
                pP.Mc[j, jj] <- 1
        }
    }
    if (!PAonly) {
        DD3 <- matrix(0, n, n)
        DD4 <- matrix(0, n, n)
        row.sum <- apply(mat2, 1, sum)
        for (j in 1:(n - 1)) {
            for (jj in (j + 1):n) {
                W <- sum(apply(mat2[c(j, jj), ], 2, min))
                A <- row.sum[j]
                B <- row.sum[jj]
                temp3 <- W * (A - B)/((A + B - W)^2)
                temp4 <- 2 * W * (A - B)/((A + B) * (A + B - 
                  W))
                DD3[j, jj] <- temp3
                DD3[jj, j] <- -temp3
                DD4[j, jj] <- temp4
                DD4[jj, j] <- -temp4
            }
        }
        return(list(DD1 = DD1, DD2 = DD2, DD3 = DD3, DD4 = DD4, 
            McNemar = McNemar, prob.McNemar = pP.Mc))
    }
    else {
        return(list(DD1 = DD1, DD2 = DD2, McNemar = McNemar, 
            prob.McNemar = pP.Mc))
    }
}
