"radfit.default" <-
    function (x, ...) 
{
    x <- as.rad(x)
    PE <- rad.preempt(x, ...)
    LN <- rad.lognormal(x, ...)
    VL <- rad.veil(x, ...)
    ZP <- rad.zipf(x, ...)
    ZM <- rad.zipfbrot(x, ...)
    out <- list(y = x, family = PE$family)
    models <- list(Preemption = PE, Lognormal = LN, Veiled.LN = VL, 
                   Zipf = ZP, Mandelbrot = ZM)
    out$models <- models
    class(out) <- "radfit"
    out
}
