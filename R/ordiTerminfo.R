"ordiTerminfo" <-
    function(d, data)
{
    Terms <- delete.response(d$terms.expand)
    mf <- model.frame(formula(Terms), data)
    xlev <- .getXlevels(Terms, mf)
    ordered <- sapply(mf, is.ordered)
    list(terms = Terms, xlev = xlev, ordered = ordered)
}

