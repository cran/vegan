"ordispantree" <-
    function(ord, tree, display = "sites",  ...)
{
    ord <- scores(ord, display = display)
    if (is.list(tree) && !is.null(tree$kid))
        tree <- tree$kid
    segments(ord[-1,1], ord[-1,2], ord[tree, 1], ord[tree, 2], ...)
    invisible()
}
