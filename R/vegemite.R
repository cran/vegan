"vegemite" <-
function (x, use, scale, sp.ind = NULL, site.ind = NULL, zero = ".") 
{
    if (!missing(use)) {
        if (!is.list(use) && is.vector(use)) {
            if (is.null(site.ind)) 
                site.ind <- order(use)
            if (is.null(sp.ind)) 
                sp.ind <- order(wascores(use, x))
        }
        else if (!is.null(class(use)) && class(use) == "hclust") {
            if (is.null(site.ind))  
                site.ind <- use$order
            if (is.null(sp.ind)) 
                sp.ind <- order(wascores(order(site.ind), x))
        }
        else if (is.list(use)) {
            tmp <- scores(use, choices=1, display="sites")
            if (is.null(site.ind)) 
                site.ind <- order(tmp)
            if (is.null(sp.ind))
               sp.ind <- try(order(scores(use, choices=1, display="species")))
            if (inherits(sp.ind, "try-error"))
               sp.ind <- order(wascores(tmp, x))
        }
        else if (is.matrix(use)) {
            tmp <- scores(use, choices=1, display="sites")
            if (is.null(site.ind)) 
                 site.ind <- order(tmp)
            if (is.null(sp.ind))
                sp.ind <- order(wascores(tmp, x))
       }
    }
    if (is.null(sp.ind)) 
        sp.ind <- 1:ncol(x)
    if (is.null(site.ind)) 
        site.ind <- 1:nrow(x)
    if (!missing(scale)) 
        x <- coverscale(x, scale)
    if (any(apply(x, 1, nchar) > 1))
        stop("Cowardly refusing to use more than 1 char symbols:\nUse scale")
    x <- x[site.ind, sp.ind]
    x <- t(x)
    sp.nam <- rownames(x)
    sp.len <- max(nchar(sp.nam))
    nst <- ncol(x)
    page.width <- getOption("width")
    per.page <- page.width - sp.len - 3
    istart <- seq(1, nst, by=per.page)
    iend <- pmin(istart+per.page-1, nst)
    for (st in 1:length(istart)) {
      tbl <- apply(x[,istart[st]:iend[st]], 1, paste, sep = "", collapse = "")
      names(tbl) <- NULL
      tbl <- gsub("0", zero, tbl)
      tbl <- cbind(sp.nam, tbl)
      st.nam <- colnames(x)[istart[st]:iend[st]]
      nlen <- max(nchar(st.nam))
      mathead <- matrix(" ", nrow = length(st.nam), ncol = nlen)
      for (i in 1:length(st.nam)) {
        tmp <- unlist(strsplit(st.nam[i], NULL))
        start <- nlen - length(tmp) + 1
        mathead[i, start:nlen] <- tmp
      }
      head <- cbind(apply(mathead, 2, paste, sep = "", collapse = ""))
      tbl <- rbind(cbind(matrix(" ", nrow = nrow(head), 1), head), 
                   tbl)
      d <- list()
      l <- 0
      for (i in dim(tbl)) {
        d[[l <- l + 1]] <- rep("", i)
      }
      dimnames(tbl) <- d
      print(noquote(tbl))
  }
    out <- list(sites = site.ind, spec = sp.ind)
    invisible(out)
}
