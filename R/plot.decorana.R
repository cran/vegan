"plot.decorana" <-
  function (x, choices = c(1, 2), origin=TRUE,
            display = c("both", "sites", "species", "none"), cex = 0.8,
            tol = 0.1, cols = c(1, 2), ...) 
{
  display <- match.arg(display)
  sites <- x$rproj
  specs <- x$cproj
  if (origin) {
    sites <- sweep(x$rproj, 2, x$origin, "-")
    specs <- sweep(x$cproj, 2, x$origin, "-")
  }
  sitnam <- rownames(x$rproj)
  spenam <- rownames(x$cproj)
  sites <- sites[, choices]
  specs <- specs[, choices]
  sp.x <- range(specs[, 1])
  sp.y <- range(specs[, 2])
  st.x <- range(sites[, 1])
  st.y <- range(sites[, 2])
  switch(display, both = {
    xlim <- range(sp.x, st.x)
    ylim <- range(sp.y, st.y)
  }, sites = {
    xlim <- st.x
    ylim <- st.y
  }, species = {
    xlim <- sp.x
    ylim <- sp.y
  }, none= {
    xlim <- range(sp.x, st.x)
    ylim <- range(sp.y, st.y)
  })
  midx <- mean(xlim)
  midy <- mean(ylim)
  xlim <- midx + (1 + tol) * 0.5 * c(-1, 1) * (xlim[2] - xlim[1])
  ylim <- midy + (1 + tol) * 0.5 * c(-1, 1) * (ylim[2] - ylim[1])
  oldpin <- par("pin")
  xuin <- oxuin <- oldpin[1]/diff(xlim)
  yuin <- oyuin <- oldpin[2]/diff(ylim)
  if (yuin > xuin) 
    yuin <- xuin
  else xuin <- yuin
  xlim <- midx + oxuin/xuin * c(-1, 1) * diff(xlim) * 0.5
  ylim <- midy + oyuin/yuin * c(-1, 1) * diff(ylim) * 0.5
  plot(sites, type = "n", xlim = xlim, ylim = ylim, ...)
  if (display == "both" || display == "sites") 
    text(sites, sitnam, cex = cex, col = cols[1])
  if (display == "both" || display == "species") 
    text(specs, spenam, cex = cex, col = cols[2])
  invisible()
}
