`orditkplot` <-
    function(x, display = "species", choices = 1:2, width, xlim, ylim,
             tcex=0.8, pcol, pbg, pcex = 0.7,
             labels,  ...)
{
    if (!capabilities("tcltk"))
        stop("Your R has no capability for Tcl/Tk")
    require(tcltk) || stop("requires package tcltk")
    ## Graphical parameters and constants, and save some for later plotting
    p <- par()
    sparnam <- c("bg","cex", "cex.axis","cex.lab","col", "col.axis", "col.lab",
                 "family", "fg", "font", "font.axis", "font.lab", "lheight",
                 "lwd", "mar", "mex", "mgp", "ps", "tcl", "las")
    ## Get par given in the command line and put them to p
    dots <- match.call(expand.dots = FALSE)$...
    if (length(dots) > 0) {
        dots <- dots[names(dots) %in% sparnam]
        ## eval() or mar=c(4,4,1,1) will be a call, not numeric
        dots <- lapply(dots, function(x) if (is.call(x)) eval(x) else x)
        p <- check.options(new = dots, name.opt = "p", envir = environment())
    }
    savepar <- p[sparnam]
    PPI <- 72                                         # Points per Inch
    p2p <- as.numeric(tclvalue(tcl("tk", "scaling"))) # Pixel per point
    DIAM <- 2.7                               # diam of plotting symbol
    ## Plotting symbol diam
    diam <- round(pcex * DIAM * p2p, 1)
    ## Sanitize colours
    sanecol <- function(x) {
        if (is.na(x))
            x <- ""
        else if (is.numeric(x))
            x <- palette()[x]
        else if (x == "transparent")
            x <- ""
        x
    } 
    p$bg <- sanecol(p$bg)
    p$fg <- sanecol(p$fg)
    p$col <- sanecol(p$col)
    p$col.axis <- sanecol(p$col.axis)
    p$col.lab <- sanecol(p$col.lab)
    ## Point colours
    if (missing(pcol))
        pcol <- p$col
    if (missing(pbg))
        pbg <- "transparent"
    pcol <- sanecol(pcol)
    pbg <- sanecol(pbg)
    ## Define fonts
    idx <- match(p$family, c("","serif","sans","mono"))
    if (!is.na(idx))
        p$family <- c("Helvetica", "Times", "Helvetica", "Courier")[idx]
    saneslant <- function(x) {
        list("roman", "bold", "italic", c("bold", "italic"))[[x]]
    }
    fnt <- c(p$family, round(p$ps*p$cex*tcex), saneslant(p$font))
    fnt.axis <- c(p$family, round(p$ps*p$cex.axis), saneslant(p$font.axis))
    fnt.lab <- c(p$family, round(p$ps*p$cex.lab), saneslant(p$font.lab))
    ## toplevel
    w <- tktoplevel()
    tktitle(w) <- deparse(match.call())
    ## Max dim of windows (depends on screen)
    YSCR <- as.numeric(tkwinfo("screenheight", w)) - 150
    XSCR <- as.numeric(tkwinfo("screenwidth", w))
    ## Buttons
    buts <- tkframe(w)
    ## Copy current canvas to EPS using the standard Tcl/Tk utility
    cp2eps <- tkbutton(buts, text="Copy to EPS", 
                       command=function() tkpostscript(can, x=0, y=0,
                       height=height, width=width, 
                       file=tkgetSaveFile(filetypes="{{EPS file} {.eps}}")))
    dismiss <- tkbutton(buts, text="Dismiss", command=function() tkdestroy(w))
    ## Dump current plot to an "orditkplot" object (internally)
    ordDump <- function() {
        xy <- matrix(0, nrow=nrow(sco), ncol=2)
        rownames(xy) <- rownames(sco)
        colnames(xy) <- colnames(sco)
        for(nm in names(pola)) {
            xy[as.numeric(tclvalue(id[[nm]])),] <- xy2usr(nm)
        }
        curdim <- round(c(width, height) /PPI/p2p, 2)
        if (pbg == "")
            pbg <- "transparent"
        if (pcol == "")
            pcol <- "transparent"
        args <- list(tcex = tcex, pcol = pcol, pbg = pbg, pcex = pcex,
                     xlim = xlim, ylim = ylim)
        xy <- list(labels = xy, points = sco, par = savepar, args = args,
                   dim = curdim)
        class(xy) <- "orditkplot"
        xy
    }        
    ## Button to dump "orditkplot" object to the R session
    pDump <- function() {
        xy <- ordDump()
        dumpVar <- tclVar("")
        tt <- tktoplevel()
        tktitle(tt) <- "R Dump"
        entryDump <- tkentry(tt, width=20, textvariable=dumpVar)
        tkgrid(tklabel(tt, text="Enter name for an R object"))
        tkgrid(entryDump, pady="5m")
        isDone <- function() {
            dumpName <- tclvalue(dumpVar)
            if (exists(dumpName, envir=.GlobalEnv)) {
                ok <- tkmessageBox(message=paste(sQuote(dumpName),
                                   "exists.\nOK to overwrite?"),
                                   icon="warning", type="okcancel",
                                   default="ok")
                if(tclvalue(ok) == "ok") {
                    assign(dumpName, xy, envir=.GlobalEnv)
                    tkdestroy(tt)
                }
            }
            else {
                assign(dumpName, xy, envir=.GlobalEnv)
                tkdestroy(tt)
            }
        }
        tkbind(entryDump, "<Return>", isDone)
        tkfocus(tt)
    }
    dump <- tkbutton(buts, text="Dump to R", command=pDump)
    ## Button to write current "orditkplot" object to a graphical device
    devDump <- function() {
        xy <- ordDump()
        ftypes <- c("eps" = "{EPS File} {.eps}",
                    "pdf" = "{PDF File} {.pdf}",
                    "png" = "{PNG File} {.png}",
                    "jpg" = "{JPEG File} {.jpg .jpeg}",
                    "bmp" = "{BMP File} {.bmp}",
                    "fig" = "{XFig File} {.fig}")
        falt <- rep(TRUE, length(ftypes))
        names(falt) <- names(ftypes)
        if (!capabilities("png"))
            falt["png"] <- FALSE
        if (!capabilities("jpeg"))
            falt["jpg"] <- FALSE
        if (.Platform$OS.type != "windows")
            falt["bmp"] <- FALSE
        ftypes <- ftypes[falt]
        fname <- tkgetSaveFile(filetypes=ftypes)
        if(tclvalue(fname) == "")
            return(NULL)
        fname <- tclvalue(fname)
        ftype <- unlist(strsplit(fname, "\\."))
        ftype <- ftype[length(ftype)]
        if (ftype == "jpeg")
            ftype <- "jpg"
        mess <- "is not a supported type: file not produced. Supported types are"
        if (!(ftype %in% names(ftypes))) {
            tkmessageBox(message=paste(sQuote(ftype), mess, paste(names(ftypes),
                         collapse=", ")), icon="warning")
            return(NULL)
        }
        pixdim <- round(xy$dim*PPI*p2p)
        switch(ftype,
               eps = postscript(file=fname, width=xy$dim[1], height=xy$dim[2],
                     paper="special", horizontal = FALSE),
               pdf = pdf(file=fname, width=xy$dim[1], height=xy$dim[2]),
               png = png(file=fname, width=pixdim[1], height=pixdim[2]),
               jpg = jpeg(file=fname, width=pixdim[1], height=pixdim[2],
                     quality = 100),
               bmp = bmp(file=fname, width=pixdim[1], height=pixdim[2]),
               fig = xfig(file=fname, width=xy$dim[1], height=xy$dim[2]))
        plot.orditkplot(xy)
        dev.off()
    }
    export <- tkbutton(buts, text="Export plot", command=devDump)
    ## Make canvas
    sco <- try(scores(x, display=display, choices = choices, ...),
               silent = TRUE)
    if (inherits(sco, "try-error")) {
        tkmessageBox(message=paste("No ordination scores were found in",
                     sQuote(deparse(substitute(x)))), icon="error")
        tkdestroy(w)
        stop("argument x did not contain ordination scores")
    }
    if (!missing(labels))
        rownames(sco) <- labels
    if (!missing(xlim))
        sco <- sco[sco[,1] >= xlim[1] & sco[,1] <= xlim[2], , drop = FALSE]
    if (!missing(ylim))
        sco <- sco[sco[,2] >= ylim[1] & sco[,2] <= ylim[2], , drop = FALSE]
    labs <- rownames(sco)
    ## Ranges and pretty values for axes
    if (missing(xlim))
        xlim <- range(sco[,1])
    if (missing(ylim))
        ylim <- range(sco[,2])
    xpretty <- pretty(xlim)
    ypretty <- pretty(ylim)
    ## Extend ranges by 4% 
    xrange <- c(-0.04, 0.04) * diff(xlim) + xlim
    xpretty <- xpretty[xpretty >= xrange[1] & xpretty <= xrange[2]]
    yrange <- c(-0.04, 0.04) * diff(ylim) + ylim
    ypretty <- ypretty[ypretty >= yrange[1] & ypretty <= yrange[2]]
    ## Canvas like they were in the default devices when I last checked
    if (missing(width)) 
        width <- p$din[1]
    width <- width * PPI * p2p
    ## Margin row width also varies with platform and devices
    ## rpix <- (p$mai/p$mar * PPI * p2p)[1]
    rpix <- p$cra[2]
    mar <- round(p$mar * rpix)
    xusr <- width - mar[2] - mar[4]
    xincr <- xusr/diff(xrange)
    yincr <- xincr
    xy0 <- c(xrange[1], yrange[2]) # upper left corner
    ## Functions to translate scores to canvas coordinates and back
    usr2xy <- function(row) {
        x <- (row[1] - xy0[1]) * xincr + mar[2]
        y <- (xy0[2] - row[2]) * yincr + mar[3]
        c(x,y)
    }
    xy2usr <- function(item) {
        xy <- as.numeric(tkcoords(can, item))
        x <- xy[1] 
        y <- xy[2] 
        x <- xrange[1] + (x - mar[2])/xincr 
        y <- yrange[2] - (y - mar[3])/yincr 
        c(x,y)
    }
    ## Equal aspect ratio
    height <- round((diff(yrange)/diff(xrange)) * xusr)
    height <- height + mar[1] + mar[3]
    ## Canvas, finally
    can <- tkcanvas(w, relief="sunken", width=width, height=min(height,YSCR),
                    scrollregion=c(0,0,width,height))
    if (p$bg != "")
        tkconfigure(can, bg=p$bg)
    yscr <- tkscrollbar(w, command = function(...) tkyview(can, ...))
    tkconfigure(can, yscrollcommand = function(...) tkset(yscr, ...))
    ## Pack it up
    tkpack(buts, side="bottom", fill="x", pady="2m")
    tkpack(can, side="left", fill="x")
    tkpack(yscr, side="right", fill="y")
    tkgrid(cp2eps, export, dump, dismiss, sticky="s")
    ## Box
    x0 <- usr2xy(c(xrange[1], yrange[1]))
    x1 <- usr2xy(c(xrange[2], yrange[2]))
    tkcreate(can, "rectangle", x0[1], x0[2], x1[1], x1[2], outline = p$fg,
             width = p$lwd)
    ## Axes and ticks
    tl <-  -p$tcl * rpix     # -p$tcl * p$ps * p2p
    axoff <- p$mgp[3] * rpix
    tmp <- xpretty
    for (i in 1:length(tmp)) {
        x0 <- usr2xy(c(xpretty[1], yrange[1]))
        x1 <- usr2xy(c(xpretty[length(xpretty)], yrange[1]))
        tkcreate(can, "line", x0[1], x0[2]+axoff, x1[1], x1[2]+axoff,
                 fill=p$fg)
        xx <- usr2xy(c(tmp[i], yrange[1]))
        tkcreate(can, "line", xx[1], xx[2] + axoff, xx[1], xx[2]+tl+axoff,
                 fill=p$fg)
        tkcreate(can, "text", xx[1], xx[2] + rpix * p$mgp[2], anchor="n",
                 text=as.character(tmp[i]), fill=p$col.axis, font=fnt.axis)
    }
    xx <- usr2xy(c(mean(xrange), yrange[1]))
    tkcreate(can, "text", xx[1], xx[2] + rpix * p$mgp[1],
             text=colnames(sco)[1], fill=p$col.lab, anchor="n", font=fnt.lab)
    tmp <- ypretty
    for (i in 1:length(tmp)) {
        x0 <- usr2xy(c(xrange[1], tmp[1]))
        x1 <- usr2xy(c(xrange[1], tmp[length(tmp)]))
        tkcreate(can, "line", x0[1]-axoff, x0[2], x1[1]-axoff, x1[2])
        yy <- usr2xy(c(xrange[1], tmp[i]))
        tkcreate(can, "line", yy[1]-axoff, yy[2], yy[1]-tl-axoff, yy[2],
                 fill=p$fg )
        tkcreate(can, "text", yy[1] - rpix * p$mgp[2] , yy[2], anchor="e",
                 text=as.character(tmp[i]), fill = p$col.axis, font=fnt.axis)
    }
    ## Points and labels
    laboff <- round(p2p * p$ps/2 + diam + 1)
    pola <- tclArray()        # points
    labtext <- tclArray()     # text
    id <- tclArray()          # index
    for (i in 1:nrow(sco)) {
        xy <- usr2xy(sco[i,])
        item <- tkcreate(can, "oval", xy[1]-diam, xy[2]-diam,
                         xy[1]+diam,  xy[2]+diam, 
                         width=1, outline=pcol, fill=pbg)
        lab <- tkcreate(can, "text", xy[1], xy[2]-laboff, text=labs[i],
                        fill = p$col, font=fnt)
        tkaddtag(can, "point", "withtag", item)
        tkaddtag(can, "label", "withtag", lab)
        pola[[lab]] <- item
        labtext[[lab]] <- labs[i]
        id[[lab]] <- i
    }
    ## Plotting and Moving
    pDown <- function(x, y) {
        x <- as.numeric(x)
        y <- as.numeric(y)
        tkdtag(can, "selected")
        tkaddtag(can, "selected", "withtag", "current")
        tkitemraise(can, "current")
        p <- as.numeric(tkcoords(can,
                                 pola[[tkfind(can, "withtag", "current")]]))
        .pX <<- (p[1]+p[3])/2
        .pY <<- (p[2]+p[4])/2
        .lastX <<- x
        .lastY <<- y
    }
    pMove <- function(x, y) {
        x <- as.numeric(x)
        y <- as.numeric(y)
        tkmove(can, "selected", x - .lastX, y - .lastY)
        tkdelete(can, "ptr")
        .lastX <<- x
        .lastY <<- y
        xadj <- as.numeric(tkcanvasx(can, 0))
        yadj <- as.numeric(tkcanvasy(can, 0))
        conn <- tkcreate(can, "line", .lastX + xadj, .lastY+yadj,
                         .pX, .pY, fill="red")
        tkaddtag(can, "ptr", "withtag", conn)
    }
    ## Dummy location of the mouse
    .lastX <- 0
    .lastY <- 0
    .pX <- 0
    .pY <- 0
    ## Highlight a label when mouse moves in
    tkitembind(can, "label", "<Any-Enter>",
               function() tkitemconfigure(can, "current", fill="red"))
    tkitembind(can, "label", "<Any-Leave>",
               function() tkitemconfigure(can, "current", fill=p$col))
    tkitembind(can, "label", "<1>", pDown)
    tkitembind(can, "label", "<ButtonRelease-1>",
               function() {tkdtag(can, "selected"); tkdelete(can, "ptr")})
    
    tkbind(can, "<B1-Motion>", pMove)
}