"vegandocs" <-
    function (doc = c("ChangeLog", "FAQ-vegan.pdf", "vegan-FAQ.pdf", "partitioning.pdf")) 
{
    doc <- match.arg(doc)
    if (length(grep(".pdf", doc)) > 0) {
        doc <- file.path(system.file(package="vegan"), "doc", doc)
        if (.Platform$OS.type == "windows")
            shell.exec(doc)
        else system(paste(getOption("pdfviewer"), doc, "&"))
    }
    else {
        file.show(system.file(package="vegan", doc))
    } 
}
