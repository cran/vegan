"make.cepnames" <-
function(names) {
   names <- make.names(names, unique=FALSE) 
   names <- lapply(strsplit(names, "\\."), function(x) substring(x, 1, 4))
   names <- unlist(lapply(names, function(x) paste(x[c(1,length(x))], collapse="")))
   names <- make.names(names, unique=TRUE)
   names
}
