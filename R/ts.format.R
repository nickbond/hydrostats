ts.format <- function(x) {
    x[[1]] <- strptime(x[[1]], format = "%d/%m/%Y")
    x[[1]] <- as.POSIXct(x[[1]])
    names(x)[[1]] <- "Date"
    return(x)
} 
