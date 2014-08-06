ts.format <- function(x, format = "%d/%m/%Y") {
    x[[1]] <- strptime(x[, 1], format = format)
    x[[1]] <- as.POSIXct(x[[1]])
    names(x)[1] <- "Date"
    return(x)
} 
