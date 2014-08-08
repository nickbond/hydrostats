four.digit.year <- function(x, change.year = 1968) {
		n <- year(x)%%100
    lubridate::year(x) <- ifelse(n > change.year%%100, 1900 + n, 2000 + n)
    x
} 
