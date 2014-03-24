four.digit.year <- function(x, year=1968){
	n <- year(x) %% 100
	year(x) <- ifelse(n > year %% 100, 1900+n, 2000+n)
	x
}