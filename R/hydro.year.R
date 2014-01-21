hydro.year <- function(flow.ts, hydro.year = "hydro") {
    
    begin <- head(flow.ts[[1]], 1)
    finish <- tail(flow.ts[[1]], 1)
    
    if (hydro.year == "hydro") {
        month.runs <- c()
        month.means <- aggregate(flow.ts[[2]], by = list(month = strftime(flow.ts[[1]], format = "%m")), sum, na.rm = T)
        month.runs[1] <- sum(month.means[1:6, 2])
        month.runs[2] <- sum(month.means[2:7, 2])
        month.runs[3] <- sum(month.means[3:8, 2])
        month.runs[4] <- sum(month.means[4:9, 2])
        month.runs[5] <- sum(month.means[5:10, 2])
        month.runs[6] <- sum(month.means[6:11, 2])
        month.runs[7] <- sum(month.means[7:12, 2])
        month.runs[8] <- sum(month.means[c(8:12, 1), 2])
        month.runs[9] <- sum(month.means[c(9:12, 1:2), 2])
        month.runs[10] <- sum(month.means[c(10:12, 1:3), 2])
        month.runs[11] <- sum(month.means[c(11:12, 1:4), 2])
        month.runs[12] <- sum(month.means[c(12, 1:5), 2])
        
        alt.month <- which.min(month.runs)
        year <- c()
        year <- ifelse(as.numeric(strftime(flow.ts[[1]], format = "%m")) < alt.month, as.numeric(strftime(flow.ts[[1]], format = "%Y")) - 
            1, as.numeric(strftime(flow.ts[[1]], format = "%Y")))
    }
    flow.ts <- cbind(flow.ts, Year = year)
    return(flow.ts)
    
} 
