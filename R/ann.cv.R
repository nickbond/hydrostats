ann.cv <- function(flow.ts) {
    Q <- NULL
    flow.ts$year <- strftime(flow.ts$Date, format = "%Y")
    ann.stats <- aggregate(. ~ year, flow.ts, function(x) c(mean = mean(x), sd = sd(x)))

    
    out <- sd(ann.stats$mean, na.rm = T)/mean(ann.stats$mean, na.rm = T) * 100
    
    return(data.frame(ann.cv = out))
} 
