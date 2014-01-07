partial.series <- function(flow.ts, ari = 2, ind.days = 7, duration = T, plot = F, volume = T) {
    gauge <- deparse(substitute(flow.ts))
    
    n.years <- nlevels(as.factor(strftime(flow.ts$Date, format = "%Y")))
    
    if (ari > n.years) {
        return(print("warning(time-series is shorter than ari. Please provide a smaller ari"))
    }
    
    n.events <- ceiling(n.years/ari)
    p.series <- vector("list", n.events)
    
    rising <- flow.ts[2:nrow(flow.ts), 2] - flow.ts[1:nrow(flow.ts) - 1, 2]
    falling <- flow.ts[3:nrow(flow.ts), 2] - flow.ts[2:nrow(flow.ts) - 2, 2]
    
    peak.search <- data.frame(flow.ts, rising = c(NA, rising), falling = c(falling, NA, NA))
    peaks <- flow.ts[which(peak.search[, "rising"] > 0 & peak.search[, "falling"] < 0), ]
    
    peaks.ord <- peaks[order(peaks[, 2], decreasing = T), ]
    
    p.series[[1]] <- data.frame(peaks.ord[1, ])
    
    i = 1
    
    while (i < n.events) {
        i <- i + 1
        dif.time.test <- difftime(peaks.ord$Date[1], peaks.ord$Date)
        
        peaks.ord <- peaks.ord[which(abs(dif.time.test) > (ind.days * 24 * 60 * 60)), ]
        
        p.series[[i]] <- data.frame(peaks.ord[1, ])
        
        if (is.na(peaks.ord[1, 2]) == T) 
            NA
    }
    p.series <- do.call("rbind", p.series)
    
    row.names(p.series) <- seq(1:nrow(p.series))
    
    flow.threshold <- tail(p.series[, 2], 1)
    
    if (plot == TRUE) {
        plot(flow.ts[[1]], flow.ts[[2]], type = "l", main = gauge, xlab = "Date", ylab = "Q")
        
        points(p.series$Date, p.series$Q, col = "red", cex = 0.25)
        abline(h = (tail(p.series[2], 1) - 1))
    }
    
    high.flows <- ifelse(flow.ts[, 2] >= flow.threshold, 1, 0)
    high.flow.runs <- rle(high.flows)
    
    if (duration == TRUE) {
        avg.duration <- mean(high.flow.runs$lengths[which(high.flow.runs$values == 1)], na.rm = T)
        max.duration <- max(high.flow.runs$lengths[which(high.flow.runs$values == 1)], na.rm = T)
    }
    if (volume == TRUE) {
        spell.factor <- rep(seq_along(high.flow.runs$lengths), times = high.flow.runs$lengths)
        spells <- split(flow.ts[[2]], spell.factor)
        spell.volumes <- flow.ts[[2]]
        spell.volumes <- sapply(spells, sum)
        spell.volumes.below.threshold <- sapply(spells, length) * flow.threshold
        spell.volumes <- spell.volumes[which(high.flow.runs$values == 1)] - spell.volumes.below.threshold[which(high.flow.runs$values == 1)]
        
        return(list(p.series = p.series, n.events = n.events, flow.threshold = flow.threshold, avg.duration = avg.duration, max.duration = max.duration, med.spell.volume = median(spell.volumes)))
    } else {
        return(list(p.series = p.series, n.events = n.events, flow.threshold = flow.threshold, avg.duration = avg.duration, max.duration = max.duration))
    }
} 
