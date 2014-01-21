low.spells <- function(flow.ts, quant = 0.1, duration = T, volume = T, plot = T, annual.stats = T, ann.stats.only = F) {
    gauge <- deparse(substitute(flow.ts))
    
    
    if (ncol(flow.ts) > 2) {
        record.year <- flow.ts[, "Year"]
    } else {
        record.year <- strftime(flow.ts[[1]], format = "%Y")
        flow.ts <- data.frame(flow.ts, Year = record.year)
    }
    
    n.years <- nlevels(as.factor(record.year))
    
    if (annual.stats == T) {
        # calculate annual minimum flow
        flow.ts.comp <- na.omit(flow.ts)
        
        # record.year<-strftime(flow.ts.comp[[1]],format='%Y')
        n.days <- tapply(flow.ts.comp[[2]], flow.ts.comp[[3]], length)
        n.most.days <- which(n.days > 350)
        flow.ts.comp <- flow.ts.comp[which(flow.ts.comp[, "Year"] %in% names(n.most.days)), ]
        record.year <- flow.ts.comp[[3]]
        n.years <- nlevels(as.factor(record.year))
        
        
        ann.mins <- ddply(flow.ts.comp, .(Year), summarise, min = min(Q, na.rm = T))
        
        ann.min.days <- ddply(flow.ts.comp, .(Year), subset, Q == min(Q))
        
        
        ann.mins.mean <- mean(ann.mins$min, na.rm = T)
        ann.mins.sd <- sd(ann.mins$min, na.rm = T)
        
        avg.ann.min.days <- ddply(ann.min.days, .(Year), function(x) day.dist(x$Date))
        
        avg.min.day <- day.dist(avg.ann.min.days$mean.doy, avg.ann.min.days$Year)
        
    }
    
    if (ann.stats.only == T) {
        
        
        
        return(list(avg.min.ann = ann.mins.mean, cv.min.ann = (ann.mins.sd/ann.mins.mean) * 100, timing.min.flow = avg.min.day[[1]], 
            pred.min.flow = avg.min.day[[2]]))
        
    } else {
        
        
        
        
        # average spell characteristics
        flow.threshold <- quantile(flow.ts[, 2], quant, na.rm = T)
        names(flow.threshold) <- NULL  #normallyhide
        low.flows <- ifelse(flow.ts[, 2] <= flow.threshold, 1, 0)
        low.flow.av <- mean(flow.ts[which(low.flows == 1), 2])
        low.flow.sd <- sd(flow.ts[which(low.flows == 1), 2])
        
        
        low.flow.runs <- rle(low.flows)
        
        
        low.spell.days <- as.numeric(strftime(flow.ts[which(low.flows == 1), 1], format = "%j"))
        
        good.low.flow.runs <- which(!is.na(low.flow.runs$values))
        flow.runs.values <- low.flow.runs$values[good.low.flow.runs]
        flow.runs.lengths <- low.flow.runs$lengths[good.low.flow.runs]
        
        low.spell.frequency <- length(flow.runs.values[flow.runs.values == 1])/n.years
        
        
        
        
        
        
        if (duration == TRUE) {
            
            avg.duration <- mean(low.flow.runs$lengths[which(low.flow.runs$values == 1)], na.rm = T)
            max.duration <- max(low.flow.runs$lengths[which(low.flow.runs$values == 1)], na.rm = T)
            sd.duration <- sd(low.flow.runs$lengths[which(low.flow.runs$values == 1)], na.rm = T)
            cv.duration <- sd.duration/avg.duration
        }
        
        if (volume == TRUE) {
            spell.factor <- rep(seq_along(low.flow.runs$lengths), times = low.flow.runs$lengths)
            spells <- split(flow.ts[[2]], spell.factor)
            spell.volumes <- flow.ts[[2]]
            spell.volumes <- sapply(spells, sum)
            spell.volumes <- spell.volumes[which(low.flow.runs$values == 1)]
            
        }
        
        
        if (plot == TRUE) {
            plot(flow.ts[[1]], flow.ts[[2]], type = "l", main = gauge, xlab = "Date", ylab = "Q")
            
            points(flow.ts[which(low.flows == 1), 1], flow.ts[which(low.flows == 1), 2], col = "red", cex = 0.25)
            
            abline(h = flow.threshold)
        }
        
        
    }
    
    return(list(low.spell.threshold = flow.threshold, avg.low.spell.duration = avg.duration, max.low.duration = max.duration, 
        low.spell.freq = low.spell.frequency, avg.min.ann = ann.mins.mean, cv.min.ann = (ann.mins.sd/ann.mins.mean) * 100, timing.min.flow = avg.min.day[[1]], 
        pred.min.flow = avg.min.day[[2]]))
    
    
    
} 
