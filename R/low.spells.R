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
        flow.ts.comp <- flow.ts.comp[which(flow.ts.comp[[3]] %in% names(n.days)), ]
        record.year <- flow.ts.comp[[3]]
        n.years <- nlevels(as.factor(record.year))
        
        ann.min <- tapply(flow.ts.comp[[2]], flow.ts.comp[[3]], min)
        
        ann.min.day.no <- tapply(rev(flow.ts.comp[[2]]), flow.ts.comp[[3]], which.min) - 1
        ann.min.date <- as.Date(paste(names(ann.min.day.no), 1, 1, sep = "-")) + ann.min.day.no
        
        correct.ann.min.day <- day.dist(names(ann.min.day.no), ann.min.day.no)
        
        # av.min.day<-mean(ann.min.day.no,na.rm=T) cv.min.day<-(sd(ann.min.day.no,na.rm=T)/mean(ann.min.day.no,na.rm=T))*100
        
    }
    
    if (ann.stats.only == T) {
        
        
        
        return(list(avg.min.ann = mean(ann.min[is.finite(ann.min)], na.rm = T), timing.min.flow = correct.ann.min.day[1], min.timing.predictability = correct.ann.min.day[2]))
        
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
    
    return(list(low.spell.threshold = flow.threshold, avg.low.spell.duration = avg.duration, max.low.duration = max.duration, low.spell.freq = low.spell.frequency, avg.min.ann = mean(ann.min[is.finite(ann.min)], 
        na.rm = T), timing.min.flow = correct.ann.min.day[[1]], pred.min.flow = correct.ann.min.day[[2]]))
    
    
    
} 
