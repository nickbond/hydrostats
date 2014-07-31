high.spells <- function(flow.ts, quant = 0.9, user.threshold = FALSE, defined.threshold = NULL, ind.days = 5, duration = TRUE, 
    volume = TRUE, plot = TRUE, ignore.zeros = FALSE, ctf.threshold = 0.1, ann.stats = TRUE, ann.stats.only = FALSE, inter.flood = FALSE, 
    hydro.year = FALSE, facs=NULL) {
    
    gauge <- deparse(substitute(flow.ts))
    Q <- NULL
    Year <- NULL
    
    if (hydro.year == TRUE) {
        print("Returning results based on hydrologic year")
        flow.ts <- hydro.year(flow.ts, year = "hydro")
        record.year <- flow.ts[, "year"]
    } else {
        record.year <- strftime(flow.ts[, "Date"], format = "%Y")
        flow.ts <- data.frame(flow.ts, year = record.year)
    }
    n.years <- nlevels(as.factor(record.year))
    
    if (ann.stats == T) {
        
        flow.ts.comp <- na.omit(flow.ts)
        
        n.days <- tapply(flow.ts.comp[, "Q"], flow.ts.comp[, "year"], length)
        n.most.days <- which(n.days >= 350)
        
        if (length(n.most.days) == 0) {
            ann.maxs.mean <- NA
            ann.maxs.sd <- NA
            avg.max.day <- data.frame()
            avg.max.day <- cbind(NA, NA)
            avg.ann.duration <- NA
            flood.skewness <- NA
            cv.ann.duration <- NA
            cv.max.ann <- NA
        } else {
            flow.ts.comp <- flow.ts.comp[which(flow.ts.comp[,'year'] %in% names(n.most.days)), ]
            record.year <- flow.ts.comp[,'year']
            n.years <- nlevels(as.factor(record.year))
            
            if(!is.null(facs)) {
            	facs2<-c(facs, "year")
            	ann.maxs <- ddply(flow.ts.comp, facs2, summarise, max = max(Q, na.rm = T))
            	mean.ann <- ddply(flow.ts.comp, facs, summarise, mean = mean(Q, na.rm =T))
            	ann.max.days <- ddply(flow.ts.comp, facs2, subset, Q == max(Q))
            	
            	
            	ann.maxs.mean <- ddply(ann.maxs, facs, summarise, mean.max=mean(max, na.rm = T))
            	ann.maxs.sd <- ddply(ann.maxs, facs, summarise, sd.max=sd(max, na.rm = T))
            	
            	avg.ann.max.days <- ddply(ann.max.days, facs2, function(x) day.dist(x$Date))
            	
            	avg.max.day <- ddply(avg.ann.max.days, facs, function(x) day.dist(days = x$mean.doy, years = x$year))
            	
            	
            } else {
            	
            	
            ann.maxs <- ddply(flow.ts.comp, .(year), summarise, max = max(Q, na.rm = T))
            mean.ann <- data.frame(mean = mean (flow.ts.comp$Q, na.rm =T))
            
            ann.max.days <- ddply(flow.ts.comp, .(year), subset, Q == max(Q))
            
            
            ann.maxs.mean <- data.frame(mean.max=mean(ann.maxs$max, na.rm = T))
            ann.maxs.sd <- data.frame(sd.max=sd(ann.maxs$max, na.rm = T))
            
            avg.ann.max.days <- ddply(ann.max.days, .(year), function(x) day.dist(x$Date))
            
            avg.max.day <- day.dist(days = avg.ann.max.days$mean.doy, years = avg.ann.max.days$year)
            
            }
            
            max.ann.flow.threshold <- min(ann.maxs$max, na.rm = T)
            
            ann.max.spells <- ifelse(flow.ts[, 'Q'] > max.ann.flow.threshold, 1, 0)
            ann.max.spell.runs <- rle(ann.max.spells)
            avg.ann.duration <- mean(ann.max.spell.runs$lengths[which(ann.max.spell.runs$values == 1)], na.rm = T)
            max.ann.duration <- max(ann.max.spell.runs$lengths[which(ann.max.spell.runs$values == 1)], na.rm = T)
            cv.max.ann <- (ann.maxs.sd[,'sd.max']/ann.maxs.mean[,'mean.max']) * 100
            flood.skewness <- ann.maxs.mean[,'mean.max']/mean.ann[,'mean']
            cv.ann.duration <- (sd(ann.max.spell.runs$lengths[which(ann.max.spell.runs$values == 1)], na.rm = T)/mean(ann.max.spell.runs$lengths[which(ann.max.spell.runs$values == 
                1)], na.rm = T)) * 100
            
        }
    }
    if (ann.stats.only == T) {
        
        return(data.frame(avg.max.ann = ann.maxs.mean, cv.max.ann = cv.max.ann, flood.timing = avg.max.day[, 'mean.doy'], flood.predictability = avg.max.day[, 'sd.doy'], 
            flood.skewness = flood.skewness, avg.ann.duration = avg.ann.duration, cv.ann.duration = cv.ann.duration))
        
    } else {
        
        if (user.threshold == T) {
            
            
            flow.threshold <- defined.threshold
            
        } else {
            
            
            if (ignore.zeros == T) {
                
                flow.threshold <- quantile(flow.ts[which(flow.ts[,'Q'] > ctf.threshold), 'Q'], quant, na.rm = T)
                names(flow.threshold) <- NULL
            } else {
                flow.threshold <- quantile(flow.ts[, 'Q'], quant, na.rm = T)
                names(flow.threshold) <- NULL
            }
            
        }
        
        
        
        
        
        high.flows <- ifelse(flow.ts[, 'Q'] > flow.threshold, 1, 0)
        
        if (ind.days > 0) {
            
            high.flow.runs <- rle(high.flows)
            too.short <- which(high.flow.runs$lengths < ind.days & high.flow.runs$values == 0)
            spell.factor <- rep(seq_along(high.flow.runs$lengths), times = high.flow.runs$lengths)
            add.to.spell <- which(spell.factor %in% too.short)
            high.flows[add.to.spell] <- 1
        }
        
        
        high.flow.runs <- rle(high.flows)
        
        
        
        high.flow.av <- mean(flow.ts[which(high.flows == 1), 'Q'], na.rm = T)
        high.flow.sd <- sd(flow.ts[which(high.flows == 1), 'Q'], na.rm = T)
        
        
        
        good.high.flow.runs <- which(!is.na(high.flow.runs$values))
        flow.runs.values <- high.flow.runs$values[good.high.flow.runs]
        flow.runs.lengths <- high.flow.runs$lengths[good.high.flow.runs]
        
        n.events <- length(flow.runs.values[flow.runs.values == 1])
        flood.frequency <- n.events/n.years
        
        
        
        
        if (duration == TRUE) {
            
            avg.duration <- mean(high.flow.runs$lengths[which(high.flow.runs$values == 1)], na.rm = T)
            med.duration <- quantile(high.flow.runs$lengths[which(high.flow.runs$values == 1)], 0.5, na.rm = T, names = F)
            max.duration <- max(high.flow.runs$lengths[which(high.flow.runs$values == 1)], na.rm = T)
            sd.duration <- sd(high.flow.runs$lengths[which(high.flow.runs$values == 1)], na.rm = T)
            cv.duration <- sd.duration/avg.duration * 100
        }
        
        if (inter.flood == TRUE) {
            
            avg.interval <- mean(high.flow.runs$lengths[which(high.flow.runs$values == 0)], na.rm = T)
            min.interval <- min(high.flow.runs$lengths[which(high.flow.runs$values == 0)], na.rm = T)
            max.interval <- max(high.flow.runs$lengths[which(high.flow.runs$values == 0)], na.rm = T)
            
            sd.interval <- sd(high.flow.runs$lengths[which(high.flow.runs$values == 0)], na.rm = T)
            cv.interval <- sd.interval/avg.interval * 100
            return(data.frame(high.spell.threshold = flow.threshold, average.interval = avg.interval, min.interval = min.interval, 
                max.interval = max.interval))
        }
        
        if (volume == TRUE) {
            spell.factor <- rep(seq_along(high.flow.runs$lengths), times = high.flow.runs$lengths)
            spells <- split(flow.ts[,'Q'], spell.factor)
            spell.volumes <- flow.ts[,'Q']
            spell.volumes <- sapply(spells, sum)
            spell.volumes.below.threshold <- sapply(spells, length) * flow.threshold
            spell.volumes <- spell.volumes[which(high.flow.runs$values == 1)] - spell.volumes.below.threshold[which(high.flow.runs$values == 
                1)]
            
        }
        
        
        if (plot == TRUE) {
            plot(flow.ts[,'Date'], flow.ts[,'Q'], type = "l", main = gauge, xlab = "Date", ylab = "Q")
            
            points(flow.ts[which(high.flows == 1), 'Date'], flow.ts[which(high.flows == 1), 'Q'], col = "red", cex = 0.25)
            
            abline(h = flow.threshold)
        }
        
    }
    
    if (ann.stats == F) {
        return(data.frame(n.years = n.years, high.spell.threshold = flow.threshold, n.events = n.events, spell.frequency = flood.frequency, 
            ari = 1/flood.frequency, avg.high.spell.duration = avg.duration, med.high.spell.duration = med.duration, max.high.spell.duration = max.duration, 
            avg.spell.volume = mean(spell.volumes, na.rm = T), avg.spell.peak = high.flow.av, sd.spell.peak = high.flow.sd))
    } else {
        return(data.frame(n.years = n.years, high.spell.threshold = flow.threshold, n.events = n.events, spell.freq = flood.frequency, 
            ari = 1/flood.frequency, avg.high.spell.duration = avg.duration, med.high.spell.duration = med.duration, max.high.spell.duration = max.duration, 
            avg.spell.volume = mean(spell.volumes, na.rm = T), avg.spell.peak = high.flow.av, sd.spell.peak = high.flow.sd, 
            avg.max.ann = ann.maxs.mean, cv.max.ann = cv.max.ann, flood.skewness = flood.skewness, flood.timing = avg.max.day[,'mean.doy'], 
            flood.predictability = avg.max.day[,'sd.doy'], avg.ann.duration = avg.ann.duration, cv.ann.duration = cv.ann.duration))
    }
    
    
} 
