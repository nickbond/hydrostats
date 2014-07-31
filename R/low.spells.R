low.spells <- function(flow.ts, quant = 0.1, user.threshold = FALSE, defined.threshold = NULL, duration = T, volume = T, plot = T, 
											 annual.stats = T, ann.stats.only = F, hydro.year = FALSE, facs=NULL) {
    gauge <- deparse(substitute(flow.ts))
    Q <- NULL
    Year <- NULL
    
    if (hydro.year == TRUE) {
        print("Returning results based on hydrologic year")
        flow.ts <- hydro.year(flow.ts, year = "hydro")
        record.year <- flow.ts[, "year"]
    } else {
        record.year <- strftime(flow.ts[,'Date'], format = "%Y")
        flow.ts <- data.frame(flow.ts, year = record.year)
    }
    
    n.years <- nlevels(as.factor(record.year))
    
    if (annual.stats == T) {
        # calculate annual minimum flow
        flow.ts.comp <- na.omit(flow.ts)
        
        # record.year<-strftime(flow.ts.comp[[1]],format='%Y')
        n.days <- tapply(flow.ts.comp[, 'Q'], flow.ts.comp[, 'year'], length)
        n.most.days <- which(n.days >= 350)
        if (length(n.most.days) == 0) {
            ann.mins.mean <- NA
            cv.min.ann <- NA
            avg.min.day <- cbind(NA, NA)
            
            
        } else {
            
            
            flow.ts.comp <- flow.ts.comp[which(flow.ts.comp[, 'year'] %in% names(n.most.days)), ]
            record.year <- flow.ts.comp[, 'year']
            n.years <- nlevels(as.factor(record.year))
            
            if(!is.null(facs)) {
            	facs2<-c(facs, "year")
            	ann.mins <- ddply(flow.ts.comp, facs2, summarise, min = min(Q, na.rm = T))
            	
            	ann.min.days <- ddply(flow.ts.comp, facs2, subset, Q == min(Q))
            	
            	
            	ann.mins.mean <- ddply(ann.mins, facs, summarise, mean.min=mean(min, na.rm = T))
            	ann.mins.sd <- ddply(ann.mins, facs, summarise, sd.min=sd(min, na.rm = T))
            	
            	avg.ann.min.days <- ddply(ann.min.days, facs2, function(x) day.dist(x$Date))
            	
            	avg.min.day <- ddply(avg.ann.min.days, facs, function(x) day.dist(days = x$mean.doy, years = x$year))
            	
            	
            } else {
            
            ann.mins <- ddply(flow.ts.comp, .(year), summarise, min = min(Q, na.rm = T))
            
            ann.min.days <- ddply(flow.ts.comp, .(year), subset, Q == min(Q))
            
            
            ann.mins.mean <- data.frame(mean.min=mean(ann.mins$min, na.rm = T))
            ann.mins.sd <- data.frame(sd.min=sd(ann.mins$min, na.rm = T))
            
            
            avg.ann.min.days <- ddply(ann.min.days, .(year), function(x) day.dist(x$Date))
            
            avg.min.day <- day.dist(days = avg.ann.min.days$mean.doy, years = avg.ann.min.days$year)
            
            }
            
            cv.min.ann <- (ann.mins.sd/ann.mins.mean) * 100
        }
    }
    
    if (ann.stats.only == T) {
        
        
        
        return(data.frame(avg.min.ann = ann.mins.mean, cv.min.ann = cv.min.ann, timing.min.flow = avg.min.day[,'mean.doy'], pred.min.flow = avg.min.day[, 'sd.doy']))
        
    } else {
        
    	if (user.threshold == T) {
    		
    		
    		flow.threshold <- defined.threshold
    		
    	} else {
        
        
        # average spell characteristics
        flow.threshold <- quantile(flow.ts[, 'Q'], quant, na.rm = T)
        names(flow.threshold) <- NULL  #normallyhide
        
    	}
        low.flows <- ifelse(flow.ts[, 'Q'] <= flow.threshold, 1, 0)
        low.flow.av <- mean(flow.ts[which(low.flows == 1), 'Q'])
        low.flow.sd <- sd(flow.ts[which(low.flows == 1), 'Q'])
        
        
        low.flow.runs <- rle(low.flows)
        
        
        low.spell.days <- as.numeric(strftime(flow.ts[which(low.flows == 1), 'Date'], format = "%j"))
        
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
            spells <- split(flow.ts[, 'Q'], spell.factor)
            spell.volumes <- flow.ts[, 'Q']
            spell.volumes <- sapply(spells, sum)
            spell.volumes <- spell.volumes[which(low.flow.runs$values == 1)]
            
        }
        
        
        if (plot == TRUE) {
            plot(flow.ts[[1]], flow.ts[, 'Q'], type = "l", main = gauge, xlab = "Date", ylab = "Q")
            
            points(flow.ts[which(low.flows == 1), 'Date'], flow.ts[which(low.flows == 1), 'Q'], col = "red", cex = 0.25)
            
            abline(h = flow.threshold)
        }
        
        
    }
    
    return(data.frame(low.spell.threshold = flow.threshold, avg.low.spell.duration = avg.duration, max.low.duration = max.duration, 
        low.spell.freq = low.spell.frequency, avg.min.ann = ann.mins.mean, cv.min.ann = cv.min.ann, timing.min.flow = avg.min.day[, 'mean.doy'], 
        pred.min.flow = avg.min.day[, 'sd.doy']))
    
    
    
} 
