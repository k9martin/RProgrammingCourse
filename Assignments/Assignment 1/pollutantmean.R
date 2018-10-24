pollutantmean<-function(directory,pollutant,id=1:332){
        
        filenames <- paste(directory,"/",formatC(id,width=3,flag="0"),".csv")
        
        list <- lapply(filenames,data.table::fread)
        dt <- rbindlist(list)
        
        if(c(pollutant) %in% names(dt)){
                return(dt[, lapply(.SD,mean,na.rm = TRUE), .SDcols = pollutant][[1]])
        }
        
}