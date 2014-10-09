tableKanbanCountsTimes <- function(kanban, TargetYear, TargetType) {
    
    require(plyr)
    
    if(!is.null(TargetType)) {
        kanban.bytype <- subset(kanban, kanban$Type == TargetType)
    }
    else {
        
        kanban.bytype <- kanban
    }
    
    countbyquarter <- count(kanban.bytype, c('Year','Quarter'))
    
    timesbyquarter <- 
        aggregate(kanban.bytype[,11:12], 
                  list(kanban.bytype$Quarter, kanban.bytype$Year), 
                  mean, na.rm=TRUE)
    colnames(timesbyquarter) <- c("Quarter", "Year", "LeadTime", "CycleTime")
    
    typebyquarter <- 
        merge(countbyquarter, timesbyquarter, 
              by = c("Quarter", "Year"))
    colnames(typebyquarter) <- c("Quarter", "Year", "Count", "LeadTime", "CycleTime")
    
    countbyquarter <- NULL
    timesbyquarter <- NULL
    
    
    if(TargetYear == "ALL")
    {
        kanbanTable <- gvisTable(typebyquarter,options=list(height=200))
    } else {
        kanbanTable <- gvisTable(subset(typebyquarter, typebyquarter$Year == TargetYear),options=list(height=150))    
    }
    
    #plot(testTable)
    return(kanbanTable)
    
    #     testTable <- gvisTable(subset(featuresbyquarter, featuresbyquarter$Year == "2013"),options=list(width=800))
    #     plot(testTable)
    
}