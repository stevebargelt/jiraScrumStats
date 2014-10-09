
readKanbanData <- function () {
    classes <- c(Opened="Date", Closed="Date", Ready.for.Dev.Date="Date", Due.Date="Date")
    kanban <- read.csv("data/KanbanScrubbed.csv", header=TRUE, colClasses=classes)
    kanban2013 <- read.csv("data/Kanban2013Scrubbed.csv", header=TRUE)
    
    kanban <- rbind(kanban, kanban2013)
    kanban2013 <- NULL
    
    #kanban.all <- subset(kanban, !is.na(kanban$Closed))
    
    kanban$LeadTime <- apply(kanban[c('Closed', 'Opened')], 1, 
                             function(x) as.Date(x[1],format='%Y-%m-%d') - 
                                 as.Date(x[2],format='%Y-%m-%d'))
    
    kanban$CycleTime <- apply(kanban[c('Closed', 'Ready.for.Dev.Date')], 1, 
                              function(x) as.Date(x[1],format='%Y-%m-%d') - 
                                  as.Date(x[2],format='%Y-%m-%d'))
    
    
    
    kanban$Quarter = quarters(as.Date(kanban$Closed))
    kanban$Month = as.numeric(strftime(kanban$Closed, "%m"))
    kanban$Day = as.numeric(strftime(kanban$Closed, "%d"))
    kanban$Year = as.numeric(strftime(kanban$Closed, "%Y"))
    
    return(kanban)
    
}

