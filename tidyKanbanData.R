tidykanbandata <- function() {
    
    kanban <- read.csv("data/kanbanScrubbed.csv", header=TRUE)
    
    #get rid of unnecessary columns
    #we don't' use Pri and Sev in any calcs
    kanban$Priority <- NULL
    kanban$Severity <- NULL
    kanban$Closed <- NULL
    names(kanban)[names(kanban)=="Release.Date"] <- "Closed"
 
    #Clean up all the dates
    kanban$Closed <- as.Date(kanban$Closed, format = "%Y-%m-%d")
    kanban$Opened <- as.Date(kanban$Opened, format = "%d-%b-%Y")

    kanban$Due.Date[kanban$Due.Date==""]  <- NA
    kanban$Ready.for.Dev.Date <- 
        as.Date(kanban$Ready.for.Dev.Date, format = "%m/%d/%Y %H:%M:%S %p")

    #Order Columns
    kanban <- kanban[c("ID", "Ranking", "Type", "Status", "Reason.for.Closure", 
             "Opened", "Ready.for.Dev.Date", "Closed", "Due.Date")]
    
    write.csv(kanban, "data/kanbanScrubbed.csv")
}



tidykanban2013data <- function() {
    
    kanban2013 <- read.csv("data/kanban2013Scrubbed.csv", header=TRUE)

    #get rid of unnecessary columns
    #we don't' use Pri and Sev in any calcs
    kanban2013$Priority <- NULL
    kanban2013$Severity <- NULL
    
    #Clean up all the dates

    #Fixup Closed Dates
    kanban2013$Closed <- as.Date(kanban2013$Closed, format = "%m/%d/%Y")
    kanban2013$Opened <- as.Date(kanban2013$Opened, format = "%m/%d/%Y")
    kanban2013$Target.Deployment <- as.Date(kanban2013$Target.Deployment, format = "%m/%d/%Y")

    
    kanban2013 <- within(kanban2013, { 
        Closed = as.character(Closed)
        Target.Deployment <- as.character(Target.Deployment)
        Closed = ifelse(is.na(Closed), Target.Deployment, Closed) 
    } )
    
    #kanban2013$Closed[is.na(kanban2013$Closed)] <- kanban2013$Target.Deployment
    kanban2013$Target.Deployment <- NULL
    
    kanban2013$Ready.for.Dev.Date <- NA #we don't have this data from 2013
    kanban2013$Due.Date <- NA #we don't have this data from 2013
    
    #Order Columns
    kanban2013 <- kanban2013[c("ID", "Ranking", "Type", "Status", "Reason.for.Closure", 
                       "Opened", "Ready.for.Dev.Date", "Closed", "Due.Date")]
    
    write.csv(kanban2013, "data/kanban2013Scrubbed.csv")
    
}

