scrubkanbandata <- function() {
    #this is a very specific script point to a specific location
    # I don't ever want to accidentally check in non-scrubbed data so I don't move
    # the raw file into the working directory.
        
    kanban <- read.csv("C:/Users/bargels/Downloads/report.csv", header=TRUE)
    
    removeCols <- c(2,3,4,9,10,12,14:23,25,26,30,31,32,34:44)
    
    kanban <- kanban[,-removeCols]
    
    kanban <- subset(kanban, kanban$Status == "Closed")
    #I could leave in Duplicate and Won't Fix items because they are "Real World"
    # but we must remove items like "Moved to Wide Beta" etc. because they could
    # point to the origin of the data
    kanban <- subset(kanban, kanban$Reason.for.Closure == "Fixed")
    
    names(kanban)[1] <- "ID"
    names(kanban)[6] <- "Opened"
    names(kanban)[7] <- "Closed"
    
    kanban$Type <- as.character(kanban$Type)
    kanban$Type[kanban$Type == "Incident"] <- "Bug"
    kanban$Type[kanban$Type == "Change Request"] <- "Feature"
    kanban$Type[kanban$Type == "New Feature"] <- "Feature"
    
    kanban$Reason.for.Closure <- as.character(kanban$Reason.for.Closure)
    kanban$Reason.for.Closure[kanban$Type == "Feature"] <- "Done"
    
    kanban$Type <- as.factor(kanban$Type)
    kanban$Reason.for.Closure <- as.factor(kanban$Reason.for.Closure)
    write.csv(kanban, "data/kanbanScrubbed.csv")
    
}

scrubkanban2013data <- function() {
    #this is a very specific script point to a specific location
    # I don't ever want to accidentally check in non-scrubbed data so I don't move
    # the raw file into the working directory.
    
    kanban2013 <- 
        read.csv("C:/Users/bargels/Documents/MTGO Live Maintenance Kanban Metrics/2013report.csv", header=TRUE)
    
    keepCols <- c(1,5:8,11,13,22,26)
    
    kanban2013 <- kanban2013[,keepCols]

    #Standardize Column Names
    names(kanban2013)[1] <- "ID"
    names(kanban2013)[6] <- "Opened"
    names(kanban2013)[7] <- "Closed"
    
    #Fixup status
    kanban2013$Status <- as.character(kanban2013$Status)
    kanban2013 <- subset(kanban2013, kanban2013$Status == "Closed" |  kanban2013$Status == "Verified")
    kanban2013$Status[kanban2013$Status == "Verified"] <- "Closed"
    kanban2013$Status <- as.factor(kanban2013$Status)
    
    #Fixup Types
    kanban2013$Type <- as.character(kanban2013$Type)
    kanban2013$Type[kanban2013$Type == "Incident"] <- "Bug"
    kanban2013$Type[kanban2013$Type == "Change Request"] <- "Feature"
    kanban2013$Type <- as.factor(kanban2013$Type)

    kanban2013 <- subset(kanban2013, kanban2013$Type == "Bug" |  kanban2013$Type == "Feature")
    
    
    #Add Reason for Closure
    kanban2013$Reason.for.Closure <- "Fixed"
    kanban2013$Reason.for.Closure[kanban2013$Type == "Feature"] <- "Done"
    kanban2013$Reason.for.Closure <- as.factor(kanban2013$Reason.for.Closure)
    
    #output new file
    write.csv(kanban2013, "data/kanban2013Scrubbed.csv")
    
}