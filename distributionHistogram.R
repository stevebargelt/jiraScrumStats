distribution <- function(kanbanData, TargetType, TimeMeasure, TargetYear) {

    
    kanbanFeatures <- subset(kanbanData, kanbanData$Type=="Feature")
    kanbanFeatures <- kanbanFeatures[order(kanbanFeatures$CycleTime),]
    
    kanbanBugs <- subset(kanbanData, kanbanData$Type=="Bug")
    kanbanBugs <- kanbanBugs[order(kanbanBugs$CycleTime),]
    dat2 <- data.frame(fDays=kanbanFeatures$CycleTime, bDays=kanbanBugs$CycleTime)

    
plothist <- gvisHistogram(dat2, options=list(
        legend="{ position: 'top', maxLines: 2 }",
        histogram.bucketSize = 1,
        width=700),
        chartid="Histogram")

plot(plothist)


hist(dat2$bDays, breaks=40, col="red")

return(plothist)

}



set.seed(123)
dat=data.frame(A=rpois(100, 20),
               B=rpois(100, 5),
               C=rpois(100, 50))
plot( 
    gvisHistogram(dat, options=list(
        legend="{ position: 'top', maxLines: 2 }",
        colors="['#5C3292', '#1A8763', '#871B47']",
        width=600),
        chartid="Histogram")
)