chartKanbanBugFeatureTimes <- function(kanban, TimeMeasure, TargetYear) {
    require(googleVis)
    require(reshape2)
    
    #get the means of the leadtimes per month
    kanban.timemeasure.m <- aggregate(kanban[,TimeMeasure], 
                                   list(Month = strftime(kanban$Closed, "%m"),
                                        Year = strftime(kanban$Closed, "%Y"), 
                                        Type = kanban$Type), 
                                    mean, na.rm=TRUE)
    
    #move factor (type bug or feature) from a factor to a column for charting
    kanban.timemeasure.m <- dcast(kanban.timemeasure.m, Month + Year ~ Type, value.var="x")

    if(TargetYear != "ALL") {
        kanban.timemeasure.m <- subset(kanban.timemeasure.m, kanban.timemeasure.m$Year == TargetYear)
    }
    
    df <- data.frame(date=as.factor(paste(kanban.timemeasure.m$Month,"-",kanban.timemeasure.m$Year, sep="")), 
                     val1=kanban.timemeasure.m["Bug"],
                     val2=kanban.timemeasure.m["Feature"])
    
    
    Line <-  gvisLineChart(df, xvar="date", yvar=c("Bug","Feature"),
                           options=list(
                               title=paste("Features:", TimeMeasure, "by Month for Features and Bugs", sep=" "),
                               titleTextStyle="{color:'blue', 
                                           fontName:'Courier', 
                                           fontSize:16}",                         
                               backgroundColor="#D3D3D3",                          
                               vAxis="{title: 'Days', gridlines:{color:'black', count:9}}",
                               hAxis="{title:'Month', titleTextStyle:{color:'blue'}}",
                               series="[{color:'orange', targetAxisIndex: 0}, 
                                   {color: 'green',targetAxisIndex:0}]",
                               
                               legend="bottom",
                               
                               width=1500,
                               height=600                         
                           ))
    #plot(Line)
    return(Line)
    
}