

kanban.features.m <- 
    aggregate(kanban.features[,11:12], 
              list(Month = strftime(kanban.features$Closed, "%m"), 
                   Year = strftime(kanban.features$Closed, "%Y")), 
              mean, na.rm=TRUE)

kanban.features.q <- 
    aggregate(kanban.features[,11:12], 
              list(Quarter = quarters(as.Date(kanban.features$Closed)), 
                   Year = strftime(kanban.features$Closed, "%Y")),
              mean, na.rm=TRUE)

kanban.bugs.m <- 
    aggregate(kanban.bugs[,11:12], 
              list(Month = strftime(kanban.bugs$Closed, "%m"), 
                   Year = strftime(kanban.bugs$Closed, "%Y")), 
              mean, na.rm=TRUE)

kanban.bugs.q <- 
    aggregate(kanban.bugs[,11:12], 
              list(Quarter = quarters(as.Date(kanban.bugs$Closed)), 
                   Year = strftime(kanban.bugs$Closed, "%Y")), 
              mean, na.rm=TRUE)


    kanban.m <- aggregate(kanban[,11:12], list(Month = strftime(kanban$Closed, "%m"), 
                                               Year = strftime(kanban$Closed, "%Y"), Type = kanban$Type), mean, na.rm=TRUE)
    
    kanban.leadtime.m <- aggregate(kanban[,11], list(Month = strftime(kanban$Closed, "%m"), 
                                                     Year = strftime(kanban$Closed, "%Y"), Type = kanban$Type), mean, na.rm=TRUE)
    
    kanban.cycletime.m <- aggregate(kanban[,12], list(Month = strftime(kanban$Closed, "%m"), 
                                                      Year = strftime(kanban$Closed, "%Y"), Type = kanban$Type), mean, na.rm=TRUE)
    
    
    kanban.leadtime.m <- dcast(kanban.leadtime.m, Month + Year ~ Type, value.var="x")
    
    kanban.m$monthyear <- as.factor(paste(kanban.m$Month,"-",kanban.m$Year, sep=""))
    
    
    kanban.q <- aggregate(kanban[,11:12], list(Quarter = quarters(as.Date(kanban$Closed)), 
                                               Year = strftime(kanban$Closed, "%Y"), Type = kanban$Type), mean, na.rm=TRUE)
    
    kanban.q$quarteryear <- as.factor(paste(kanban.q$Quarter,"-",kanban.q$Year, sep=""))
    
    # kanban.features.m <- 
    #     aggregate(kanban.features[,c(9,11:12)], 
    #               list(Month = strftime(kanban.features$Closed, "%m"), 
    #                    Year = strftime(kanban.features$Closed, "%Y")), 
    #               mean, na.rm=TRUE)
    
    # kanban.features.m$monthyear <- as.factor(paste(strftime(kanban.features.m$Closed, "%m"),"-",strftime(kanban.features.m$Closed, "%Y"), sep=""))
    
    library(googleVis)
    
    df <- data.frame(date=kanban.m["monthyear"], val1=kanban.features.m["LeadTime"], val2=kanban.features.m["CycleTime"])
    df <- data.frame(date=as.factor(paste(kanban.leadtime.m$Month,"-",kanban.leadtime.m$Year, sep="")), 
                     val1=kanban.leadtime.m["Bug"],
                     val2=kanban.leadtime.m["Feature"])
    
    Line <-  gvisLineChart(df, xvar="date", yvar=c("Bug","Feature"),
                           options=list(
                               title="Features: Lead Time by Month for Features and Bugs",
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
    plot(Line)
    
    kanban.features.q <- 
        aggregate(kanban.features[,11:12], 
                  list(Quarter = quarters(as.Date(kanban.features$Closed)), 
                       Year = strftime(kanban.features$Closed, "%Y")),
                  mean, na.rm=TRUE)
    
    
    
    kanban.features.q$quarteryear <- as.factor(paste(quarters(as.Date(kanban.features$Closed)),"-",strftime(kanban.features.m$Closed, "%Y"), sep=""))
    
    library(googleVis)
    
    df <- data.frame(date=kanban.features.m["monthyear"], val1=kanban.features.m["LeadTime"], val2=kanban.features.m["CycleTime"])
    Line <-  gvisLineChart(df, xvar="monthyear", yvar=c("LeadTime","CycleTime"),
                           options=list(
                               title="Features: Lead Time and Cycle Time by Month",
                               titleTextStyle="{color:'blue', 
                                           fontName:'Courier', 
                                           fontSize:16}",                         
                               backgroundColor="#D3D3D3",                          
                               vAxis="{title: 'Days', gridlines:{color:'black', count:9}}",
                               hAxis="{title:'Month', titleTextStyle:{color:'blue'}}",
                               series="[{color:'green', targetAxisIndex: 0}, 
                                   {color: 'orange',targetAxisIndex:0}]",
                               
                               legend="bottom",
                               
                               width=1500,
                               height=600                         
                           ))
    plot(Line)
    
    
    kanban.bugs.m <- 
        aggregate(kanban.bugs[,11:12], 
                  list(Month = strftime(kanban.bugs$Closed, "%m"), 
                       Year = strftime(kanban.bugs$Closed, "%Y")), 
                  mean, na.rm=TRUE)
    
    
    kanban.bugs.q <- 
        aggregate(kanban.bugs[,11:12], 
                  list(Quarter = quarters(as.Date(kanban.bugs$Closed)), 
                       Year = strftime(kanban.bugs$Closed, "%Y")), 
                  mean, na.rm=TRUE)
    