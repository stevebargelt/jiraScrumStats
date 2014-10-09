chartFeatureLeadTimeVSBugCount <- function(kanban, TargetYear) {
    
    require(plyr)

#     if(!is.null(TargetYear)) {
#         kanban.leadtime.m <- subset(kanban.leadtime.m, kanban.leadtime.m$Year == TargetYear)
#     }
    
    
    #get the means of the leadtimes per month
    kanban.leadtime.m <- aggregate(kanban[,11], 
                                   list(Month = strftime(kanban$Closed, "%m"),
                                        Year = strftime(kanban$Closed, "%Y"), 
                                        Type = kanban$Type), 
                                   mean, na.rm=TRUE)
    
    #move factor (type bug or feature) from a factor to a column for charting
    kanban.leadtime.m <- dcast(kanban.leadtime.m, Month + Year ~ Type, value.var="x")
    
    kanban.bug <- subset(kanban, kanban$Type == "Bug")
    
    countbymonth <- count(kanban.bug, c('Year','Month'))
    countbymonth$Month <- as.character(formatC(countbymonth$Month, width=2, flag="0"))
    countbymonth$Year <- as.character(formatC(countbymonth$Year, width=2, flag="0"))

    answer <- 
        merge(kanban.leadtime.m, countbymonth, 
              by = c("Month", "Year"))
        
    #sort the result
    answer <- answer[order(answer$Year, answer$Month),]
    answer$freq <- as.numeric(answer$freq)

    df <- data.frame(date=as.factor(paste(answer$Month,"-",answer$Year, sep="")), 
                     val1=answer["Feature"],
                     val2=answer["freq"])
    
    
    cmb <-  
    gvisComboChart(df, xvar="date", yvar=c("Feature","freq"),
                  options=list(title="Feature Lead Time vs. Bug Count",
                                titleTextStyle="{color:'blue', 
                                                fontName:'Courier', 
                                                fontSize:16}",
                               pointSize=9,
                               backgroundColor="#D3D3D3", 
                               seriesType="bars",
                               series="[{type:'line', 
                                       targetAxisIndex:0,
                                       color:'green'}, 
                                      {type:'bars', 
                                       targetAxisIndex:1,
                                       color:'grey'}]",
                               vAxes="[{title:'Days',
                                      titleTextStyle: {color: 'black'},
                                      textStyle:{color: 'black'},
                                      textPosition: 'out'}, 
                                     {title:'Count',
                                      titleTextStyle: {color: 'grey'},
                                      textStyle:{color: 'grey'},
                                      textPosition: 'out',
                                      minValue:0}]",
                               hAxes="[{title:'Date',
                                      textPosition: 'out'}]",
                               legend="bottom",
                               
                               width=1500,
                               height=600                        ))

    #plot(cmb)
    return(cmb)
    
}