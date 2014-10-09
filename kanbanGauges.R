kanbanGauges <- function(KPI) {
    
    name = c("Fixed Delivery") 
    value = c(90.9)
    df = data.frame(name, value)  
    
    Gauge <- gvisGauge(df, options=list(min=0, max=100, 
                                    greenFrom=KPI, greenTo=100, 
                                    yellowFrom=70, yellowTo=KPI,
                                    redFrom=0, redTo=70))
    
    #plot(Gauge)
   return(Gauge)
    
}