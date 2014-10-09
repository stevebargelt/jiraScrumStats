setwd("C:/Users/bargels/code/kanbanStats")

source("basicKanbanDataSetup.R")
source("chartKanbanBugFeatureTimes.R")
source("tableKanbanCountsTimes.R")
source("chartFeatureLeadTimeVSBugCount.R")
source("kanbanGauges.R")

# ****************************************
# SET SLA Variables and other 
# non calculated KPI Metrics
# ****************************************
    # this is the % the team committed to being on-time with 
    # fixed delivery data class of service
    fixedDeliveryDatePercentSLA <- .90
    
    # this is the cycle time the team committed to 
    # for the standard class of service (days)
    standardCycleTimeSLA <- 45
    # this is the % the team committed to being
    # below standardCycleTimeSLA
    standardSLA <- .80
# ****************************************
# END SET SLA Variables 
# ****************************************

kanbanData <- readKanbanData()

## Split Raw Data into Service Classes
#kanbanExpidited <- subset(kanbanData, kanbanData$Expidited == TRUE)
kanbanStandard <- subset(kanbanData, is.na(kanbanData$Due.Date))
kanbanFixedDeliveryDate <- subset(kanbanData, !is.na(kanbanData$Due.Date))

# ****************************************
# FIXED DELIVERY DATE CLASS OF SERVICE
# ****************************************
kanbanFixedDeliveryDate$DateMet <- kanbanFixedDeliveryDate$Due.Date >= kanbanFixedDeliveryDate$Closed
t <- table(kanbanFixedDeliveryDate$DateMet)
kanbanFixedDeliveryDatePercentSuccess <- (1 - (t["FALSE"] / t["TRUE"]))
fixedDeliveryDateGauge <- kanbanGauges(fixedDeliveryDatePercentSLA)
## TODO: Caclulate average Lead and Cycle Times for these??

# ****************************************
# STANDARD CLASS OF SERVICE
# ****************************************

## TODO: Calculate suggested SLA dates (based on 1 or 3 sigma)

bugFeatureLeadTimeLine <- chartKanbanBugFeatureTimes(kanbanStandard, "LeadTime", "ALL")
bugFeatureLeadTimeLine2014 <- chartKanbanBugFeatureTimes(kanbanStandard, "LeadTime", "2014")
bugFeatureCycleTimeLine2014 <- chartKanbanBugFeatureTimes(kanbanStandard, "CycleTime", "2014")

table2013Bugs <- tableKanbanCountsTimes(kanbanData, "2013", "Bug")
table2014Bugs <- tableKanbanCountsTimes(kanbanData, "2014", "Bug")

featureTimeVSBugCount <- chartFeatureLeadTimeVSBugCount(kanbanData)


# ****************************************
# PLOT EVERYTHING
# ****************************************

#plot(table2013Bugs)
#plot(table2014Bugs)
#plot(bugFeatureLeadTimeLine)
#plot(bugFeatureLeadTimeLine2014)
#plot(featureTimeVSBugCount)

fixedDeliveryDateGauge2 <- fixedDeliveryDateGauge
tables <- gvisMerge(table2013Bugs, table2014Bugs, horizontal=TRUE)
gauges <- gvisMerge(fixedDeliveryDateGauge, tables)
Lines <- gvisMerge(bugFeatureLeadTimeLine, bugFeatureLeadTimeLine2014)
LBCA <- gvisMerge(gvisMerge(table2013Bugs, bugFeatureLeadTimeLine), gvisMerge(table2014Bugs, bugFeatureLeadTimeLine2014),
                  horizontal=TRUE, tableOptions="bgcolor=\"#AABBCC\"") 

plot(test)
