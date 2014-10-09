initial.dir <- getwd()
setwd("C:/Users/bargels/code/kanbanStats")
kanban <- read.csv("data/KanbanScrubbed.csv", header=TRUE)

 kanban <- kanban[order(as.Date(kanban$ReleaseDate, format="%m/%d/%Y")),]

# kanban$Opened <- as.Date(kanban$Opened, "%m/%d/%y")
# kanban$ReadyForDevDate <- as.Date(kanban$ReadyForDevDate, "%m/%d/%y")
# kanban$ReleaseDate <- as.Date(kanban$ReleaseDate, "%m/%d/%y")
# kanban$Closed <- as.Date(kanban$Closed, "%m/%d/%y")

# kanban$LeadTime <- apply(kanban[c('ReleaseDate', 'Opened')], 1, 
#                          function(x) x[1] - x[2])

kanban$LeadTime <- apply(kanban[c('ReleaseDate', 'Opened')], 1, 
                         function(x) as.Date(x[1],format='%m/%d/%Y') - 
                           as.Date(x[2],format='%m/%d/%Y'))


kanban$CycleTime <- apply(kanban[c('ReleaseDate', 'ReadyForDevDate')], 1, 
                          function(x) as.Date(x[1],format='%m/%d/%Y') - 
                            as.Date(x[2],format='%m/%d/%Y'))

kanban.types <- as.vector(unique(kanban$Type))
kanban.types <- append(kanban.types, 'ALL',0)
selected.type <- select.list(kanban.types, graphics = TRUE)

if(selected.type != "ALL") {
  kanban.sub <- subset(kanban, kanban$Type == selected.type)  
} else {
  kanban.sub <- kanban
}

mean.lead.time <- mean(kanban.sub$LeadTime)
sd.lead.time <- sd(kanban.sub$LeadTime)
ucl <- mean.lead.time + sd.lead.time
lcl <- mean.lead.time - sd.lead.time
if (lcl<0) {
  lcl = 0
}

# make ReleaseDate an ordered factor (so ggplot2 doesn't sort them)
kanban.sub$ReleaseDate <- factor(kanban.sub$ReleaseDate, levels = kanban.sub$ReleaseDate, ordered = TRUE)

if(selected.type != "ALL") {
  p <- ggplot(kanban.sub, aes(x = ReleaseDate, y=LeadTime)) + geom_point()  
} else {
  
    p <- ggplot(kanban.sub, aes(x = ReleaseDate, y=LeadTime)) + geom_point(aes(color=factor(kanban.sub$Type)))

p + geom_abline(intercept = lcl, slope = 0, color="red") + 
    geom_abline(intercept = mean.lead.time, slope = 0) +
    geom_abline(intercept = ucl, slope = 0, color="red") 
    #+ annotate("text", x = as.Date("4/23/2014", format("%m/%d/%Y")), y = 25, label = "Some text") 
    #+ scale_x_date()


#print(sd.lead.time)
#print(ucl)
#print(mean.lead.time)
#print(lcl)


setwd(initial.dir)

