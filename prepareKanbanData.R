source("scrubkanbanData.R")
source("tidykanbanData.R")
originalwd <- getwd()
setwd("c:/users/bargels/code/kanbanStats")

scrubkanbandata()
tidykanbandata()

scrubkanban2013data()
tidykanban2013data()

setwd(originalwd)