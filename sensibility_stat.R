#NOVEL_MEANING

#this is the script for doing statistics on log and sensibility rating data
#by Kristijan Armeni

#####-------------------------INITIALISATION

library(ggplot2)
library(reshape)
library(plyr)
library(psych)
library(ez)
curr_dir = getwd()

inp_dir = 'D:/Kristijan/raziskovalno/novel_meaning/data/beh/stats/descriptive'
out_dir = 'D:/Kristijan/raziskovalno/novel_meaning/output/beh/stats/tests'

# set proper dir for reading in the files
if (curr_dir != inp_dir){
  setwd(inp_dir)
}

#loading the dataset
load('sum_sensibility_persubjects.Rdata')
data <- sel_data