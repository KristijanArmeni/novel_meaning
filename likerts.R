#NOVEL_MEANING

#this is the script for analysing individual sentence meaning ratings
#by Kristijan Armeni

#####-------------------------INITIALISATION

library(ggplot2)
library(reshape)
library(plyr)
curr_dir = getwd()

inp_dir = 'D:/Kristijan/raziskovalno/novel_meaning/data/logs'
out_dir = 'D:/Kristijan/raziskovalno/novel_meaning/output/beh'

subjects = c('s01', 's02', 's03', 's04')
myfiles = list.files(pattern = '*log.txt')


# set proper dir for reading in the files
if (curr_dir != inp_dir){
  setwd(inp_dir)
}

#####-------------------------SUBJECT-LOOP

for (i in 1:length(myfiles)){
  
  subject = substr(myfiles[i], 1, 3)
  print(subject)
  
  sub_file <- read.table(myfiles[i], header = TRUE)
  
  if (i == 1)
  {sub_all <- sub_file}
  else
  {sub_all <- rbind(sub_all, sub_file)}
  

}

