#NOVEL_MEANING

#this is the script for doing statistics on sensibility rating data
#by Kristijan Armeni

#####-------------------------INITIALISATION

library(ggplot2)
library(reshape)
library(plyr)
library(psych)
library(ez)
curr_dir = getwd()

inp_dir = 'D:/Kristijan/raziskovalno/novel_meaning/output/beh/stats/descriptive'
out_dir = 'D:/Kristijan/raziskovalno/novel_meaning/output/beh/stats/tests'

# set proper dir for reading in the files
if (curr_dir != inp_dir){
  setwd(inp_dir)
}

#loading the dataset
load('sum_sensibility_persubjects.Rdata')
dat <- sum_dat_persub
dat <- split(dat, f = list(dat$mood, dat$condition))
dat2 <- split(sum_dat_persub, f = sum_dat_persub$mood)

#####-------------------------STATS

tS <- t.test(dat$happy.sens$mean, dat$happy.nsens$mean, paired = TRUE) 
tH <- t.test(dat$sad.sens$mean, dat$sad.nsens$mean, paired = TRUE) 

t3 <- t.test(dat2$happy$mean, dat2$sad$mean)
