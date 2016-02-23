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

tS <- t.test(dat$happy.sens$mean, dat$happy.nsens$mean, paired = TRUE) #sensibility effect in happy group
tH <- t.test(dat$sad.sens$mean, dat$sad.nsens$mean, paired = TRUE) #sensibility effect in sad group
tM <- t.test(dat2$happy$mean, dat2$sad$mean) #main effect of mood acroos sensibility conditions

#####-------------------------STATS

fileT <- file.path(out_dir, "sensibility_t.txt")
capture.output(tS, file = fileT)
capture.output(tH, file = fileT, append = T)
capture.output(tM, file = fileT, append = T)
capture.output(print("Boferroni corrected p-values (N = 3):"), file = fileT, append = T)
capture.output(print(p_adjust), file = fileT, append = T)
