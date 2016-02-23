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

inp_dir = 'D:/Kristijan/raziskovalno/novel_meaning/data/logs/Rdata_logs'
out_dir = 'D:/Kristijan/raziskovalno/novel_meaning/output/beh/stats/tests'

# set proper dir for reading in the files
if (curr_dir != inp_dir){
  setwd(inp_dir)
}

#loading the dataset
load('ratings_selected.Rdata')
data <- sel_data

#####-------------------------INITIALISATION

#subseting and refactoring the loaded dataset for comparisons
ratings <- subset(data, data$scale_type == 'emo') #exclude attention & motivation ratings
ratings <- subset(ratings, ratings$scale != 'mip2') #exclude mip2 ratings
ratings$scale <- factor(ratings$scale)
ratings$group <- factor(ratings$group)
ratings$scale_type <- factor(ratings$scale_type)
ratings$ID <- factor(ratings$ID)

#spliting for t-tests
data_by_scale <- split(ratings, f = ratings$scale)
data_fullsplit <- split(ratings, f = list(ratings$scale, ratings$group))

#####-------------------------STATS

#ANOVA
des <- ezDesign(data = ratings, .(group), .(rating), col = .(scale))
summary(des)

#ezPlot
pl <- ezPlot(data = ratings, x = .(scale), dv = .(rating), within = .(scale), between = .(group), wid = .(ID),
             do_lines = F)
pl

an <- ezANOVA(data = ratings, dv = .(rating), within = .(scale), between = .(group), wid = .(ID), type = 3)
summary(an)

#post-hoc t-test: scale
t0 <- t.test(data_fullsplit$BL.happy$rating, data_fullsplit$BL.sad$rating)      #baseline
t1 <- t.test(data_fullsplit$mip1.happy$rating, data_fullsplit$mip1.sad$rating)  #mip1
t2 <- t.test(data_fullsplit$mip3.happy$rating, data_fullsplit$mip3.sad$rating)  #mip2

#post-hoc t-test group
t3 <- t.test(data_fullsplit$BL.happy$rating, data_fullsplit$mip1.happy$rating) #happy; BL vs. mip1
t4 <- t.test(data_fullsplit$mip1.happy$rating, data_fullsplit$mip3.happy$rating) #happy; mip1 vs. mip3
t5 <- t.test(data_fullsplit$BL.sad$rating, data_fullsplit$mip1.sad$rating) #happy; BL vs. mip1
t6 <- t.test(data_fullsplit$mip1.sad$rating, data_fullsplit$mip3.sad$rating) #happy; mip1 vs. mip3

pvalues <- c(t0$p.value, t1$p.value, t2$p.value) #store just the p-values

#adjust for multiple comparisons
p_adjusted <- p.adjust(pvalues, method = "BH")

#saving

aovFile <- file.path(out_dir, "mood_rating_aov.txt") #create long filename for ANOVA results
capture.output(an, file = aovFile) #save ANOVA results

tFile <- file.path(out_dir, "mood_rating_t.txt")
capture.output(t0, file = tFile)
capture.output(t1, file = tFile, append = T)
capture.output(t2, file = tFile, append = T)


