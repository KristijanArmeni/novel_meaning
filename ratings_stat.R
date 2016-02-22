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
out_dir = 'D:/Kristijan/raziskovalno/novel_meaning/output/beh'

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

#test for normality

#ANOVA
des <- ezDesign(data = ratings, .(group), .(rating), col = .(scale))
summary(des)

an <- ezANOVA(ratings, .(rating), within = .(scale), between = .(group), wid = .(ID))
summary(an)

aov <- aov(data = ratings, formula = rating ~ group*scale + Error(ID/scale))
summary(aov)

#post-hoc t-test
t0 <- t.test(data_fullsplit$BL.happy$rating, data_fullsplit$BL.sad$rating)      #baseline
t1 <- t.test(data_fullsplit$mip1.happy$rating, data_fullsplit$mip1.sad$rating)  #mip1
t2 <- t.test(data_fullsplit$mip3.happy$rating, data_fullsplit$mip3.sad$rating)  #mip2

pvalues <- c(t0$p.value, t1$p.value, t2$p.value) #store just the p-values

#adjust for multiple comparisons
p_adjusted <- p.adjust(pvalues, method = "bonferroni")

