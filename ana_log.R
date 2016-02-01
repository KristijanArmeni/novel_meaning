#NOVEL_MEANING

#this is the script for analysing individual emotion/attention/motivation ratings
#by Kristijan Armeni

#####-------------------------INITIALISATION

library(ggplot2)
library(reshape)
curr_dir = getwd()

inp_dir = 'D:/Kristijan/raziskovalno/novel_meaning/data/logs'
out_dir = 'D:/Kristijan/raziskovalno/novel_meaning/output/beh'

# set proper dir for reading in the files
if (curr_dir != inp_dir){
  setwd(inp_dir)
}

#####-------------------------READ-IN

# read in the data
my_data <- read.table("Ratings.txt", header = TRUE)

#####-------------------------SHAPE-THE-DATA

#transform mood variable to factor
my_data$mood <- factor(my_data$mood, levels = c(1,2), labels = c("happy", "sad"))

#ratings
ratings <- data.frame(ID = my_data$sID,
                  group = my_data$mood,
                  BL = my_data$E1,
                  mip1 = my_data$E2,
                  mip2 = my_data$E3,
                  mip3 = my_data$E4,
                  att1 = my_data$A1,
                  att2 = my_data$A2,
                  mot1 = my_data$M1,
                  mot2 = my_data$M2)

#get summary statistics
sum_stat_BL <- aggregate(ratings$BL, by = list(ratings$group), FUN = summary)
sum_stat_mip1 <- aggregate(ratings$mip1, by = list(ratings$group), FUN = summary)
sum_stat_mip2 <- aggregate(ratings$mip2, by = list(ratings$group), FUN = summary)
sum_stat_mip3 <- aggregate(ratings$mip3, by = list(ratings$group), FUN = summary)

#reshaping my data for plotting
ratings2 <- melt(ratings, id = c('ID', 'group'))

groupMeans <- cast(ratings2, formula = group~mip, value = "rating", mean)

#####-------------------------PLOT-THE-DATA

#summary plots

mip = ratings2$mip
rating = ratings2$rating
group = ratings2$group

summary_plot <- ggplot(data = ratings2) +
                
                stat_summary(fun.data = 'mean_se',
                              position = position_dodge(width = 0.5),
                              aes(x = mip,
                                  y = rating,
                                  color = group)) +
                
                geom_abline(aes(x = mip,
                                y = rating,
                                color = group),
                            stat = 'summary')
summary_plot

#saving the plot
ggsave(filename = 'logs_plot', plot = summary_plot, path = out_dir)

#####-------------------------PRINT-THE-DATA
