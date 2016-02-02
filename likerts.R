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

#####-------------------------SUMMARIES

names(summary_data)[2] <- 'group'
summary_data$group <- factor(summary_data$group)
summary_data$condition <- factor(summary_data$condition)


my_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

summary_data <- ddply(sub_all, ~sID + Condition + condition,
                      summarise,
                      N = length(sens_rat),
                      min = min(sens_rat),
                      max = max(sens_rat),
                      mean = mean(sens_rat),
                      sd_min   = round(mean - sd(sens_rat),2),
                      sd_max   = round(mean + sd(sens_rat), 2),
                      mode = my_mode(sens_rat))

#####-------------------------PLOTS

sum_plot <- ggplot(data = summary_data,
                          aes(x = group, y = mean, color = condition, group = group)) +
                          geom_point(position = position_jitter(width = 0.08)) +
                          #geom_errorbar(aes(ymin=sd_min, ymax=sd_max), width=.1, position = position_jitter(width = 0.08)) +
                          labs(x = 'group', y = 'rating', color = 'semantic condition') +
                          ggtitle('sensibility ratings') +
                          ylim(0, 8)

sum_plot

#####-------------------------SAVING
ggsave("mean_ratings.pdf", sum_plot, path = out_dir)
write.table(summary_data, "ratings_sum.txt", quote = FALSE)