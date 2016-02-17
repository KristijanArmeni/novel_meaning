#NOVEL_MEANING

#this is the script for analysing individual sentence meaning ratings
#by Kristijan Armeni

#####-------------------------INITIALISATION

library(ggplot2)
library(reshape)
library(plyr)
library(psych)
library(ez)
curr_dir = getwd()

inp_dir = 'D:/Kristijan/raziskovalno/novel_meaning/data/logs'
out_dir = 'D:/Kristijan/raziskovalno/novel_meaning/output/beh'

# set proper dir for reading in the files
if (curr_dir != inp_dir){
  setwd(inp_dir)
}

#create a list with all filenames from the dir
myfiles = list.files(pattern = '*log.txt')


#####-------------------------SUBJECT-LOOP

#loop through all the filenames (i.e. subjects)
for (i in 1:length(myfiles)){
  
  subject = substr(myfiles[i], 1, 3)
  
  sub_file <- read.table(myfiles[i], header = TRUE)
  
  if (i == 1)
  {sub_all <- sub_file}
  else
  {sub_all <- rbind(sub_all, sub_file)}
  
}

sub_all <- head(sub_all[, -c(6, 10)])
names(sub_all)[3] <- 'mood' 

#####-------------------------SUMMARIES

#function that is used to compute the mode below
my_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#create a summary data frame using ddply()
summary_data <- ddply(sub_all, ~sID + mood + condition,
                      summarise,
                      N = length(sens_rat),
                      min = min(sens_rat),
                      max = max(sens_rat),
                      mean = round(mean(sens_rat),2),
                      sd = round(sd(sens_rat), 2),
                      se = round(sd/sqrt(N), 2),
                      mode = my_mode(sens_rat))

#factor the relevant columns
summary_data$group <- factor(summary_data$mood)
summary_data$condition <- factor(summary_data$condition)

summary_means <- describeBy(summary_data$mean, list(summary_data$group, summary_data$condition))

#####-------------------------PLOTS

sum_plot <- ggplot(data = summary_data,
                          aes(x = condition, y = mean, color = group, group = group)) +
                          geom_point(position = position_jitter(width = 0.08)) +
                          geom_line() +
                          labs(x = 'semantic condition', y = 'rating', color = 'mood') +
                          ggtitle('sensibility ratings') +
                          ylim(0, 8)

sum_plot

#####-------------------------SAVING
ggsave("mean_ratings.pdf", sum_plot, path = out_dir)
write.table(summary_data, "ratings_sum.txt", quote = FALSE)