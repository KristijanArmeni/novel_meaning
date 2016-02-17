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

core_dir = 'D:/Kristijan/raziskovalno/novel_meaning'
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


#####-------------------------SHAPING-THE-DATA

sub_all <- sub_all[, -c(6, 10)]
names(sub_all)[3] <- 'mood'
names(sub_all)[4] <- 'paragrafnr'

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
summary_data$mood <- factor(summary_data$mood, levels = c(1, 2), labels = c('happy', 'sad'))
summary_data$condition <- factor(summary_data$condition, levels = c(1, 2), labels = c('sens', 'nsens'))

summary_split <- split(summary_data, f = list(summary_data$mood, summary_data$condition))

#table of means by group and condition
summary_means <- describeBy(summary_data$mean, list(summary_data$mood, summary_data$condition))
summary_agg <- aggregate(summary_data$mean, by = list(summary_data$mood, summary_data$condition), FUN = mean)
#####-------------------------PLOTS

sum_plot <- ggplot(data = summary_agg,
                          aes(x = Group.2, y = x, color = Group.1, group = Group.1)) +
                          geom_point() +
                          geom_line() +
                          labs(x = 'semantic condition', y = 'rating', color = 'mood') +
                          ggtitle('sensibility ratings') +
                          ylim(0, 8)

sum_plot

#####-------------------------SAVING

#saving R objects
save_data <- file.path(core_dir, 'preproc', 'sensibility_all.Rdata', fsep = '/')
save_summary <- file.path(out_dir, 'stats/descriptive', 'summary_sensibility.Rdata', fsep = '/')

save(sub_all, file = save_data) #save sensibility ratings for all subjects in one data frame
save(summary_data, file = save_summary) #saving summary dataframe

#saving plots & tables
ggsave("mean_ratings.pdf", sum_plot, path = out_dir)
write.table(summary_data, "ratings_sum.txt", quote = FALSE)