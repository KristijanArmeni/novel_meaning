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

#factor the relevant columns
sub_all$mood <- factor(sub_all$mood, levels = c(1, 2), labels = c('happy', 'sad'))
sub_all$condition <- factor(sub_all$condition, levels = c(1, 2), labels = c('sens', 'nsens'))

#####-------------------------SUMMARIES

#function that is used to compute the mode below
my_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#create a summary data frame using ddply()
summary_data <- ddply(sub_all, ~ mood + condition,
                      summarise,
                      N = length(sens_rat),
                      min = min(sens_rat),
                      max = max(sens_rat),
                      mean = round(mean(sens_rat),2),
                      sd = round(sd(sens_rat), 2),
                      se = round(sd/sqrt(16), 2),
                      median = median(sens_rat),
                      mode = my_mode(sens_rat))

sum_dat_persub <- ddply(sub_all, ~ sID + mood + condition,
                      summarise,
                      N = length(sens_rat),
                      min = min(sens_rat),
                      max = max(sens_rat),
                      mean = round(mean(sens_rat),2),
                      sd = round(sd(sens_rat), 2),
                      se = round(sd/sqrt(N), 2),
                      median = median(sens_rat),
                      mode = my_mode(sens_rat))

summary_split <- split(summary_data, f = list(summary_data$mood, summary_data$condition))

#table of means by group and condition
summary_means <- describeBy(summary_data$mean, list(summary_data$mood, summary_data$condition))
summary_mean <- aggregate(summary_data$mean, by = list(summary_data$mood, summary_data$condition), FUN = mean)
summary_median <- aggregate(summary_data$mean, by = list(summary_data$mood, summary_data$condition), FUN = median)


#####-------------------------PLOTS

#custom function for changing facet labels on histogram
mf_labeller <- function(var, value){
  value <- as.character(value)
  if (var=="mood") { 
    value[value=="happy"] <- "happy (N = 16)"
    value[value=="sad"]   <- "sad (N = 16)"
  }
  return(value)
}

#histogram across subjects
my_hist <- ggplot(data = sub_all, aes(x = sens_rat)) +
           geom_histogram(binwidth = 1,
                          position = 'identity',
                          origin = - 0.5,
                          alpha = 0.5,
                          fill = 'white',
                          color = 'black') +
           
           facet_grid(condition ~ mood, labeller = mf_labeller) + 
           
           geom_vline(data=summary_data, aes(xintercept = mean),
                      linetype="dashed", size=1, colour="red") +
           
           scale_x_continuous(breaks = 1:7) + 
           labs(x = 'sensibility rating') +
           ggtitle('Sensibility rating counts\n(across subjects)') +
           theme(plot.title = element_text(vjust = 1.5),
                 strip.text.x = element_text()) +
           
           scale_fill_discrete(breaks=c("sens", "nsens"),
                               labels=c("sensible", "non-\nsensible"))
my_hist

#means per subject and grand average plot
plot <-     ggplot(data = sum_dat_persub,
                   aes(x = condition, y = mean)) +
            geom_point(size = 3,
                       stroke = 3,
                       color = 'darkgreen',
                       fill = 'white',
                       shape = 21,
                       position = position_jitter(width = 0.15)) +
  
            geom_point(data = summary_data,
                       aes(y = mean),
                       color = '#CC0000',
                       size = 4) +
            
            geom_errorbar(data =summary_data,
                          aes(ymax = mean + se, ymin = mean - se),
                          color = '#CC0000',
                          width = 0.10,
                          size = 1) +
  
            facet_grid(facets = . ~ mood) +
            labs(x = 'semantic condition', y = 'mean rating') +
            ggtitle('Sensibility ratings\n(mean per subject)') +
            theme(plot.title = element_text(vjust = 1.5)) +
            ylim(1, 7)
plot


#####-------------------------SAVING

#saving R objects
save_data <- file.path(core_dir, 'preproc', 'sensibility_all.Rdata', fsep = '/')
save_summary <- file.path(out_dir, 'stats/descriptive', 'summary_sensibility.Rdata', fsep = '/')
save_sum_persub <- file.path(out_dir, 'stats/descriptive', 'sum_sensibility_persubjects.Rdata', fsep = '/')
save_plot <- file.path(out_dir, 'plots', fsep = '/')

save(sub_all, file = save_data) #save sensibility ratings for all subjects in one data frame
save(summary_data, file = save_summary) #saving summary dataframe
save(sum_dat_persub, file = save_sum_persub) #saving summary dataframe per each subject

#saving plots & tables
ggsave("sensibility_ratings.png", plot, width = 8, height = 5, path = save_plot)
ggsave("sensibility_hist.png", my_hist, width = 5, height = 5, path = save_plot)
write.table(summary_data, "ratings_sum.txt", quote = FALSE)