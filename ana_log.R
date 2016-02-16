#NOVEL_MEANING

#this is the script for analysing individual emotion/attention/motivation ratings
#by Kristijan Armeni

#####-------------------------INITIALISATION

library(ggplot2)
library(reshape)
library(plyr)
library(psych)
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

#relabeling the columns
my_data <- data.frame(ID = my_data$sID,
                  group = my_data$mood,
                  BL = my_data$E1,
                  mip1 = my_data$E2,
                  mip2 = my_data$E3,
                  mip3 = my_data$E4,
                  att1 = my_data$A1,
                  att2 = my_data$A2,
                  mot1 = my_data$M1,
                  mot2 = my_data$M2)


#reshaping and adding scale as a column-variable - RATINGS data frame
ratings <- melt(my_data, id = c('ID', 'group'))
names(ratings)[3] <- 'scale'
names(ratings)[4] <- 'rating'

#adding a new column vector scale_type conditioned on scale column
ratings$scale_type <- ratings$scale

levels(ratings$scale_type) <- c(levels(ratings$scale_type), "emo")
levels(ratings$scale_type) <- c(levels(ratings$scale_type), "ctrl")

ratings[ratings$scale %in% c('BL', 'mip1', 'mip2', 'mip3'), "scale_type"] <- 'emo'
ratings[ratings$scale %in% c('att1', 'att2', 'mot1', 'mot2'), "scale_type"] <- 'ctrl'



#####-------------------------GENERAL-SUMMARY

summary <- describeBy(ratings$rating, list(ratings$group,ratings$scale), mat = TRUE)

#rounding specific columns
selected_columns <- c(1, 2, 3, 4, 5, 8, 11, 12, 13)
summary[, -selected_columns] <- round(summary[,-selected_columns], 1)

#summaries using cast()
groupMeans <- cast(ratings, formula = group~scale, value = "rating", mean)

groupSd <- cast(ratings, formula = group~scale, value = "rating", sd)
groupSd[, -1] <- round(groupSd[, -1], 2)

#computing means and se (two ways for safety check)
sum_data <- ddply(ratings, ~group+scale+scale_type, function(x) round(mean_se(x$rating), 1))

sum_data2 <- ddply(ratings, c("group", 'scale'), summarise,
                            N    = length(rating),
                            mean = round(mean(rating),2),
                            sd   = round(sd(rating),2),
                            se   = round(sd / sqrt(N), 2),
                            se_up = round((mean - se), 2),
                            se_low = round((mean + se), 2))

sum_data2


#####-------------------------PLOTS

#mean + error_bar plot for all scales (x), both groups (color) and mean ratings (y)
summary_plot <- ggplot(data = sum_data,
                        aes(x = scale, y = y, color = group, group = group)) +
                geom_errorbar(aes(ymin=ymin, ymax=ymax), width=.1) +
                geom_line() +
                geom_point() +
                labs(x = 'rating label', y = 'mean rating', color = 'group') +
                ylim(-10, 10) +
                ggtitle("Emotion, attention and motivation ratings") + 
                theme(plot.title = element_text(vjust = 1.5)) +
  
                scale_colour_discrete(name="mood",
                                      breaks=c("happy", "sad"),
                                      labels=c("happy\n(N = 16)", "sad\n(N = 16)"))

summary(summary_plot)
summary_plot

#same as above, just using different data frame (safety check)
summary_plot2 <- ggplot(data = sum_data2,
                       aes(x = scale, y = mean, color = group, group = group)) +
                geom_errorbar(aes(ymin=se_low, ymax=se_up), width=.1) +
                geom_line() +
                geom_point() +
                labs(x = 'rating', y = 'mip', color = 'group') +
                ylim(-10, 10)

summary(summary_plot2)
summary_plot2


#####-------------------------EEG_BASED_SUMMARY_AND_PLOTS

#lists with selected subject ID's (based on useful EEG data)
sad_subs    = c('s04', 's06', 's08', 's10', 's12', 's16', 's18', 's20', 's22', 's26', 's28', 's30', 's32')
happy_subs  = c('s01', 's03', 's05', 's07', 's09', 's11', 's15', 's17', 's19', 's21', 's23', 's25', 's27', 's29', 's31')
selected_subjects = c(happy_subs, sad_subs)

#this is the data frame with subject that have useful EEG data
sel_data = ratings[ratings$ID %in% selected_subjects, ]

#start summarizin the selected data
summary_sel <- describeBy(sel_data$rating, list(sel_data$group, sel_data$scale), mat = TRUE)

#rounding specific columns
selected_columns <- c(1, 2, 3, 4, 5, 8, 11, 12, 13)
summary_sel[, -selected_columns] <- round(summary_sel[,-selected_columns], 1)

#summaries (mean & SD) using cast()
groupMeans_sel <- cast(sel_data, formula = group~scale, value = "rating", mean)
groupMeans_sel[, -1] <- round(groupMeans_sel[, -1], 2)

groupSd_sel <- cast(sel_data, formula = group~scale, value = "rating", sd)
groupSd_sel[, -1] <- round(groupSd_sel[, -1], 2)

#computing means and se (two ways for safety check)
sum_data_check <- ddply(sel_data, ~group+scale+scale_type, function(x) round(mean_se(x$rating), 1))

sum_data_sel <- ddply(sel_data, c("group", 'scale'), summarise,
                   N    = length(rating),
                   mean = round(mean(rating),2),
                   sd   = round(sd(rating),2),
                   se   = round(sd / sqrt(N), 2),
                   se_up = round((mean - se), 2),
                   se_low = round((mean + se), 2))

sum_data_sel

#mean + error_bar plot for all scales (x), both groups (color) and mean ratings (y)
summary_plot_sel <- ggplot(data = sum_data_check,
                       aes(x = scale, y = y, color = group, group = group)) +
                    geom_errorbar(aes(ymin=ymin, ymax=ymax), width=.1) +
                    geom_line() +
                    geom_point() +
                    
                    labs(x = 'rating label', y = 'mean rating', color = 'group') +
                    ylim(-10, 10) +
                    
                    ggtitle("Emotion, attention and motivation ratings") + 
                    theme(plot.title = element_text(vjust = 1.5, face = 'bold')) +
                    
                    scale_colour_discrete(name="mood",
                                        breaks=c("happy", "sad"),
                                        labels=c("happy\n(N = 15)", "sad\n(N = 13)"))

summary(summary_plot_sel)
summary_plot_sel

#####-------------------------PRINT-THE-GENERAL-SUMMARY-DATA

ggsave("logs_plot2.pdf", summary_plot, path = out_dir)
write.table(sum_data2, "rating_summary.txt", quote = FALSE)
