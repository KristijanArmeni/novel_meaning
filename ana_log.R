#NOVEL_MEANING

#this is the script for analysing individual emotion/attention/motivation ratings
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

#adding a new column vector scale_type ('emo' or 'ctrl') conditioned on the scale column
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

#computing means and se (two ways for safety check)
sum_data <- ddply(ratings, ~group+scale+scale_type, function(x) round(mean_se(x$rating), 1))

sum_data_2 <- ddply(ratings, c("group", 'scale'), summarise,
                            N    = length(rating),
                            mean = round(mean(rating),2),
                            sd   = round(sd(rating),2),
                            se   = round(sd / sqrt(N), 2),
                            se_up = round((mean - se), 2),
                            se_low = round((mean + se), 2))


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
summary_plot_2 <- ggplot(data = sum_data_2,
                       aes(x = scale, y = mean, color = group, group = group)) +
                geom_errorbar(aes(ymin=se_low, ymax=se_up), width=.1) +
                geom_line() +
                geom_point() +
                labs(x = 'rating', y = 'mip', color = 'group') +
                ylim(-10, 10)

summary(summary_plot_2)
summary_plot_2


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

#computing means and se (two ways for safety check)
sum_dataSel_scaleType <- ddply(sel_data, ~group+scale+scale_type, function(x) round(mean_se(x$rating), 1))

sum_dataSel_groupScale <- ddply(sel_data,
                                c("group", 'scale'),
                                summarise,
                                      N    = length(rating),
                                      mean = round(mean(rating),2),
                                      sd   = round(sd(rating),2),
                                      se   = round(sd / sqrt(N), 2),
                                      se_up = round((mean - se), 2),
                                      se_low = round((mean + se), 2))

sum_dataSel_groupScale

#mean + error_bar plot for all scales (x), both groups (color) and mean ratings (y)
summary_plot_sel <- ggplot(data = sum_dataSel_scaleType,
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
                                        labels=c("happy\n(N = 15)", "sad\n(N = 13)")) +
  
                    scale_x_discrete(breaks=c("BL", "mip1", "mip2", "mip3", "att1", "att2", "mot1", "mot2"),
                                     labels=c("baseline", "mood-1", "mood-2", "mood-3", "att-1", "att-2", "mot-1", "mot-2"))

summary(summary_plot_sel)
summary_plot_sel


#####-------------------------STATS

#subseting the original dataset for comparisons
ratings_emotion <- subset(sel_data, sel_data$scale_type == 'emo')

BL <-   subset(ratings_emotion, ratings_emotion$scale == 'BL')
mip1 <- subset(ratings_emotion, ratings_emotion$scale == 'mip1')
mip2 <- subset(ratings_emotion, ratings_emotion$scale == 'mip2')
mip3 <- subset(ratings_emotion, ratings_emotion$scale == 'mip3')

#subseting for t-tests
BL_h <- subset(BL, BL$group == 'happy')
BL_s <- subset(BL, BL$group == 'sad')
mip1_h <- subset(mip1, mip1$group == 'happy') 
mip1_s <- subset(mip1, mip1$group == 'sad') 
mip2_h <- subset(mip2, mip2$group == 'happy') 
mip2_s <- subset(mip2, mip2$group == 'sad') 
mip3_h <- subset(mip3, mip3$group == 'happy') 
mip3_s <- subset(mip3, mip3$group == 'sad') 

#ANOVA
des <- ezDesign(data = ratings_emotion, .(group), .(rating), col = .(scale))
summary(des)
an <- ezANOVA(ratings_emotion, .(rating), within = .(scale), between = .(group), wid = .(ID))
summary(an)

#post-hoc t-test
BL_ttest <- t.test(BL_h, BL_s,)

#####-------------------------PRINT-THE-GENERAL-SUMMARY-DATA

#creating file names with paths
raw_data <- file.path(inp_dir, 'Rdata_logs', 'ratings.Rdata', fsep = '/')
reshaped_data <- file.path(inp_dir, 'Rdata_logs', 'ratings_reshaped.Rdata', fsep = '/')
raw_data_selected <- file.path(inp_dir, 'Rdata_logs', 'ratings_selected.Rdata', fsep = '/')

saveData <- file.path(out_dir, 'sumData_scaleType.Rdata', fsep = '/')
saveData2 <- file.path(out_dir, 'sumData_groupScale.Rdata', fsep = '/')

#save R objects
save(sum_dataSel_scaleType, file = saveData) 
save(sum_dataSel_groupScale, file = saveData2)
save(my_data, file = raw_data)                   #imported data, just relabeled
save(ratings, file = reshaped_data)              #reshaped imported data
save(sel_data, file = raw_data_selected)         #EEG-specific data

#save the plots and tables
ggsave("ratings_plot.pdf", summary_plot, width = 8, height = 5, path = out_dir)
ggsave("ratings_selected.pdf", summary_plot_sel, width = 8, height = 5, path = out_dir)
write.table(sum_data_2, "rating_summary.txt", quote = FALSE)
