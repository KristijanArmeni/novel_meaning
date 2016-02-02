#NOVEL_MEANING

#this is the script for analysing individual emotion/attention/motivation ratings
#by Kristijan Armeni

#####-------------------------INITIALISATION

library(ggplot2)
library(reshape)
library(plyr)
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

#regenerating a data.frame with renamed columns
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


#melting my data for plotting
ratings2 <- melt(ratings, id = c('ID', 'group'))
names(ratings2)[3] <- 'scale'
names(ratings2)[4] <- 'rating'

#adding a new column vector scale_type conditioned on scale column
ratings2$scale_type <- ratings2$scale
levels(ratings2$scale_type) <- c(levels(ratings2$scale_type), "emo_")
levels(ratings2$scale_type) <- c(levels(ratings2$scale_type), "ctrl")
ratings2[ratings2$scale %in% c('BL', 'mip1', 'mip2', 'mip3'), "scale_type"] <- 'emo_'
ratings2[ratings2$scale %in% c('att1', 'att2', 'mot1', 'mot2'), "scale_type"] <- 'ctrl'

groupMeans <- cast(ratings2, formula = group~mip, value = "rating", mean)

#####-------------------------SUMMARIZE-AND-PLOT-THE-DATA

#computing means and se (two ways for safety check)
sum_data <- ddply(ratings2, ~group+scale+scale_type, function(x) round(mean_se(x$rating), 1))

sum_data2 <- ddply(ratings2, c("group", 'scale'), summarise,
                            N    = length(rating),
                            mean = round(mean(rating),2),
                            sd   = round(sd(rating),2),
                            se   = round(sd / sqrt(N), 2),
                            se_up = round((mean - se), 2),
                            se_low = round((mean + se), 2))

sum_data2

summary_plot2 <- ggplot(data = sum_data,
                        aes(x = scale, y = y, color = group, group = group)) +
                geom_errorbar(aes(ymin=ymin, ymax=ymax), width=.1) +
                geom_line() +
                geom_point() +
                labs(x = 'rating', y = 'mip', color = 'group') +
                ylim(-10, 10)

summary(summary_plot)
summary_plot

summary_plot2 <- ggplot(data = sum_data2,
                       aes(x = scale, y = mean, color = group, group = group)) +
                geom_errorbar(aes(ymin=se_low, ymax=se_up), width=.1) +
                geom_line() +
                geom_point() +
                labs(x = 'rating', y = 'mip', color = 'group') +
                ylim(-10, 10)

summary(summary_plot2)
summary_plot2

#####-------------------------PRINT-THE-DATA

ggsave("logs_plot2.pdf", summary_plot2, path = out_dir)
write.table(sum_data2, "logs_sum.txt", quote = FALSE)
