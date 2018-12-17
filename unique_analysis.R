library(openxlsx)
library(ggplot2)
library(dplyr)
library(wordcloud)
library(tm)

theme_joey <- function () { 
  theme_bw(base_size=12, base_family="Avenir") %+replace% 
    theme(axis.title.x= element_text("Words",size=12,angle=0,hjust=.5,vjust=0,face="plain") , axis.text.x = element_text("Words", size = 12, angle = 90, hjust = 1)
    )
}

data <- read.xlsx("Labels.xlsx")
analysis <- data %>% group_by(KMeans_Labels) %>% summarise(Total = n(), Deleted = sum(Deleted_Email)/Total, Morning = sum(Time_Morning)/Total, Working = sum(Time_Working)/Total, 
                                                           Night = sum(Time_Night)/Total, Number_Sent = mean(Number_Sent), Forwarded = sum(Forward)/Total, Replied = sum(Reply)/Total,
                                                           Images = mean(Images), URLs = mean(n_urls), Digits = mean(n_digits), Extraspace = mean(n_extraspaces), 
                                                           Capitals = mean(n_caps), Punctuations =mean(n_puncts), Words = mean(n_words))
# Personal (Thanks please)
words_label_0 <- read.xlsx("Cluster_0.xlsx")
names(words_label_0) = c("Words", "Frequency")

# Personal
words_label_1 <- read.xlsx("Cluster_1.xlsx")
names(words_label_1) = c("Words", "Frequency")

#Business
words_label_2 <- read.xlsx("Cluster_2.xlsx")
names(words_label_2) = c("Words", "Frequency")

#Fantasy
words_label_3 <- read.xlsx("Cluster_3.xlsx")
names(words_label_3) = c("Words", "Frequency")

#Spam
words_label_4 <- read.xlsx("Cluster_4.xlsx")
names(words_label_4) = c("Words", "Frequency")


ggplot(data = arrange(words_label_4,desc(Frequency))[1:20,], aes(x = reorder(Words,-Frequency), y = Frequency)) + geom_bar(stat="identity", fill = "#fdae6b") + theme_joey() + scale_x_discrete("Word")

wordcloud(words_label_0$Words, words_label_0$Frequency, color = "black")

ggplot(data = words_label_0, aes(x = reorder(Words,-Frequency), y = Frequency)) + geom_bar(stat="identity", fill = "#fdae6b") + theme_joey() + scale_x_discrete("Word")
