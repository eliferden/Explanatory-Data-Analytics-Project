# EDA Project - Step 2

setwd('C:/Users/Elif Erden/Desktop/EDA/EDA Proje')
GoogleApps <- read.csv("GoogleApps.csv")

library(ggplot2)

#------One Variable---------#

# Bar Plot for Content.Rating by Type
ggplot(GoogleApps, aes(x=Content.Rating))+ geom_bar(stat="count", width=0.5, color='purple')+
  theme_minimal() + facet_wrap(~Type)

#By looking at the Google Play Apps data set, it was identified that there are two types of 
#applications by their charge. These types are Free and Paid. Also, in the data set, content 
#ratings are divided into seven levels. In this bar plot, it was intended to observe the 
#number of applications by their content ratings and types. From the plot, it seems that 
#there is a huge difference between Everyone level and the other levels in Free type and also
#every level of content ratings in Free type have higher number of applications comparing to 
#the Paid type.

# Histogram for Size by Category 
ggplot(aes(x=Size..MB.), data=GoogleApps) +
  geom_histogram(aes(color = Category), fill = "white", bins = 50) + theme_minimal() +
  scale_y_log10() + scale_x_log10()

# Histogram for Installs by Rating
ggplot(aes(x=Installs),data = GoogleApps) + geom_histogram() + 
  scale_x_log10() + facet_wrap(~Rating)

#------Two Variables---------#
ggplot(aes(x=Reviews, y=Installs),data=GoogleApps) +
  geom_jitter(alpha = 0.1) 
# Scatter plot for Reviews and Installs
ggplot(aes(x=Reviews, y=Installs),data=GoogleApps) +
  geom_jitter(alpha = 0.1) +
  scale_x_log10() + scale_y_log10()

# Scatter plot for Rating and Installs
ggplot(aes(x=Rating, y=Installs),data=GoogleApps) +
  geom_jitter(alpha=0.2)+ scale_x_log10() + scale_y_log10() +
  geom_smooth(method=lm, se=FALSE, color="darkred")

# Scatter plot for Size and Installs
ggplot(aes(x=Size..MB., y=Installs), data = GoogleApps) + 
  geom_point(alpha = 0.2) 
+ scale_y_log10() + scale_x_log10()
