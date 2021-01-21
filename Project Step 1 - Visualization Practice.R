
getwd()
setwd('C:/Users/Elif Erden/Desktop/EDA/EDA Proje')

library(readxl)
googleplayexcel <- read_excel("GoogleApps.xlsx")
write.csv(googleplayexcel, "C:/Users/Elif Erden/Desktop/GoogleApps.csv", row.names = FALSE)

GoogleApps <- read.csv("GoogleApps.csv")
#view(GoogleApps)

#------------------------------------------------------------------one variable

head(GoogleApps)
names(GoogleApps)
summary(GoogleApps)
summary(GoogleApps$Price....)
summary(GoogleApps$Size..MB.)

library(ggplot2)

sum(is.na(GoogleApps))

#histogram for size
qplot(x=Size..MB., data=GoogleApps)
ggplot(aes(x=Size..MB.),data=GoogleApps) +
  geom_histogram()+
  scale_x_log10()

#histogram for price
qplot(x=Price...., data=GoogleApps)
qplot(x=Price...., data=GoogleApps,binwidth = 50) +
  scale_y_continuous(limits = c(0,15))
qplot(x=Price...., data=GoogleApps, binwidth = 50) + 
  scale_x_continuous(limits=c(0,450),breaks=seq(0,450,50)) +
  scale_y_continuous(limits = c(0,15))
qplot(x=Price...., data=GoogleApps, binwidth = 10) + 
  scale_x_continuous(limits=c(0,150),breaks=seq(0,150,25)) +
  scale_y_continuous(limits = c(0,10))

#boxplot for rating
boxplot(GoogleApps$Size..MB.)
outlier_values <- boxplot.stats(GoogleApps$Size..MB.)$out
summary(outlier_values)

#facet_wrap between reviews and type
ggplot(aes(x=Reviews),data=GoogleApps) +
  geom_histogram() +
  facet_wrap(~Type)
#limits are added to visualize better and x limits are added based on first and third quartiles of reviews
summary(GoogleApps$Reviews)
ggplot(aes(x=Reviews),data=GoogleApps) +
  geom_histogram() +
  scale_x_continuous(limits = c(127,44238)) +
  scale_y_continuous(limits = c(0,750)) +
  facet_wrap(~Type)
#log10 transformation for x is added
ggplot(aes(x=Reviews),data=GoogleApps) +
  geom_histogram() +
  scale_x_continuous(limits = c(127,44238)) +
  scale_y_continuous(limits = c(0,750)) +
  facet_wrap(~Type) + 
  scale_x_log10()

#facet_wrap between size and type
ggplot(aes(x=Size..MB.),data=GoogleApps) +
  geom_histogram() + 
  facet_wrap(~Type)
#log10 transformation for x is added
ggplot(aes(x=Size..MB.),data=GoogleApps) +
  geom_histogram() + scale_x_log10() +
  facet_wrap(~Type)

#--------------------------------------------------------------------two variables

#scatter plot for rating and reviews
ggplot(aes(x=Rating, y=Reviews),data=GoogleApps) +
  geom_point()
ggplot(aes(x=Rating, y=Reviews),data=GoogleApps) +
  geom_point() + scale_y_log10()
ggplot(aes(x=Rating, y=Reviews),data=GoogleApps) +
  geom_point(alpha=0.1) + scale_y_log10()

#scatter plot for reviews and installs
ggplot(aes(x=Reviews, y=Installs),data=GoogleApps) +
  geom_point()
#log transformation is added for both variables
ggplot(aes(x=Reviews, y=Installs),data=GoogleApps) +
  geom_point() + scale_x_log10() + scale_y_log10()
#alpha is added to avoid overlaying
ggplot(aes(x=Reviews, y=Installs),data=GoogleApps) +
  geom_point(alpha = 0.1) + scale_x_log10() + scale_y_log10()
#jitter is added to avoid stepwise visualization
ggplot(aes(x=Reviews, y=Installs),data=GoogleApps) +
  geom_jitter(alpha = 0.1) + scale_x_log10() + scale_y_log10()

#scatter plot for rating and installs
ggplot(aes(x=Rating, y=Installs),data=GoogleApps) +
  geom_point()
ggplot(aes(x=Rating, y=Installs),data=GoogleApps) +
  geom_point()+ scale_x_log10()+ scale_y_log10()
ggplot(aes(x=Rating, y=Installs),data=GoogleApps) +
  geom_jitter()+ scale_x_log10() + scale_y_log10()
