
setwd('C:/Users/Elif Erden/Desktop/EDA/EDA Proje')
GoogleApps <- read.csv("GoogleApps.csv")

library(ggplot2)
library(GGally)

with(GoogleApps, plot(Reviews, Installs)) + 
  title(main="Reviews According to Ratings")

is.factor(GoogleApps$Type)

cor(GoogleApps[,c(3:6,8)]) 

with(GoogleApps, cor(Installs, Reviews))

ggpairs(GoogleApps,columns = c(3:6,8))

options(digits=4)

#Backward Selection
max_model <- lm(Installs ~ Category + Rating + Reviews + Size..MB. + Type + Price.... + Content.Rating + Genres ,data=GoogleApps)
stepmodel1 <- step(max_model, direction="backward")
summary(stepmodel1)

#Forward Selection
min_model <- lm( Installs ~ 1 , data =  GoogleApps)
stepmodel2 <- step(min_model, direction="forward", scope = list(lower=min_model, upper=max_model))
summary(stepmodel2)

#For Both
stepmodel3 <- step(max_model, direction="both")
summary(stepmodel3)

model1 <- lm(Installs ~ Category + Rating + Reviews + Size..MB. + Type, data=GoogleApps)
summary(model1)
names(model1)
model1$coefficients[1]

model2 <- lm(Installs ~ Reviews,data=GoogleApps)
summary(model2)

GoogleApps$Installs[700]
model2$residuals[700]
model2$fitted.values[700]

coef(model2)
resid(model2)
fitted(model2)

#Let's the data and the regression line together
plot(Installs~Reviews, data=GoogleApps) +
 abline(model1,col='red')

#Let's examine the residuals for each x variable
plot(GoogleApps$Reviews, resid(model1)) +
  abline(0,0,col='red')

#Log Transformation
ggplot(aes(x=Reviews, y=Installs),data=GoogleApps) +
  geom_jitter(alpha = 0.1) +
  scale_x_log10() + scale_y_log10()

model_w_t <- lm(log(Installs) ~ log(Reviews), data=GoogleApps)
summary(model_w_t)



