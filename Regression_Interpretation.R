#Regression with factors
#This script will show you how to circumvent interpretational difficulties with factors in regression models.
#Keep in mind: Factors are variables that have distinct levels: E.g. Gender, anything binary (Survival), Education, Employment etc. 

#Logistic Regression
#--------------------------------------------------------------------
#If you want to research influences ONTO a binary factor, choose a logistic model. We will do a quick example with the titanic dataset and see what contributes to survival. Install the package with the code below (uncomment first), if necessary.
#install.packages("titanic")
library(titanic)
dat <- titanic_train
dat$Pclass <- factor(dat$Pclass, levels = c("3","2","1"))
dat$Sex <- factor(dat$Sex, levels= c("male", "female"))
dat$Embarked <- as.factor(dat$Embarked)
fit <- glm(family = "binomial", Survived~Pclass + Sex + Age, data = dat)
summary(fit)

#This doesn't tell us much apart from significance. All we have to look for is: What does the intercept represent? In this case, the reference is a third class male passenger of mean age. All other logits are the survival probability fractions of their specific category changed (e.g. passenger class 2) in relation to this reference, transformed with ln. This is tedious to interpret. Basically, such a logistic regression is quite useless most of the time. To get some more info out of this, we can predict person parameters (in this case: survival probabilities) from the model and attach them to individual data:

pred_prob <- predict(fit, newdata = dat, type = "response")
dat$Prob <- pred_prob

#now we can plot the influence of factors as a function of actual survival probability. Using the full power of ggplot parameters, we can split up all variables and in so doing can see differences in sex, passenger class and age when it comes to survival probability. 

p <- ggplot(data = dat, aes(x=Age, y= Prob, color= Pclass))+
  geom_point(aes(shape= as.factor(Sex)))+
  geom_smooth(method = "glm", method.args = list(family="binomial"),
              se=F)+
  labs(title = "Logistic Regression: Titanic survival by predictors", y= "Survival Probability", x= "Age", shape = "Sex", col = "Passenger Class", caption = "Note: The black line at y = 0.5 represents the threshold of classification. The model would consider \nanything above this line survival and anything below this line death")+
  geom_hline(yintercept = 0.50)+
  theme(plot.caption = element_text(hjust=0))+
  apatheme

#The plot shows the following facts:
#The cheaper the passenger class, the lower the chance of survival.
#The older a passenger, the lower their chance of survival.
#Women have a vastly superior chance of survival than men in any class 
#you could not easily get this from the glm output. Neither will your readers. Plot such things. 

#In case of a multinomial logistic regression (if your criterion has more than 2 levels), you can use the nnet package. I will not add anything further to this, you can read up on advanced regression models here: https://bookdown.org/chua/ber642_advanced_regression/

#-------------------------------------------------------------------
#Multiple linear Regression (MLR)

#this is easier. Since you work with metric data, interpretation of beta-weights is more intuitive. Let's look at a common MLR that includes a factorial predictor with 3 levels and a metric predictor combined with an interaction of both. 
#install.packages("car")

library(car)
dat <- mtcars
dat$gear <- factor(dat$gear, levels = c("3","4","5"))
fit2 <- lm(data=dat, hp~gear+cyl+ wt)
summary(fit2)

#Here we can see from the output that the intercept is significant at -99. The intercept in this case is the horsepower of a car that has 3 gears, an average amount of cylinders (6) and an average weight (3200 pounds). However, the horsepower for this car is -99. This does not make sense whatsoever, therefore we cannot interpret this estimate in a meaningful way. 
#let's instead look at the standardized versions of this regression to solve this problem.

fit3 <- lm(data=dat, scale(hp)~gear+scale(cyl)+scale(wt))
summary(fit3)

#Standardization centers data at the mean and divides it through its standard deviation. Therefore, relative deviation from the mean is expressed as a multitude of a standard deviation.
#We can now ignore the intercept, as it does not prove to be significant. All other effects however remained significant. We can now say that gear 5 on average significantly increases horsepower by 1.2 standard deviations compared to the a car that has 3 gears. adding one standard deviation of cylinders will on average increase horse power by 0.7 standard deviations. Now let's plot this with significant predictors!

ggplot(dat, aes(x = scale(cyl), y= scale(hp), color = gear))+
  geom_point()+
  geom_smooth(method = "lm", se= FALSE)+
  apatheme+
  labs(y = "sd(Horsepower)", 
       x="sd(Cylinders)", 
       col="Gear", 
       title = "Horsepower predicted by Cylinders and Gears")

#we can see the regression graphs and also an interesting relationship in the data: Three-geared cars do not have many cylinders, while four- and five-geared cars have more. This gives rise to the notion that Gears and Cylinders could have an interaction effect: The more cylinders you have, the more gears you would need to drive efficiently (or vice versa, strictly speaking).
#Obviously, you could have deduced this from theory/prior knowledge beforehand and added it to the initial model. Let's pretend we know nothing about cars for now.
#let's test this hypothesis and also omit weight with a new model, as it did not prove significant at alpha = 0.05. We predict that the new model performs better in terms of goodness of fit and has a significant interaction effect:

fit4 <- lm(data=dat, scale(hp)~scale(cyl)+gear + scale(cyl)*gear)
summary(fit4)


#We can see: We could get a significant interaction from our model and push R-Squared by 2 percent by omitting (!) an insignificant variable and including an interaction.  
