# Stats and Snacks - 
# EcoLunch 3/21/16
# Led by: Briana K. Whitaker

# to run a line of code, have clicker on that line, "Command | Enter"
# Commenting Code - do this always, use a hash (#)


# clear your working environment
rm(list=ls())

################################################################################
#### INTRODUCTION TO R
# types of data
# values
x <- 5
x 
y <- "Briana"
y
# vectors
x1 <- c(5,4,3,2,1)
x1
y1 <- c("Briana", "Is", "Awesome")
y1
# lists
?list
# matrices (non-mixed sets of data)
x2 <- matrix(c(1,1,2,1,2,1,1,1,3),ncol=3, nrow=3)
x2
y2 <- matrix(c(rep("y",3),rep("n",2),"y"), ncol=2)
y2
# data frames (mixed sets of data)
# Height vs. Wingspan (Armspan)
my.dat <- data.frame( hgt <- c(67,73,74,63,64,70,67,65.5,72,63), 
                      wng <- c(67,70.5,76,62,61,68,69,65.5,74,62) )
my.dat  #messy column names, let's fix it
colnames(my.dat) <- c("hgt", "wng")
my.dat

# let's plot this just for funzies
plot(hgt ~ wng, data=my.dat)
plot(hgt ~ wng, data=my.dat, pch=16, col='navy', ylab="Height", xlab="Wingspan")



# see what is in your data collection
# PS : "?" is a way to see the help files for a function
# "??" is a way to search for potential matching functions or calls
?names
?str
?is.numeric
?is.character
?is.matrix
?is.data.frame
?length
?dim
?rownames
?colnames
?levels


# examples
dim(y2)
length(x1)
is.numeric(x)
is.character(x)


# How to Install and Load Packages into R
# library(ggplot2)
library(car)

################################################################################

# Cut to theory lesson

################################################################################
#### INTRODUCTION TO ANOVA
?chickwts
head(chickwts)
dim(chickwts)
str(chickwts)
names(chickwts)

# Let's start with the chickwts data

# perform visual diagnostics and data visualization (do this first, always)
qqnorm(chickwts$weight)
qqline(chickwts$weight)
hist(chickwts$weight, breaks=12)
plot(density(chickwts$weight))
# Shapiro-Wilk's test of normality
# if p-value >0.05 or 0.10 then normal
shapiro.test(chickwts$weight) 

# Create a boxplot
plot(weight ~ feed, data=chickwts)

# create a linear model
?lm
# Y = XB + e , where we estimate Beta's, and receive estimated Y's
# the error is unobservable, but we do get residuals as the difference between
# our observed and our fitted Y's, as an estimate of the error
wts1 <- lm(weight ~ feed, data=chickwts)
summary(wts1)
names(wts1)
names(summary(wts1))

# Type 1 Anova
anova(wts1)
# Type 2 Anova
#Anova(wts1)
# Type 3 Anova
#Anova(wts1, type=3)

# anova by hand using the Sum-of-Squares Method (i.e., the hard way)
#levels(chickwts$feed)
#table(chickwts$feed)
n <- as.numeric(table(chickwts$feed))
N <- sum(n)
# Mean Chicken weight, for each feed type
meanc<-mean(chickwts$weight[chickwts$feed=="casein"])
meanh<-mean(chickwts$weight[chickwts$feed=="horsebean"])
meanl<-mean(chickwts$weight[chickwts$feed=="linseed"])
meanm<-mean(chickwts$weight[chickwts$feed=="meatmeal"])
meanso<-mean(chickwts$weight[chickwts$feed=="soybean"])
meansu<-mean(chickwts$weight[chickwts$feed=="sunflower"])
# Mean chick-wgt across all feed type categories
meanY <- mean(chickwts$weight)
# Total sum-of-squares of Y
SYY <- sum((chickwts$weight-meanY)^2)

# Between sum-of-squares
SSB<-n[1]*(meanc-meanY)^2 + n[2]*(meanh-meanY)^2 + n[3]*(meanl-meanY)^2 + 
   n[4]*(meanm-meanY)^2 + n[5]*(meanso-meanY)^2 + n[6]*(meansu-meanY)^2
# degrees of freedom between categories of chicken feed
between.df <- length(levels(chickwts$feed))-1   ;   between.df 
# Between Mean-Square
between.meansquare<-SSB/between.df
# Within sum-of-squares
SSW <- sum((chickwts$weight[chickwts$feed=="casein"]-meanc)^2) + 
       sum((chickwts$weight[chickwts$feed=="horsebean"]-meanh)^2) + 
       sum((chickwts$weight[chickwts$feed=="linseed"]-meanl)^2) + 
       sum((chickwts$weight[chickwts$feed=="meatmeal"]-meanm)^2) + 
       sum((chickwts$weight[chickwts$feed=="soybean"]-meanso)^2) + 
       sum((chickwts$weight[chickwts$feed=="sunflower"]-meansu)^2)
within.df<-(N-length(levels(chickwts$feed)))   ;   within.df
within.meansquare<-SSW/within.df

# perform the ANOVA test
Fstat <- between.meansquare/within.meansquare    ; Fstat
pval<-(1-pf(Fstat,df1=between.df,df2=within.df)) ; pval

# Construct the anova table
my.anova<-data.frame(rbind(
            c("Between", between.df, SSB, between.meansquare, Fstat, pval),
          	c("Within", within.df, SSW, within.meansquare, "", "")   ))
colnames(my.anova)<-c("","DF","Sum Sq","Mean Sq","F-val","P-Val")
my.anova
anova(wts1)

names(summary(wts1))
summary(wts1)$fstatistic   #Global F-Test
summary(wts1)$sigma        # Residual standard Error of the model
RSS <- anova(wts1)$"Sum Sq"[2]    # Residual sum of squares of the model (unexplained variance)
# RSS <- sum((chickwts$weight-wts1$fitted.values)^2) 

SYY                        # another way of thinking about SYY, is how much variation is there available to explain?
R2 <- 1- RSS/SYY   ; R2
summary(wts1)$r.squared
SSreg <- SYY-RSS  ; SSreg    # Sum of Squares of Regression 
# SSreg <- sum((wts1$fitted.values-meanY)^2) 
# SSreg = how much of the variation available to explain (SYY) was actually exaplained by regression using your predictors? 
# SYY = RSS + SSreg
# visualize this concept = the whole world is variation, and all the points merely players....
par(mfrow=c(1,2))
boxplot(chickwts$weight, ylab="Weight")
boxplot(weight ~ feed, data=chickwts)


################################################################################
### Diagnostics on the residuals (we will not cover this, but the code is here)
hist(wts1$residuals)          # visualize
shapiro.test(wts1$residuals)  # are the residuals normally distributed?
plot(wts1$residuals ~ wts1$fitted.values)   # residuals plot
leveneTest(wts1)              # are the residuals homoschedastic/constant variance
outlierTest(wts1)             # are there any outliers?
# ?outlierTest  ;  # Outlier test using studentized residuals & Bonferroni-correction
# cooks.distance(wts1)   ; this tests for influential points
plot(wts1, which=4)           # visual test of cook's distance, which tests for influential points
abline(h=(4/(N-length(wts1$coefficients)-1)), col='red', lty=2)
# the rule of thumb for high-influence is cooks.distance > 4/n-k-1


################################################################################

# Cut to theory lesson

################################################################################
#### INTRODUCTION TO ANOVA - with matrices

?Loblolly
head(Loblolly)
dim(Loblolly)
str(Loblolly)
names(Loblolly)

par(mfrow=c(1,1))
plot(height ~ age, data=Loblolly)
abline(lm(height ~ age, data=Loblolly))
plot(height ~ Seed, data=Loblolly)

hist(Loblolly$height)
shapiro.test(Loblolly$height)
# show's the height data is non-normal
# can use the Box-Cox method for choosing a transformation (not discused here)
summary(powerTransform(lm(height ~ age, data=Loblolly)))  
# or can just try basic tranformations yourself
hist(sqrt(Loblolly$height))
hist(log(Loblolly$height))
shapiro.test(log(Loblolly$height))
 # these show there may not be a good transformation, will try model and look at residuals

pine <- lm(height ~ age + Seed, data=Loblolly)
summary(pine)
# would you want to do the sum-of squares method for the Seed factor variable???
# heck no
# so let's try matrices method
# PS - Seed variable might actually be more appropriate as a random intercept...

anova(pine)

y <- Loblolly$height
mu <- mean(Loblolly$height)      ;  mu
xage <- Loblolly$age
Xseed <- model.matrix( ~ Seed - 1 , data=Loblolly)
head(dummy)
Xfull <- model.matrix( ~ age + Seed - 1 , data=Loblolly)
head(Xfull)

# Sum of Squares of Y
SYYp <- sum((Loblolly$height - mean(Loblolly$height))^2)  ; SYYp

# intercept model
intercept.model <- lm(Loblolly$height ~ 1)
# compute residuals, or take from output ; (y - intercept.model$fitted.values)
# compute RSS
RSS0 <- sum((intercept.model$residuals)^2)    ;   RSS0
# wait a minute, note that this equals SYYp, the intercept model is the same
#   as the SYY

# add a regressor to model
# another way to think of this, is adding a 'column to your X-matrix
age.model <- lm(Loblolly$height ~ Loblolly$age)
# compute RSS
RSSage <- sum((age.model$residuals)^2)    ;   RSSage

seed.model <- lm(Loblolly$height ~ Loblolly$Seed)
RSSseed <- sum((seed.model$residuals)^2)    ;   RSSseed

full.model <- lm(Loblolly$height ~ Loblolly$age + Loblolly$Seed)
RSSf <- sum((full.model$residuals)^2)    ;   RSSf

# define degrees.of.freedom
df0 <- length(y) - 1    # minus one because we estimate the regressor
dfage <- length(y) - 1 - 1  # because we estimate intercept and one continuous.regressor
dfseed <- length(y) - 1 - (length(levels(Loblolly$Seed)) -1)  # because we estimate intercept, and k-1 regressors for the dummy variables of the catergorical predictor
dff <- length(y) - 1 - (length(levels(Loblolly$Seed)) -1) -1
df0; dfage; dfseed; dff


# compute F-Statistics for EACH X regressor

# this answers the question, what is the variation explained by adding ALL REGRESSORS to the model
GlobalF <- ( (RSS0-RSSf)/(df0-dff) ) / (RSSf/dff)
GlobalF
pvalGlobal  <- (1-pf(GlobalF,df1=(df0-dff),df2=dff)) ; pvalGlobal


# this answers the question, what is the additional variation explained by adding Seed to a model that already contains age?
SeedF <- ( (RSSage-RSSf)/(dfage-dff) ) / (RSSf/dff)
SeedF
pvalSeed  <- (1-pf(SeedF,df1=(dfage-dff),df2=dff)) ; pvalSeed

# this answers the question, what is the additional variation explained by adding age to a model that already contains Seed?
AgeF <- ( (RSSseed-RSSf)/(dfseed-dff) ) / (RSSf/dff)
AgeF
pvalAge  <- (1-pf(AgeF,df1=(dfseed-dff),df2=dff)) ; pvalAge

# does this match what we saw in the anova table?
anova(pine)
# does the global F-test match what we saw in the summary of the model?
# (look down at the very bottom of the summary table)
summary(pine)


# Type 2 Anova     
# this type of Anova follows the marginal means method
# it is much more appropriate choice for ANOVA when dealing with many regressors
Anova(pine)
Anova(pine, type=3)



# end