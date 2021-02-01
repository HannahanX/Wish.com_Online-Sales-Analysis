df <- read.csv("E:/5510/summer-products-with-rating-and-performance_2020-08.csv")


## check missing value and recode
colSums(is.na(df))
# recode missing value in has_urgency_banner as 0
df$has_urgency_banner[is.na(df$has_urgency_banner)] <- 0
# delete missing value
df <- na.omit(df)

# Check the distribution of target
library(ggplot2)
ggplot(df, aes(x=units_sold))+
  geom_histogram(color="darkblue", fill="lightblue")

# check overdispersion
mean(df$units_sold)
var(df$units_sold)
# it has overdispersion, so we should use negative binomial instead of poisson

# minmax-normalization
set.seed(123)
normalise <- function(x, na.rm = TRUE) {
  ranx <- range(x, na.rm = na.rm)
  (x - ranx[1]) / diff(ranx)
}
apply(df[,c('price','rating','merchant_rating')],2,normalise)

df <- data.frame(df, apply(df[,c('price','rating','merchant_rating')],2,normalise))
colnames(df)
drop <- c('price','rating','merchant_rating')
df = df[,!(names(df) %in% drop)]
names(df)[names(df) == 'price.1'] <- 'price'
names(df)[names(df) == 'rating.1'] <- 'rating'
names(df)[names(df) == 'merchant_rating.1'] <- 'merchant_rating'

# We will only include price, rating, merchant_rating, uses_ad_boosts in model
# because those are the key features that influence units sold we believe.
# In my model, we will build a linear regression model, and negative binomial model for comparsion.
ml <- lm(units_sold~price+rating+merchant_rating+as.factor(uses_ad_boosts)+
           as.factor(merchant_has_profile_picture),
           data=df)
summary(ml)

library(MASS)
nml<-glm(units_sold~price+rating+merchant_rating+as.factor(uses_ad_boosts)+
            as.factor(merchant_has_profile_picture),
            family=negative.binomial(1),
            data=df)
summary(nml)

library("lmtest")
lrtest(ml,nml)
# looks like nml model has better result.

# we have doubt on the coefficient of ad group
# try to run a psm
# PSM1: for uses_ads_boost
# first we test if price, rating, merchant_rating and profile are different with and without ads
summary(ml2<-lm(price~uses_ad_boosts, data=df))
summary(ml3<-lm(rating~uses_ad_boosts, data=df))
summary(ml4<-lm(merchant_rating~uses_ad_boosts, data=df))
summary(ml5<-lm(merchant_has_profile_picture~uses_ad_boosts, data=df))
# price and rating are significantly different with ads, 
# so we conduct propensity scoring matching to make two group comparable

# install.packages("MatchIt")
library("MatchIt")
psm <- matchit(uses_ad_boosts~price+rating, data=df, 
               method = "nearest", ratio=1,
               distance='logit', caliper=0.0001)
summary(psm)

# After PSM, we match fair number of control and treated group
data_psm=subset(cbind(df,weights=psm$weights),weights==1)
View(data_psm)
nml2<-glm(units_sold~price+ merchant_rating +rating+as.factor(uses_ad_boosts)
          +as.factor(merchant_has_profile_picture), 
         family=negative.binomial(1), data=data_psm,
         control=glm.control(maxit=500))
summary(nml2)
# coefficient of ad group is not significant
# since we cannot conclude that ad have significant impact from our dataset.

# check balance between products from control and treatment after match
summary(ml<-lm(price~uses_ad_boosts, data=data_psm))
summary(ml<-lm(rating~uses_ad_boosts, data=data_psm))

# PSM2: for merchant_has_profile_picture
summary(ml2<-lm(price~merchant_has_profile_picture, data=df))
summary(ml3<-lm(rating~merchant_has_profile_picture, data=df))
summary(ml4<-lm(uses_ad_boosts~merchant_has_profile_picture, data=df))
summary(ml5<-lm(merchant_rating~merchant_has_profile_picture, data=df))

psm2 <- matchit(merchant_has_profile_picture~merchant_rating, data=df, 
               method = "nearest", ratio=1,
               distance='logit', caliper=0.0001)
summary(psm2)

data_psm2=subset(cbind(df,weights=psm$weights),weights==1)
View(data_psm2)
nml3<-glm(units_sold~price+rating+merchant_rating+as.factor(uses_ad_boosts)
          +as.factor(merchant_has_profile_picture), 
          family=negative.binomial(1), data=data_psm2,
          control=glm.control(maxit=500))
summary(nml3)
# merchant_has_profile_picture has significant postive impact on units sold.

# test rating and merchant rating have same impact on units sold or not.
library("car")
linearHypothesis(nml,"rating=merchant_rating")
# from the result, we can reject the null hypothesis that they have same effect on units sold.