###############################################################
# Title:        ps5.r
# Author:       Apurva Agrawal
# Date:         2017-12-5
# Description:  turn in file for PS-5
###############################################################


###################################### QUESTION 1 ######################################
rm(list=ls())

library(data.table)

# data import and validation
context1 <- fread("wage1.csv")

## K-means Estimation
seed        <-	2
maxClusters	<-	10

## WSS to choose k
wss	<- rep(-1,maxClusters)
for (i in 1:maxClusters) {
  set.seed(seed)
  model <- kmeans(context1,centers=i,nstart=10)
  wss[i] <- model$tot.withinss
}

plot(1:maxClusters,	wss, type="b", 
     xlab="Number of Clusters",
     ylab="Aggregate Within Group SS")

## k chosen as 3 from wss plot
set.seed(seed)
model1 <- kmeans(context1,centers=3,nstart=10)
model1$centers
groups1 <- model1$cluster
groups1

## Segmenting
context1 <- fread("wage1.csv")
context1$cluster <- groups1


model2 <- lm(wage~educ+exper+tenure,data=context1[cluster==1])
model3 <- lm(wage~educ+exper+tenure,data=context1[cluster==2])
model4 <- lm(wage~educ+exper+tenure,data=context1[cluster==3])

summary(model2)
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  3.78799    2.83502   1.336 0.184419    
# educ         0.40735    0.10276   3.964 0.000135 ***
# exper       -0.10172    0.05851  -1.739 0.085077 .  
# tenure       0.13653    0.03098   4.407 2.55e-05 ***
summary(model3)
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -3.21053    0.81295  -3.949 0.000101 ***
#   educ         0.52524    0.06025   8.717 3.49e-16 ***
#   exper        0.17286    0.03806   4.542 8.55e-06 ***
#   tenure       0.29156    0.06613   4.409 1.52e-05 ***
summary(model4)
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) -4.85740    2.03152  -2.391    0.018 *  
#   educ         0.73631    0.11888   6.194 5.30e-09 ***
#   exper        0.05905    0.05871   1.006    0.316    
#   tenure       0.21796    0.04655   4.683 6.26e-06 ***

# Interpretations:
# a. Using the elbow test on the within- sum of squares plot, find the optimal number o clusters for this data
#    set.
#    The optimal number of clusters for this data from looking at the elbow curve is 2.
#
# b. Looking at the means from model1, describe the different clusters. [Hint: Look at the education,
#                                                                       experience, and tenure variables in particular.]
#    Cluster 1 consits of employees with relatively low educ but very high exper and tenure; wages on lower side
#    cluster 2 consits of employees with relatively higher educ than other 2 clusters, but very low exper and tenure; 
#    wages similar to cluster 1 
#    cluster 3 consits of employees with medium level of educ, exper and tenure; higher wages
#
# c. Discuss the differences between models 2, 3, and 4.
#    Model 3, all the 3 variables educ, exper and tenure are significant and an increase in any 
#    of these variables increases wage.
#    Experience doesn't seem to be significant in model 2 and model 4, and has a negative correlation in model2. 
#    The coefficients of model 4 for educ and tenure are greater than model 2, implying that the 
#    effect of increase in these variables will be higher in cluster 3 than cluster 2.
#    The coefficients of model 2 for educ and tenure are smaller than model 3, implying that the 
#    effect of increase in these variables will be lower in cluster 1 than cluster 2.


###################################### QUESTION 2 ######################################

# importing data and validation
context2 <- read.csv("ffportfolios.csv")
library(tseries)

# kpss test loop
for(i in 2:33) {
kpss.test(as.matrix(context2)[,i],null="Level")
}
warnings()


Xdata <- context2[2:ncol(context2)]
model5 <- prcomp(Xdata)
screeplot(model5,type="lines") 
factors <- model5$x[,1]

head(factors)
summary(factors)
var(factors)

# creating factor variable and standardizing it 
context2$factor <- factors
context2$factor <- scale(context2$factor)
var(context2$factor)
summary(context2)
ts.plot(context2$factor)

# years corresponding to factor <-2.58
years<- subset(context2,factor < -2.58)
years$Year


# Interpretations
# a. Based on the scree plot, how many principal componenets should we use for this data?
#    Based on the screenplot the number of prinicpal components we should use is 1.
#
# b. Looking at the years where the standardized factor is less than the first percentile (-2.58), how would
#    you characterize this principal component?
#
#   [1] 1970.250 1973.833 1978.750 1980.167 1987.750 1998.583 2001.667 2002.500 2008.750
#   These years correspond to the lowest variations observed in the portforlio returns which has been captured by
#   this principal component 
#

