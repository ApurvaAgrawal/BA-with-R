###############################################################
# Title:        ps3.r
# Author:       Apurva Agrawal
# Date:         2017-10-30
# Description:  Turn-in product for problem set 3
###############################################################

#clear environment
rm(list=ls(all=TRUE))

#import packages
library(data.table)
library(sandwich)
library(lmtest)
library(tseries)
library(plm)

########################### QUESTION 1 ######################

#Data import and validation
context1 <- fread('hprice1.csv')
summary(context1)

model1<-lm(price~bdrms+lotsize+sqrft, data=context1)
##summary(model1)

coeftest(model1,vcov.=vcov) ##OLS test
# 
# t test of coefficients:
#   
#               Estimate  Std. Error t value  Pr(>|t|)    
# (Intercept) -2.1770e+01  2.9475e+01 -0.7386  0.462208    
# bdrms        1.3853e+01  9.0101e+00  1.5374  0.127945    
# lotsize      2.0677e-03  6.4213e-04  3.2201  0.001823 ** 
# sqrft        1.2278e-01  1.3237e-02  9.2751 1.658e-14 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

coeftest(model1,vcov.=vcovHC) ## White corrected test 

# t test of coefficients:
#   
#                Estimate  Std. Error t value Pr(>|t|)   
# (Intercept) -21.7703086  41.0326944 -0.5306 0.597124   
# bdrms        13.8525219  11.5617901  1.1981 0.234236   
# lotsize       0.0020677   0.0071485  0.2893 0.773101   
# sqrft         0.1227782   0.0407325  3.0143 0.003406 **
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

model2<-lm(log(price)~bdrms+log(lotsize)+log(sqrft), data=context1)
## summary(model2)

coeftest(model2,vcov.=vcov) ##OLS test
# 
# t test of coefficients:
#   
#               Estimate Std. Error t value  Pr(>|t|)    
# (Intercept)  -1.297042   0.651284 -1.9915   0.04967 *  
# bdrms         0.036958   0.027531  1.3424   0.18308    
# log(lotsize)  0.167967   0.038281  4.3877 3.307e-05 ***
# log(sqrft)    0.700232   0.092865  7.5403 5.006e-11 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

coeftest(model2,vcov.=vcovHC) ## White corrected test 
# 
# t test of coefficients:
#   
#               Estimate Std. Error t value  Pr(>|t|)    
# (Intercept)  -1.297042   0.850457 -1.5251  0.130988    
# bdrms         0.036958   0.035576  1.0389  0.301845    
# log(lotsize)  0.167967   0.053275  3.1528  0.002243 ** 
# log(sqrft)    0.700232   0.121392  5.7683 1.298e-07 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# a. Identify which variables are significant using the OLS test for model1.
#    --> Lotsize, at 1% level and sqrft, at 0.1 % level
#
# b. Which variables are still significant after using the White-corrected 
#    significance test for model1?
#    --> sqrft, at 1% level
#
# c. Identify which variables are significant using the OLS test for model2.
#    --> log(lotsize),at 0.1% level and log(sqrft), at 0.1% level
#
# d. Which variables are still significant after using the White-corrected
#    significance test for model1?
#    --> log(lotsize),now at 1% level and log(sqrft), at 0.1% level
#
# e. Keeping these results in mind, what is the effect of taking logs on 
#    heteroskedasticity in the data?
#    -->  Log transformation corrects the heteroskedasticity in the explanatory
#         variables. Hence, after applying log, we can see that OLS and White-corrected 
#         significance tests are similar.

########################### QUESTION 2 ######################


#Data import and validation
context2 <- fread('beveridge.csv')
summary(context2)

ts.plot(context2$urate)
ts.plot(context2$vrate)

model3 <- lm(urate ~ vrate, data=context2)
summary(model3)

# 
# Call:
#   lm(formula = urate ~ vrate, data = context2)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.1399 -0.9063 -0.1726  0.7893  2.9342 
# 
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  17.1194     0.5920   28.92   <2e-16 ***
# vrate        -3.7414     0.2068  -18.09   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.202 on 133 degrees of freedom
# Multiple R-squared:  0.7111,	Adjusted R-squared:  0.7089 
# F-statistic: 327.3 on 1 and 133 DF,  p-value: < 2.2e-16

coeftest(model3,vcov=NeweyWest(model3,lag=5))
# 
# t test of coefficients:
#   
#              Estimate Std. Error t value  Pr(>|t|)    
# (Intercept) 17.11942    1.36561  12.536 < 2.2e-16 ***
# vrate       -3.74145    0.39575  -9.454 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

kpss.test(context2$urate,null="Level")
# KPSS Test for Level Stationarity
# 
# data:  context2$urate
# KPSS Level = 2.6835, Truncation lag parameter = 2, p-value = 0.01
kpss.test(context2$urate,null="Trend")
# KPSS Test for Trend Stationarity
# 
# data:  context2$urate
# KPSS Trend = 0.74085, Truncation lag parameter = 2, p-value = 0.01
kpss.test(context2$vrate,null="Level")
# KPSS Test for Level Stationarity
# 
# data:  context2$vrate
# KPSS Level = 1.2735, Truncation lag parameter = 2, p-value = 0.01
kpss.test(context2$vrate,null="Trend")
# KPSS Test for Trend Stationarity
# 
# data:  context2$vrate
# KPSS Trend = 0.43255, Truncation lag parameter = 2, p-value = 0.01

ts.plot(diff(context2$urate))
ts.plot(diff(context2$vrate))

kpss.test(diff(context2$urate),null="Level") ## good
# KPSS Test for Level Stationarity
# 
# data:  diff(context2$urate)
# KPSS Level = 0.25265, Truncation lag parameter = 2, p-value = 0.1
kpss.test(diff(context2$urate),null="Trend")
# KPSS Test for Trend Stationarity
# 
# data:  diff(context2$urate)
# KPSS Trend = 0.2695, Truncation lag parameter = 2, p-value = 0.01
kpss.test(diff(context2$vrate),null="Level") ## good
# KPSS Test for Level Stationarity
# 
# data:  diff(context2$vrate)
# KPSS Level = 0.30923, Truncation lag parameter = 2, p-value = 0.1
kpss.test(diff(context2$vrate),null="Trend")
# KPSS Test for Trend Stationarity
# 
# data:  diff(context2$vrate)
# KPSS Trend = 0.20946, Truncation lag parameter = 2, p-value = 0.01245

ts.plot(diff(diff(context2$urate)))
ts.plot(diff(diff(context2$vrate)))

kpss.test(diff(diff(context2$urate)),null="Level") ## good
kpss.test(diff(diff(context2$urate)),null="Trend") ## good
kpss.test(diff(diff(context2$vrate)),null="Level") ## good
kpss.test(diff(diff(context2$vrate)),null="Trend") ## good

model4 <- lm(diff(urate) ~ diff(vrate) + t[2:135], data=context2)
summary(model4)
# 
# Call:
#   lm(formula = diff(urate) ~ diff(vrate) + t[2:135], data = context2)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.53268 -0.13873 -0.03323  0.07785  0.67070 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)
# (Intercept)  0.0491276  0.0364617   1.347    0.180
# diff(vrate) -0.0241672  0.1080456  -0.224    0.823
# t[2:135]    -0.0001759  0.0004627  -0.380    0.704
# 
# Residual standard error: 0.2065 on 131 degrees of freedom
# Multiple R-squared:  0.001602,	Adjusted R-squared:  -0.01364 
# F-statistic: 0.1051 on 2 and 131 DF,  p-value: 0.9003
coeftest(model4,vcov=NeweyWest(model4,lag=5))
# t test of coefficients:
#   
#                Estimate  Std. Error t value Pr(>|t|)
# (Intercept)  0.04912761  0.04079194  1.2043   0.2306
# diff(vrate) -0.02416722  0.08545720 -0.2828   0.7778
# t[2:135]    -0.00017589  0.00076747 -0.2292   0.8191

# f. Do the OLS and NeweyWest significance tests show that the coefficient on the vanancy rate is significant
#     or not (before we correct for stationarity)?
#    --> Yes, vrate is significant in both the tests
#
# g. Based on the KPSS findings, which transformation/transformations should we apply to the unemployment
#     rate before modeling?
#    -->  First difference
#
# h. Based on the KPSS findings, which transformation/transformations should we apply to the vacancy rate
#     before modeling?
#   --> First difference
#
# i. How have the significance tests changed from model3 to model4?
#   --> vrate is no more a significant variable after correcting for stationarity
#
# j. Which model better describes the data?
#   --> Model 4.

########################### QUESTION 3 ######################


#Data import and validation
context3 <- fread('JTRAIN.csv')
summary(context3)

#generating new variables
n <- nrow(context3)
for (i in 1:n)
{
if (context3$year[i] == 1988) {
  context3$d88[i] <- 1
} else {
  context3$d88[i] <- 0
}
}  

for (i in 1:n)
{
  if (context3$year[i] == 1989) {
    context3$d89[i] <- 1
  } else {
    context3$d89[i] <- 0
  }
}  

context3$grant_minus_one <- 0
i <- 1
while(i<n)
{
  if (((context3$year[i] == 1987)| (context3$year[i] == 1988)) & (context3$grant[i] == 1))
    {
    context3$grant_minus_one[i+1] <- 1
  } else {
    context3$grant_minus_one[i+1] <- 0
  }
  i<-i+1
}

context3 <- plm.data(context3,index=c("fcode","year"))

model5 <- plm(log(scrap)~d88+d89+grant+grant_minus_one,model="pooling",data=context3)
model6 <- plm(log(scrap)~d88+d89+grant+grant_minus_one,model="within",data=context3)

summary(model5)
# > summary(model5)
# Pooling Model
# 
# Call:
#   plm(formula = log(scrap) ~ d88 + d89 + grant + grant_minus_one, 
#       data = context3, model = "pooling")
# 
# Balanced Panel: n=54, T=3, N=162
# 
# Residuals :
#   Min.  1st Qu.   Median  3rd Qu.     Max. 
# -5.20260 -0.89599 -0.08461  1.02417  3.30029 
# 
# Coefficients :
#   Estimate Std. Error t-value Pr(>|t|)   
# (Intercept)      0.597434   0.203063  2.9421 0.003754 **
#   d88             -0.239370   0.310864 -0.7700 0.442447   
# d89             -0.496524   0.337928 -1.4693 0.143748   
# grant            0.200020   0.338285  0.5913 0.555186   
# grant_minus_one  0.048936   0.436066  0.1122 0.910792   
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Total Sum of Squares:    355.75
# Residual Sum of Squares: 349.59
# R-Squared:      0.017311
# Adj. R-Squared: -0.0077257
# F-statistic: 0.691427 on 4 and 157 DF, p-value: 0.59893
summary(model6)
# > summary(model6)
# Oneway (individual) effect Within Model
# 
# Call:
#   plm(formula = log(scrap) ~ d88 + d89 + grant + grant_minus_one, 
#       data = context3, model = "within")
# 
# Balanced Panel: n=54, T=3, N=162
# 
# Residuals :
#   Min.   1st Qu.    Median   3rd Qu.      Max. 
# -2.286936 -0.112387 -0.017841  0.144272  1.426674 
# 
# Coefficients :
#   Estimate Std. Error t-value Pr(>|t|)  
# d88             -0.080216   0.109475 -0.7327  0.46537  
# d89             -0.247203   0.133218 -1.8556  0.06634 .
# grant           -0.252315   0.150629 -1.6751  0.09692 .
# grant_minus_one -0.421590   0.210200 -2.0057  0.04749 *
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Total Sum of Squares:    32.25
# Residual Sum of Squares: 25.766
# R-Squared:      0.20105
# Adj. R-Squared: -0.23684
# F-statistic: 6.5426 on 4 and 104 DF, p-value: 9.7741e-05
summary(model6, vcov=vcovHC(model6, method = "arellano"))
# > summary(model6, vcov=vcovHC(model6, method = "arellano"))
# Oneway (individual) effect Within Model
# 
# Note: Coefficient variance-covariance matrix supplied: vcovHC(model6, method = "arellano")
# 
# Call:
#   plm(formula = log(scrap) ~ d88 + d89 + grant + grant_minus_one, 
#       data = context3, model = "within")
# 
# Balanced Panel: n=54, T=3, N=162
# 
# Residuals :
#   Min.   1st Qu.    Median   3rd Qu.      Max. 
# -2.286936 -0.112387 -0.017841  0.144272  1.426674 
# 
# Coefficients :
#   Estimate Std. Error t-value Pr(>|t|)  
# d88             -0.080216   0.095719 -0.8380  0.40393  
# d89             -0.247203   0.192514 -1.2841  0.20197  
# grant           -0.252315   0.140329 -1.7980  0.07507 .
# grant_minus_one -0.421590   0.276335 -1.5256  0.13013  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Total Sum of Squares:    32.25
# Residual Sum of Squares: 25.766
# R-Squared:      0.20105
# Adj. R-Squared: -0.23684
# F-statistic: 7.38691 on 4 and 53 DF, p-value: 8.3412e-05
# > 
# 
#  k. Interpret the estimated coe¢ cient on grantit in model5.
#     --> The fact that the firm received a grant this year has an associated increase of 20% in the scrap rate, over time
#        considering all the firms, controlling for year variables d88,d89 and the fact that the firm received a grant
#        in the previous year
# 
#  l. Interpret the estimated coe¢ cient on granti;t-????1 in model5.
#     --> The fact that the firm received a grant previous year has an assocated increase of 4.8% in the scrap rate, over time
#        considering all the firms, controlling for year variables d88,d 89 and the fact that the firm received a grant
#        in this year
# 
#  m. How do you interpret the signs of B3 and B4?
#     --> The positive signs indicate a positive relation between the grant varaiables and the scarp rate
#         i.e, the fact that firm received a grant has an associated increase in the scrap rate.
# 
#  n. Interpret the estimated coefficient on grantit in model6.
#     --> If the firm receives a grant this year, it experiences an associated decrease of 25% units in the
#         scrap rate over the considered time period, controlling for the year variables d88 and d89 and the fact that
#         whether or not it recieved the grant the previous year.
# 
#  o. Interpret the estimated coefficient on granti;t-????1 in model6.
#     --> The fact that the firm received a grant in the previous year is associated with a decareae
#         of 42% units in the scrap rate over the considered period of time, controlling for year varaibles
#         d88 and d89 and the fact that whether or not it received the grant this year
#
#  p. How do you interpret the signs of B3 and B4 now?
#     --> the negative sign on B3 and B4 indicates a decrease in scrap rate with the fact that the firm received a grant
#         in any of the considered years which makes sense as the grant seems to increase the productivity of the firm.
# 
#  q. How do the significance results change from using the HAC (Arellano) significance results compared to
#     OLS?
#     --> d89 and grant minus one variable are not significant after HAC test results. Grant variable remians significant 
#         at 10% level

# pooled model: bwtn each firm : grants & scrap rate are positively correlated bcoz higher scrap rate firms were given grants
#                      