###############################################################
# Title:        ps2.r
# Author:       Apurva Agrawal
# Date:         2017-09-21
# Description:  Turn-in product for problem set 2
###############################################################

#clear environment
rm(list=ls(all=TRUE))

#import packages
library(data.table)

########################### QUESTION 1 ######################

#Data import and validation
context1 <- fread('attend.csv')
summary(context1)

#new variables
attendrt <- (context1$attend/32)
hwrt <- (context1$hw/8)

#model1 -- equation 1
#  termGPA(i) = B0 + B1*priGPA(i) + B2*ACT(i) + B3*attendrt(i) + B4*hwrt(i) + e(i)
#run model1
model1 <- lm(termGPA~priGPA+ACT+attendrt+hwrt, data=context1)

#summarize model1
summary(model1)
# Call:
#   lm(formula = termGPA ~ priGPA + ACT + attendrt + hwrt, data = context1)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.87210 -0.28100  0.00001  0.30164  1.49711 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) -1.286983   0.164169  -7.839 1.77e-14 ***
#   priGPA       0.548962   0.042418  12.942  < 2e-16 ***
#   ACT          0.036099   0.006051   5.966 3.92e-09 ***
#   attendrt     1.052246   0.155436   6.770 2.81e-11 ***
#   hwrt         0.913031   0.116932   7.808 2.22e-14 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.4788 on 675 degrees of freedom
# Multiple R-squared:   0.58,	Adjusted R-squared:  0.5775 
# F-statistic:   233 on 4 and 675 DF,  p-value: < 2.2e-16

#INTERPRETATIONS
# equation 1 :
# termGPA(i) = -1.286983 + 0.548962*priGPA(i) + 0.036099*ACT(i) + 
#               1.052246*attendrt(i) + 0.913031*hwrt(i) + e(i)
#
# a.  Interpret the estimated coefficient on attendrt from model1 (eq 1).
#     --> An attendrt of 1 for the student i.e. the student attends all 32 classes, is associated with the 
#     increase in termGPA by 1.0522, controlling for priGPA, ACT score and the homework rate.

# b.  Interpret the estimated coefficient on hwrt from model1 (eq 1).
#     --> The hwrt of 1 for the student i.e. the student turns in all 8 homeworks, is associated with the
#     increase in termGPA by 0.9130, controlling for priGPA, ACT score and the attendance rate.

# c.  Predict the termGPA for a student with a 32 ACT and a 2.2 priGPA who attended 28 lectures and
#     turned-in 8 homework assignments.
      termGPA_c = -1.286983 + 0.548962*2.2 + 0.036099*32 + 
                 1.052246*(28/32) + 0.913031*(8/8)
      termGPA_c
#     [1] 2.909648
#     --> The predicted termGPA is 2.909648
      
# d.  Predict the termGPA for a student with a similar attendence and homework pattern who had a 20 ACT
#     and a 3.9 priGPA.
      termGPA_d = -1.286983 + 0.548962*3.9 + 0.036099*20 + 
        1.052246*(28/32) + 0.913031*(8/8)
      termGPA_d
#     [1] 3.409695
#     --> The predicted termGPA is 3.409695
      
# e.  Intuitively, which variable is more important to the termGPA, ACT or priGPA?
#     --> Comparing p-values for ACT and priGPA, since priGPA has a smaller p-value than ACT, 
#     priGPA is more important to the termGPA.
#     another way to compare:  std(act)*b2 VS  std(prigpa)*b1
      
      
# f.  Predict the termGPA for a student with a 25 ACT and a 3.0 priGPA who attends all the classes, but
#     only finishes half the homework assignments.
      termGPA_f = -1.286983 + 0.548962*3.0 + 0.036099*25 + 
        1.052246*(32/32) + 0.913031*(4/8)
      termGPA_f
#     [1] 2.77114
#     --> The predicted termGPA is 2.77114

# g.  Predict the termGPA for a similarly qualified student who turns in all the homwork assignments, but
#     only attends half the classes.
      termGPA_g = -1.286983 + 0.548962*3.0 + 0.036099*25 + 
        1.052246*(16/32) + 0.913031*(8/8)
      termGPA_g
#     [1] 2.701532
#     --> The predicted termGPA is 2.701532

# h.  Intuitively, which variable is more important to the termGPA, attendance or homework completion?
#     --> Comparing p-values for hwrt and attendance, since hwrt has a smaller p-value than attendrt, 
#     hwrt is more important to the termGPA.

# i.  Why is it easier to compare attendrt and hwrt than it is to compare priGPA and ACT score?
#     --> attendrt and hwrt are the rates which can be compared just with the magnitudes. On the other hand, 
#     priGPA and ACT are on different scales and thus to be compared, they must be converted into ratios
#     to make conclusions considering their magnitudes.  

############################# QUESTION 2 ###############################

#data import and validation
context2 <- fread('CEOSAL2.csv')
summary(context2)

# model 2 - equation 2:
# ln[salary(i)] = B0 + B1*ln[mktval(i)] + B2*profits(i) + B3*ceoten(i) + e(i)
model2 <- lm(log(salary)~log(mktval)+profits+ceoten, data= context2)

summary(model2)
# Call:
#   lm(formula = log(salary) ~ log(mktval) + profits + ceoten, data = context2)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.63382 -0.34660  0.00627  0.35059  1.96220 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) 4.7095052  0.3954502  11.909  < 2e-16 ***
#   log(mktval) 0.2386220  0.0559166   4.267 3.25e-05 ***
#   profits     0.0000793  0.0001566   0.506   0.6132    
#   ceoten      0.0114646  0.0055816   2.054   0.0415 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.5289 on 173 degrees of freedom
# Multiple R-squared:  0.2514,	Adjusted R-squared:  0.2384 
# F-statistic: 19.36 on 3 and 173 DF,  p-value: 7.141e-11


# model 3 - equation 3:
# ln[salary(i)] = B0 + B1*ln[mktval(i)] + B2*profits(i) + B3*ceoten(i) + B4*ln[sales(i)] + e(i)
model3 <- lm(log(salary)~log(mktval)+profits+ceoten+log(sales), data= context2)
summary(model3)
# 
# Call:
#   lm(formula = log(salary) ~ log(mktval) + profits + ceoten + log(sales), 
#      data = context2)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.48792 -0.29369  0.00827  0.29951  1.85524 
# 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 4.558e+00  3.803e-01  11.986  < 2e-16 ***
# log(mktval) 1.018e-01  6.303e-02   1.614   0.1083    
# profits     2.905e-05  1.503e-04   0.193   0.8470    
# ceoten      1.168e-02  5.342e-03   2.187   0.0301 *  
# log(sales)  1.622e-01  3.948e-02   4.109 6.14e-05 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.5062 on 172 degrees of freedom
# Multiple R-squared:  0.3183,	Adjusted R-squared:  0.3024 
# F-statistic: 20.08 on 4 and 172 DF,  p-value: 1.387e-13


# Interpretations
#  equation 2:
#  ln[salary(i)] = 4.7095052 + 0.2386220*ln[mktval(i)] + 0.0000793*profits(i) + 0.0114646*ceoten(i) + e(i)
#
#  equation 3:
#  ln[salary(i)] = 4.558 + 1.018e-01*ln[mktval(i)] + 2.905e-05*profits(i) + 1.168e-02*ceoten(i) + 1.622e-01*ln[sales(i)] + e(i)

# j.  We used natural logs on all the dollar-valued quantities except profits in models 2 & 3 (eq. 2 & 3). Why
#     did we not take the log of profits?
#     --> We did not take the log of profits as it has some observed values as negative. Log of negative variables 
#     does not exist.

# k.  Interpret the estimated coefficient on log mktval in model 2 (eq 2).
#     --> Every 1% increase in the market value is associated with 23.8622% increase in compensation to the CEO, controlling
#     for profits and CEO tenure. 


# l.  Interpret the estimated coefficient on log mktval in model 3 (eq 3).
#     --> Every 1% increase in the market value is associated with 10.18% increase in compensation to the CEO, controlling
#     for profits, CEO tenure and sales of the firm.

# m.  Compare the test statistics on log mktval between model 2 and model 3. Please explain the differences
#     you find in terms of the biases we discussed in class.
#     --> t value for log mktval from model 2 is: 4.261
#         t value for log mktval from model 3 is: 1.614
#     The lower t-value for log mktval from model 3, which includes the variable of log sales can be explained
#     by presence of omitted variable bias in model 2 which has higher t value for log mktval.
#     |t| value less than 1.96 for log mktval in model 3 which includes log sales, indicates the log sales is the real driver 
#     of the dependent variable which was omitted in the model 2 and thus t value was higher for log mktval.

# n.  Is the coefficient on profits significant in model 3 (eq 3)?
#     --> No. 

# o.  Interpret the estimated coefficient on log sales in model 3 (eq 3).
#     --> Every 1% increase in sales of the firm is associated with 16.22% increase in compensation to the CEO, controlling
#     for market value, profits and CEO tenure.


############################# QUESTION 3 ###############################

#data import and validation
context3 <- fread('hprice1.csv')
summary(context3)
# model 4 - equation 4:
# price(i) = B0 + B1*bdrms(i) + B2*ln[lotsize(i)] + B3*ln[sqrft(i)] + B4*colonial(i) + e(i)
model4 <- lm(price~bdrms+log(lotsize)+log(sqrft)+colonial, data= context3)
summary(model4)
# 
# Call:
#   lm(formula = price ~ bdrms + log(lotsize) + log(sqrft) + colonial, 
#      data = context3)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -109.603  -38.258   -4.325   22.984  220.766 
# 
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
#    (Intercept)  -2030.455    210.967  -9.625 3.68e-15 ***
#   bdrms           18.572      9.308   1.995   0.0493 *  
#   log(lotsize)    61.446     12.372   4.966 3.60e-06 ***
#   log(sqrft)     225.508     30.072   7.499 6.41e-11 ***
#   colonial         4.134     14.509   0.285   0.7764    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 59.66 on 83 degrees of freedom
# Multiple R-squared:  0.6781,	Adjusted R-squared:  0.6626 
# F-statistic: 43.71 on 4 and 83 DF,  p-value: < 2.2e-16


# model 5 - equation 5:
# ln[price(i)] = B0 + B1*bdrms(i) + B2*ln[lotsize(i)] + B3*ln[sqrft(i)] + B4*colonial(i) + e(i)
model5 <- lm(log(price)~bdrms+log(lotsize)+log(sqrft)+colonial, data= context3)
summary(model5)
# 
# Call:
#   lm(formula = log(price) ~ bdrms + log(lotsize) + log(sqrft) + 
#        colonial, data = context3)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.69479 -0.09750 -0.01619  0.09151  0.70228 
# 
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -1.34959    0.65104  -2.073   0.0413 *  
# bdrms         0.02683    0.02872   0.934   0.3530    
# log(lotsize)  0.16782    0.03818   4.395 3.25e-05 ***
# log(sqrft)    0.70719    0.09280   7.620 3.69e-11 ***
# colonial      0.05380    0.04477   1.202   0.2330    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.1841 on 83 degrees of freedom
# Multiple R-squared:  0.6491,	Adjusted R-squared:  0.6322 
# F-statistic: 38.38 on 4 and 83 DF,  p-value: < 2.2e-16

# Interpretations
# equation 4:
# price(i) = -2030.455 + 18.572*bdrms(i) + 61.446*ln[lotsize(i)] + 225.508*ln[sqrft(i)] + 4.134*colonial(i) + e(i)
# equation 5:
# ln[price(i)] = -1.34959 + 0.02683*bdrms(i) + 0.16782*ln[lotsize(i)] + 0.70719*ln[sqrft(i)] + 0.05380*colonial(i) + e(i)

# p.  Interpret the estimated coefficient on log lotsize from model4 (eq 4).
#     --> Every 100% increase in lotsize is associated with 61.466 $1000s increase in house price, controlling for
#     no. of bed rooms, size of house and the style.

# q.  Interpret the estimated coefficient on log lotsize from model5 (eq 5).
#     --> Every 100% increase in lotsize is associated with 16.782% increase in house price, controlling for
#     no. of bed rooms, size of house and the style.

# r.  Interpret the estimated coefficient on colonial from model4 (eq 4, please ignore significance here).
#     --> The colonial style of the house is asscoiated with an increase of $4134 in house price, controlling for
#     no. of bedrooms, size of lot and size of house.

# s.  Which model (4 or 5) better fits the data for this data set? On what criterion/criteria are you basing
#     your judgement?
#     --> Model 5 fits better for this data set. Adjusted R-square statistic is comparable (though higher for model 5) for the models.
#     However, RSE for model 4 is very high which indiactes lack of fit of the model. RSE for model 5
#     is closer to 0 which makes the model better fit for this data set.
#     RSE cant be used coz units for both the models are different. 
#     R-square can be used where no. of variables are same in both the models.

# t.  Suppose your house is worth $300k. You are considering an expansion of your home to add a master suite
#     (+1 bedroom to your home). This expansion would increase your square-footage by 10% and would cost
#     $50k. You have valued your enjoyment of the additional space at $20k, so you would only be willing to
#     consider the build if it were to also increase your property value accordingly. Does the appropriate model
#     indicate that you should pursue the expansion?
#     --> using model 5,
#     Percentage change in price = 0.02683*1*100 + 0.70719*0.1*100 = 2.2683 + 7.0719 = 9.3402
#     Price of Hpuse after expansion= $300k + (9.3402 % of $300k) = $300k +$28.020 = $328.02k
#     Breakeven point for considering expansion= $300k + $50k -$20k =$330k
#     As the predicted property value is less than the breakeven price, I should not pursue the expansion.



############################# QUESTION 4 ###############################


#data import and validation
context4 <- fread('JTRAIN2.csv')
summary(context4)

# model 6 - equation 6:
# re78(i) = B0 + B1*re75(i) + B2*train(i) + B3*educ(i) + B4*black(i) + e(i)
model6 <- lm(re78~re75+train+educ+black, data= context4)
summary(model6)
# Call:
#   lm(formula = re78 ~ re75 + train + educ + black, data = context4)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -9.120 -4.377 -1.756  3.353 54.058 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)  1.97686    1.89028   1.046   0.2962   
# re75         0.14697    0.09811   1.498   0.1349   
# train        1.68422    0.62700   2.686   0.0075 **
# educ         0.41026    0.17267   2.376   0.0179 * 
# black       -2.11277    0.82941  -2.547   0.0112 * 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 6.496 on 440 degrees of freedom
# Multiple R-squared:  0.04917,	Adjusted R-squared:  0.04053 
# F-statistic: 5.688 on 4 and 440 DF,  p-value: 0.00018


# Interpretations
# Equation 6:
# re78(i) = 1.97686 + 0.14697*re75(i) + 1.68422*train(i) + 0.41026*educ(i) - 2.11277*black(i) + e(i)

# u.  Interpret the estimated coefficient on re75 from model6 (eq 6).
#     --> Every $1000 increase in real earns,1975 is associated with $146.97 increase in real earns, 1978.

# v.  Interpret the estimated coefficient on train from model6 (eq 6). Is the coefficient significant?
#     --> Being assigned job training is associated with $1684.22 increase in real earns, 1978.

# w.  Interpret the estimated coefficient on black from model6 (eq 6).
#     --> Being black is associated with $2112.77 decrease in real earns, 1978.

