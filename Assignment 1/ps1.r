###############################################################
# Title:        ps1.r
# Author:       Apurva Agrawal
# Date:         2017-09-11
# Description:  Turn-in product for problem set 1
###############################################################


#Clear Environment
rm(list=ls(all=TRUE))

#Import Package
library(data.table)

#Import data
context1 <- fread("WAGE1.csv")

#############################
# Data description from WAGE1_labels.txt
#---------------------------------------------------------------------------------------------------
# storage   display    value
# variable name   type    format     label      variable label
#---------------------------------------------------------------------------------------------------
#  wage            float   %8.2g                 average hourly earnings
#  educ            byte    %8.0g                 years of education
#  exper           byte    %8.0g                 years potential experience
#  tenure          byte    %8.0g                 years with current employer
#  nonwhite        byte    %8.0g                 =1 if nonwhite
#  female          byte    %8.0g                 =1 if female
#  married         byte    %8.0g                 =1 if married
#  numdep          byte    %8.0g                 number of dependents
#  smsa            byte    %8.0g                 =1 if live in SMSA
#  northcen        byte    %8.0g                 =1 if live in north central U.S
#  south           byte    %8.0g                 =1 if live in southern region
#  west            byte    %8.0g                 =1 if live in western region
#  construc        byte    %8.0g                 =1 if work in construc. indus.
#  ndurman         byte    %8.0g                 =1 if in nondur. manuf. indus.
#  trcommpu        byte    %8.0g                 =1 if in trans, commun, pub ut
#  trade           byte    %8.0g                 =1 if in wholesale or retail
#  services        byte    %8.0g                 =1 if in services indus.
#  profserv        byte    %8.0g                 =1 if in prof. serv. indus.
#  profocc         byte    %8.0g                 =1 if in profess. occupation
#  clerocc         byte    %8.0g                 =1 if in clerical occupation
#  servocc         byte    %8.0g                 =1 if in service occupation
#---------------------------------------------------------------------------------------------------
##########################################

#Summary Statistics
summary(context1)

# Plot for observation purpose only
plot(context1$educ,context1$wage)

#New variable as log(wage)
lwage=log(context1$wage)

# eq 1
# wage(i)=B0 + B1*educ(i) +e(i)
model1 <- lm(wage~educ,data=context1)

# eq 2
# wage(i)=B0 + B1*educ(i) +B2*exper(i) +B3*tenure(i) +e(i)
model2 <- lm(wage~educ+exper+tenure,data=context1)

# eq 3
# lwage(i)=B0 + B1*educ(i) +B2*exper(i) +B3*tenure(i) +e(i)
model3 <- lm(lwage~educ+exper+tenure,data=context1)

# SUMMARY OF MODELS

summary(model1)
#Call:
#lm(formula = wage ~ educ, data = context1)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#
#Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
#  (Intercept) -0.93389    0.68769  -1.358    0.175    
#  educ         0.54470    0.05346  10.189   <2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 3.392 on 524 degrees of freedom
#Multiple R-squared:  0.1654,	Adjusted R-squared:  0.1638 
#F-statistic: 103.8 on 1 and 524 DF,  p-value: < 2.2e-16

summary(model2)
#Call:
#  lm(formula = wage ~ educ + exper + tenure, data = context1)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-7.6498 -1.7708 -0.6407  1.2051 14.7201 
#
#Coefficients:
#      Estimate Std. Error t value Pr(>|t|)    
#  (Intercept) -2.91354    0.73172  -3.982 7.81e-05 ***
#  educ         0.60268    0.05148  11.708  < 2e-16 ***
#  exper        0.02252    0.01210   1.861   0.0633 .  
#  tenure       0.17002    0.02173   7.825 2.83e-14 ***
# ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 3.096 on 522 degrees of freedom
#Multiple R-squared:  0.3072,	Adjusted R-squared:  0.3032 
#F-statistic: 77.15 on 3 and 522 DF,  p-value: < 2.2e-16

summary(model3)
#Call:
#  lm(formula = lwage ~ educ + exper + tenure, data = context1)
#
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-2.05911 -0.29563 -0.03302  0.28590  1.42657 
#
#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#  (Intercept) 0.282635   0.104331   2.709  0.00697 ** 
#  educ        0.092256   0.007340  12.569  < 2e-16 ***
#  exper       0.004137   0.001726   2.397  0.01687 *  
#  tenure      0.022112   0.003098   7.138 3.19e-12 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 0.4415 on 522 degrees of freedom
#Multiple R-squared:  0.3165,	Adjusted R-squared:  0.3125 
#F-statistic: 80.56 on 3 and 522 DF,  p-value: < 2.2e-16


# INTERPRETATIONS:

# =>  Putting the coeeficients from summary of model1 into eq 1, we get:
#        wage(i)=  -0.93389 + 0.54470 *educ(i) +e(i)
# =>  Putting the coeeficients from summary of model2 into eq 2, we get:
#        wage(i)=  -2.91354 + 0.60268 *educ(i) + 0.02252*exper(i) + 0.17002*tenure(i) +e(i)
# =>  Putting the coeeficients from summary of model3 into eq 3, we get:
#        lwage(i)=  0.282635 + 0.092256 *educ(i) + 0.004137*exper(i) + 0.022112*tenure(i) +e(i)
#        For interpretation, getting coefficients from model3 into percentage form
         coef(model3)*100 
#                  (Intercept)        educ       exper      tenure 
#                  28.2634948   9.2256203   0.4136804   2.2111673    ---> (%)


# a. Interpret the estimated coefficient on educ from model1 (eq 1).
#
#     An increase of 2 years in education is associated with an increase in average hourly
#     wage of the worker by 1 unit.
#
# b. Interpret the estimated coefficient on educ from model2 (eq 2).
#     
#     An increase of 2 years in education is associated with an icrease in average hourly
#     wage of worker by 1.2 units, controlling for years of potenitial experience and
#     years with current employer.
#
# c. Interpret the estimated coefficient on exper from model2 (eq 2).
# 
#     An increase of 5 years in potential experience is associated with an increase 
#     in average hourly wage of the worker by 0.1 unit, controlling for years of education 
#     and years with current employer.
#
# d. Interpret the estimated coefficient on tenure from model2 (eq 2).
#
#     An increase of 5 years under the current employer are associated with an increase
#     in average hourly wage of the worker by 0.85 units, controlling for years of education
#     and years of potential experience.
#
# e. Interpret the estimated intercept from model2 (eq 2).
#
#    With no education, no potential experience and no experience with the current employer,
#    the model predicts the average hourly wage of the worker to be 2.9 units.
#
# f. Interpret the estimated coefficient on educ from model3 (eq 3).
#
#     An increase of 2 years in education is associated with a 18% increase in the average
#     hourly wage of the worker, controlling for years of potential experience and years with
#     current employer.
#
# g. Interpret the estimated coefficient on exper from model3 (eq 3).
#
#    An increase of 5 years in potential experience is associated with a 2% increase in
#    average hourly wage of the worker, controlling for years of education and years with 
#    the current employer.
#
# h. Interpret the estimated coefficient on tenure from model3 (eq 3).
#
#    An increase of 5 years with the current employer is associated with a 10% increase in 
#    avearage hourly wage of the worker, controlling for years of education and years of 
#    potential experience.
