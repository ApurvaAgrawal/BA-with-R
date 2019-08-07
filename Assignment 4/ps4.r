###############################################################
# Title:        ps4.r
# Author:       Apurva Agrawal
# Date:         2017-11-21
# Description:  Turn-in product for problem set 4
###############################################################

#clear environment
rm(list=ls(all=TRUE))

#import packages
library(data.table)
library(sandwich)
library(lmtest)
library(ggplot2)
library(mfx)
library(pscl)
library(party)
library(evtree)

#######################################   QUESTION 1  #########################################

## Data import and validaticon
context1    <- fread('htv.csv')
summary(context1)

#                storage   display    value
# variable name   type    format     label      variable label
# ---------------------------------------------------------------------------------------------------
#   wage            float   %9.0g                 hourly wage, 1991
# abil            float   %9.0g                 abil. measure, not standardized
# educ            byte    %9.0g                 highest grade completed by 1991
# exper           byte    %9.0g                 potential experience

model1 <- lm(log(wage)~abil+educ+exper,data=context1)
summary(model1) 
c(AIC(model1),BIC(model1))
#[1] 1935.995 1961.569

context1$abilsq <- context1$abil^2
context1$educsq <- context1$educ^2
context1$expersq <- context1$exper^2
context1$abileduc <- context1$abil*context1$educ
context1$educexper <- context1$educ*context1$exper
context1$abilexper <- context1$abil*context1$exper
# 
# 
# model2 <- lm(log(wage)~abil+educ+exper+abilsq+educsq+expersq+abileduc+abilexper+educexper,data=context1)
# summary(model2)
# c(AIC(model2),BIC(model2))
#model2 <-lm(log(wage))~abil+educ+exper+I(educ^2)+I(abil^2)+I(exper^2), data=context1)
# model3 <- lm(log(wage)~abil+educ+exper+abilsq,data=context1)
# model4 <- lm(log(wage)~abil+educ+exper+educsq,data=context1)
# model5 <- lm(log(wage)~abil+educ+exper+expersq,data=context1) #low bic
# c(AIC(model3),AIC(model4),AIC(model5))
# c(BIC(model3),BIC(model4),BIC(model5))
# model6 <- lm(log(wage)~abil+educ+exper+abilsq+educsq+expersq,data=context1)
# c(AIC(model6),BIC(model6))
# mdel2 <- step(model2, direction="backward",k=log(nrow(context1)))
model2 <- lm(log(wage)~exper+abileduc+educexper,data=context1)
summary(model2)
c(AIC(model2),BIC(model2))
#[1] 1926.457 1952.031
# 
# Interpretations:
# a. Once model2 has been reduced to the lowest BIC version, what is the difference between models 1 and 2?
#    -->  The BIC of model 2 (1952) is lower than BIC of moedl 1 (1961)
#
# b. What is the variable (educi x experi) doing to the model? We call this variable an interaction variable.
#    --> It is making the model better in terms of understanding the relationship among the variable educ and exper
#        and their effect on wage(dependent variable)



#######################################   QUESTION 2  #########################################

## Data import and validaticon
context2    <- fread('loanapp.csv')
summary(context2)
context2$whiteobrat <- context2$white*context2$obrat
# variable name   type    format     label      variable label
# ---------------------------------------------------------------------------------------------------
#   occ             byte    %9.0g                 occupancy
# loanamt         int     %9.0g                 loan amt in thousands
# action          byte    %9.0g                 type of action taken
# msa             int     %9.0g                 msa number of property
# suffolk         byte    %9.0g                 =1 if property in suffolk co.
# appinc          int     %9.0g                 applicant income, $1000s
# typur           byte    %9.0g                 type of purchaser of loan
# unit            byte    %9.0g                 number of units in property
# married         byte    %9.0g                 =1 if applicant married
# dep             byte    %9.0g                 number of dependents
# emp             byte    %9.0g                 years employed in line of work
# yjob            byte    %9.0g                 years at this job
# self            byte    %9.0g                 =1 if self employed
# atotinc         float   %9.0g                 total monthly income
# cototinc        float   %9.0g                 coapp total monthly income
# hexp            float   %9.0g                 propose housing expense
# price           float   %9.0g                 purchase price
# other           float   %9.0g                 other financing, $1000s
# liq             float   %9.0g                 liquid assets
# rep             byte    %9.0g                 no. of credit reports
# gdlin           int     %9.0g                 credit history meets guidelines
# lines           float   %9.0g                 no. of credit lines on reports
# mortg           byte    %9.0g                 credit history on mortgage paym
# cons            byte    %9.0g                 credit history on consumer stuf
# pubrec          byte    %9.0g                 =1 if filed bankruptcy
# hrat            float   %9.0g                 housing exp, % total inc
# obrat           float   %9.0g                 other oblgs, % total inc
# fixadj          byte    %9.0g                 fixed or adjustable rate?
# term            float   %9.0g                 term of loan in months
# apr             float   %9.0g                 appraised value
# prop            byte    %9.0g                 type of property
# inss            byte    %9.0g                 PMI sought
# inson           byte    %9.0g                 PMI approved
# gift            byte    %9.0g                 gift as down payment
# cosign          byte    %9.0g                 is there a cosigner
# unver           byte    %9.0g                 unverifiable info
# review          int     %9.0g                 number of times reviewed
# netw            float   %9.0g                 net worth
# unem            float   %9.0g                 unemployment rate by industry
# min30           byte    %9.0g                 =1 if minority pop. > 30%
#   bd              byte    %9.0g                 =1 if boarded-up val > MSA med
# mi              byte    %9.0g                 =1 if tract inc > MSA median
# old             byte    %9.0g                 =1 if applic age > MSA median
# vr              byte    %9.0g                 =1 if tract vac rte > MSA med
# sch             byte    %9.0g                 =1 if > 12 years schooling
# black           byte    %9.0g                 =1 if applicant black
# hispan          byte    %9.0g                 =1 if applicant Hispanic
# male            byte    %9.0g                 =1 if applicant male
# reject          byte    %9.0g                 =1 if action == 3
# approve         byte    %9.0g                 =1 if action == 1 or 2
# mortno          byte    %9.0g                 no mortgage history
# mortperf        byte    %9.0g                 no late mort. payments
# mortlat1        byte    %9.0g                 one or two late payments
# mortlat2        byte    %9.0g                 > 2 late payments
# chist           byte    %9.0g                 =0 if accnts deliq. >= 60 days
# multi           byte    %9.0g                 =1 if two or more units
# loanprc         float   %9.0g                 amt/price
# thick           byte    %9.0g                 =1 if rep > 2
# white           byte    %9.0g                 =1 if applicant white
# ------------------------------------------------------------------------------------------------

model3 <- glm(approve~white,family=binomial(link="logit"),data=context2)
coeftest(model3,vcov.=vcovHC)
# z test of coefficients:
#   
#   Estimate Std. Error z value  Pr(>|z|)    
# (Intercept)  0.88469    0.12570   7.038  1.95e-12 ***
#   white        1.40942    0.15152   9.302 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


model4 <- glm(approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr,family=binomial(link="logit"),data=context2)
coeftest(model4,vcov.=vcovHC)
# 
# z test of coefficients:
#   
#                 Estimate Std. Error z value  Pr(>|z|)    
#   (Intercept)  3.801710   0.653953  5.8134 6.120e-09 ***
#   white        0.937764   0.177764  5.2753 1.325e-07 ***
#   hrat         0.013263   0.013924  0.9525 0.3408381    
#   obrat       -0.053034   0.012809 -4.1403 3.469e-05 ***
#   loanprc     -1.904951   0.535160 -3.5596 0.0003714 ***
#   unem        -0.066579   0.036124 -1.8430 0.0653225 .  
#   male        -0.066385   0.210174 -0.3159 0.7521100    
#   married      0.503282   0.186857  2.6934 0.0070728 ** 
#   dep         -0.090734   0.075412 -1.2032 0.2289086    
#   sch          0.041229   0.179024  0.2303 0.8178605    
#   cosign       0.132059   0.406794  0.3246 0.7454585    
#   chist        1.066577   0.173265  6.1558 7.472e-10 ***
#   pubrec      -1.340665   0.233076 -5.7520 8.817e-09 ***
#   mortlat1    -0.309882   0.545600 -0.5680 0.5700580    
#   mortlat2    -0.894675   0.608995 -1.4691 0.1418053    
#   vr          -0.349828   0.156653 -2.2331 0.0255398 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

model5 <- glm(approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr+whiteobrat,family=binomial(link="logit"),data=context2)
coeftest(model5,vcov.=vcovHC)
# 
# z test of coefficients:
# 
#                Estimate Std. Error z value  Pr(>|z|)    
#   (Intercept)  4.306527   0.901355  4.7778 1.772e-06 ***
#   white        0.296882   0.858772  0.3457 0.7295644    
#   hrat         0.013405   0.014158  0.9468 0.3437269    
#   obrat       -0.066604   0.020728 -3.2133 0.0013121 ** 
#   loanprc     -1.909701   0.533546 -3.5793 0.0003446 ***
#   unem        -0.067549   0.035988 -1.8770 0.0605206 .  
#   male        -0.071904   0.210842 -0.3410 0.7330802    
#   married      0.503536   0.187212  2.6897 0.0071526 ** 
#   dep         -0.095772   0.076030 -1.2597 0.2077873    
#   sch          0.034893   0.180807  0.1930 0.8469704    
#   cosign       0.152567   0.413003  0.3694 0.7118226    
#   chist        1.061385   0.174154  6.0945 1.098e-09 ***
#   pubrec      -1.344267   0.235075 -5.7185 1.075e-08 ***
#   mortlat1    -0.333314   0.540401 -0.6168 0.5373739    
#   mortlat2    -0.920857   0.610013 -1.5096 0.1311534    
#   vr          -0.350862   0.157302 -2.2305 0.0257141 *  
#   whiteobrat   0.018149   0.023603  0.7689 0.4419416    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Interpretations:
# a. In model3, we can't directly interpret the coe¢ cient on white like we did in the linear model because the
#    logit link function g() confuses things a bit. That said, the sign and significance of B1 still have the same
#    effect. What is this coefficient indicating roughly?
#   -->  the coefficicient indicates a positive correlation between the white and the aprrove variable.
#
# b. After adding 14 more variables in model4, how does B1 change? Is it still significant?
#   --> yes, it is still significant at 0.1% level.
#
# c. After adding the interaction between white and obrat in model5, how has B1 changed now?
#    --> It is not significant even at 10% level.
#
# d. What is (white  obrat) ; and why do you think it has affected the model so greatly?
#    --> white*obrat is the interaction variable. It has affected the model so greatly because 
#        it incorporated the relationship between the white and obrat variables which made the model
#        more accurate for analysis of dependent variable.


#######################################   QUESTION 3  #########################################

## Data import and validaticon
context3    <- fread('smoke.csv')

summary(context3)
context3$agesq <- (context3$age)^2
context3$lnincome <- log(context3$income)

model6 <- glm(cigs~educ+age+agesq+lnincome+restaurn,family=poisson(link="log"),data=context3)
#summary(model6)
coeftest(model6,vcov.=vcovHC)
# z test of coefficients:
#   
#   Estimate Std. Error z value  Pr(>|z|)    
#   (Intercept) -0.0895322  0.7819305 -0.1145  0.908840    
#   educ        -0.0595212  0.0194111 -3.0663  0.002167 ** 
#   age          0.1139858  0.0215662  5.2854 1.254e-07 ***
#   agesq       -0.0013679  0.0002485 -5.5045 3.701e-08 ***
#   lnincome     0.1047168  0.0840807  1.2454  0.212973    
#   restaurn    -0.3613089  0.1386491 -2.6059  0.009163 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# Interpretations:
# a. Interpret the coefficient on educ in model6 (hint: the link function for Poisson is g (y) = ln [y])?
#    --> Increase in 1 year of schooling is associated with a 6% decrease in number of cigs smoked per day,
#       controlling for age, annual income and restaurant smoking restrictions. 
#
# b. Find the rate of change for ln [cigs] with respect to age. Find this number when a person is 20 and when
# a person is 60.
#   --> d(ln[cigs])/d(age) = 1*0.1139 + 2*(age)*(-0.0013)
#       at 20:  0.0619
#       at 60: -0.0421
#        intensive margin: when ppl are 20, everyone somkes 6% more
#         extensive margin: when ppl are 20, 6% more ppl start smoking
#         or 6% can be a combination of above 2 


#######################################   QUESTION 4  #########################################

## Data import and validaticon
context4   <- fread('hdisease.csv')
summary(context4)
context4$exang		<-	ifelse(context4$exang=="yes",1,0)
f <- hdisease ~ age + cp + trestbps + thalach + exang
# 
# Variable name				Description
# dset						Data set
# age							Age (years)
# sex							Sex
# cp							Chest pain scale (1-4)
# trestbps					Resting blood pressure (mm Hg on admission to hospital)
# chol						Cholesterol (mg/dl)
# restecg						Resting ECG results
# thalach						Maximum heart rate (beats per min)
# exang						Exercise-induced angina
# hdisease					Heart Disease (stage 0-4)

model7 <- evtree(f, data=context4)
plot(model7)

model8 <- ctree(f, data=context4)
plot(model8)

context5 <- fread('hdisease-new.csv')
context5$hdisease_pred <- predict(model8,context5)

# Interpretations:
# a. Comparing model7 to model8, which model might be overfitting the data? Which model might be
#    underfitting the data?
#    --> model7 is underfitting the data and model8 is overfitting the data
#
# b. Why don't we include dset in these models?
#    --> dset is a categorical variable, which needs to be converted into numeric one or the one having
#        multiple levels but using the numeric values won't give the results as expected.