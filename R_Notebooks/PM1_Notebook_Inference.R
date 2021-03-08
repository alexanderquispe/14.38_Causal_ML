install.packages("xtable")
install.packages("sandwich")
install.packages("hdm")

setwd( "C:/Users/Anzony/Documents/GitHub/14.38_Causal_ML")

load( "data/wage2015_subsample_inference.Rdata")
attach(data)
==
dim(data)

library(xtable)

Z <- data[which(colnames(data) %in% c("lwage","sex","shs","hsg","scl","clg","ad","ne","mw","so","we","exp1"))]

data_female <- data[data$sex==1,]
Z_female <- data_female[which(colnames(data) %in% c("lwage","sex","shs","hsg","scl","clg","ad","ne","mw","so","we","exp1"))]


data_male <- data[data$sex==0,]
Z_male <- data_male[which(colnames(data) %in% c("lwage","sex","shs","hsg","scl","clg","ad","ne","mw","so","we","exp1"))]

table <- matrix(0, 12, 3)
table[1:12,1]   <- as.numeric(lapply(Z,mean))
table[1:12,2]   <- as.numeric(lapply(Z_male,mean))
table[1:12,3]   <- as.numeric(lapply(Z_female,mean))
rownames(table) <- c("Log Wage","Sex","Less then High School","High School Graduate","Some College","Gollage Graduate","Advanced Degree", "Northeast","Midwest","South","West","Experience")
colnames(table) <- c("All","Men","Women")
tab<- xtable(table, digits = 4)
tab

print(tab,type="html") # set type="latex" for printing table in LaTeX

mean(data_female$lwage)-mean(data_male$lwage)

library(sandwich)
nocontrol.fit <- lm(lwage ~ sex)
nocontrol.est <- summary(nocontrol.fit)$coef["sex",1]
HCV.coefs <- vcovHC(nocontrol.fit, type = 'HC');
nocontrol.se <- sqrt(diag(HCV.coefs))[2] # Estimated std errors

# print unconditional effect of gender and the corresponding standard error
cat ("The estimated gender coefficient is",nocontrol.est," and the corresponding robust standard error is",nocontrol.se)

# Ols regression with controls

flex <- lwage ~ sex + (exp1+exp2+exp3+exp4)*(shs+hsg+scl+clg+occ2+ind2+mw+so+we)

#   Note that ()*() operation in formula objects in R creates a formula of the sort:
#  (exp1+exp2+exp3+exp4)+ (shs+hsg+scl+clg+occ2+ind2+mw+so+we) +  (exp1+exp2+exp3+exp4)*(shs+hsg+scl+clg+occ2+ind2+mw+so+we)
#  This is not intuitive at all, but that's what it does.

control.fit <- lm(flex, data=data)
control.est <- summary(control.fit)$coef[2,1]

summary(control.fit)

cat("Coefficient for OLS with controls", control.est)

HCV.coefs <- vcovHC(control.fit, type = 'HC');
control.se <- sqrt(diag(HCV.coefs))[2] # Estimated std errors

# Partialling-Out using ols

# models
flex.y <- lwage ~  (exp1+exp2+exp3+exp4)*(shs+hsg+scl+clg+occ2+ind2+mw+so+we) # model for Y
flex.d <- sex ~ (exp1+exp2+exp3+exp4)*(shs+hsg+scl+clg+occ2+ind2+mw+so+we) # model for D

# partialling-out the linear effect of W from Y
t.Y <- lm(flex.y, data=data)$res
# partialling-out the linear effect of W from D
t.D <- lm(flex.d, data=data)$res

# regression of Y on D after partialling-out the effect of W
partial.fit <- lm(t.Y~t.D)
partial.est <- summary(partial.fit)$coef[2,1]

cat("Coefficient for D via partialling-out", partial.est)

# standard error
HCV.coefs <- vcovHC(partial.fit, type = 'HC')
partial.se <- sqrt(diag(HCV.coefs))[2]

# confidence interval
confint(partial.fit)[2,]


library(hdm)

# models
flex.y <- lwage ~  (exp1+exp2+exp3+exp4)*(shs+hsg+scl+clg+occ2+ind2+mw+so+we) # model for Y
flex.d <- sex ~ (exp1+exp2+exp3+exp4)*(shs+hsg+scl+clg+occ2+ind2+mw+so+we) # model for D

# partialling-out the linear effect of W from Y
t.Y <- rlasso(flex.y, data=data)$res
# partialling-out the linear effect of W from D
t.D <- rlasso(flex.d, data=data)$res

# regression of Y on D after partialling-out the effect of W
partial.lasso.fit <- lm(t.Y~t.D)
partial.lasso.est <- summary(partial.lasso.fit)$coef[2,1]

cat("Coefficient for D via partialling-out using lasso", partial.lasso.est)

# standard error
HCV.coefs <- vcovHC(partial.lasso.fit, type = 'HC')
partial.lasso.se <- sqrt(diag(HCV.coefs))[2]

table<- matrix(0, 4, 2)
table[1,1]<- nocontrol.est  
table[1,2]<- nocontrol.se   
table[2,1]<- control.est
table[2,2]<- control.se    
table[3,1]<- partial.est  
table[3,2]<- partial.se  
table[4,1]<-  partial.lasso.est
table[4,2]<- partial.lasso.se 
colnames(table)<- c("Estimate","Std. Error")
rownames(table)<- c("Without controls", "full reg", "partial reg", "partial reg via lasso")	
tab<- xtable(table, digits=c(3, 3, 4))
tab

print(tab, type="html")

extraflex <- lwage ~ sex + (exp1+exp2+exp3+exp4+shs+hsg+scl+clg+occ2+ind2+mw+so+we)^2

control.fit <- lm(extraflex, data=data)
#summary(control.fit)
control.est <- summary(control.fit)$coef[2,1]


cat("Number of Extra-Flex Controls", length(control.fit$coef)-1, "\n")


cat("Coefficient for OLS with extra flex controls", control.est)


#summary(control.fit)



HCV.coefs <- vcovHC(control.fit, type = 'HC');

n= length(wage); p =length(control.fit$coef);

control.se <- sqrt(diag(HCV.coefs))[2]*sqrt(n/(n-p)) # Estimated std errors

# crude adjustment for the effect of dimensionality on OLS standard errors, motivated by Cattaneo, Jannson, and Newey (2018)

# for really correct way of doing this, we need to implement Cattaneo, Jannson, and Newey (2018)'s procedure.


library(hdm)

# models
extraflex.y <- lwage ~  (exp1+exp2+exp3+exp4+shs+hsg+scl+clg+occ2+ind2+mw+so+we)^2 # model for Y
extraflex.d <- sex ~ (exp1+exp2+exp3+exp4+shs+hsg+scl+clg+occ2+ind2+mw+so+we)^2 # model for D

# partialling-out the linear effect of W from Y
t.Y <- rlasso(extraflex.y, data=data)$res
# partialling-out the linear effect of W from D
t.D <- rlasso(extraflex.d, data=data)$res

# regression of Y on D after partialling-out the effect of W
partial.lasso.fit <- lm(t.Y~t.D)
partial.lasso.est <- summary(partial.lasso.fit)$coef[2,1]

cat("Coefficient for D via partialling-out using lasso", partial.lasso.est)

# standard error
HCV.coefs <- vcovHC(partial.lasso.fit, type = 'HC')
partial.lasso.se <- sqrt(diag(HCV.coefs))[2]

table<- matrix(0, 2, 2)
table[1,1]<- control.est
table[1,2]<- control.se    
table[2,1]<-  partial.lasso.est
table[2,2]<- partial.lasso.se 
colnames(table)<- c("Estimate","Std. Error")
rownames(table)<- c("full reg","partial reg via lasso")	
tab<- xtable(table, digits=c(3, 3, 4))
tab

print(tab, type="latex")


