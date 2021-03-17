setwd("C:/Users/Anzony/Documents/GitHub/14.38_Causal_ML/Jupyter_Notebooks")

Penn <- as.data.frame(read.table("../data/penn_jae.dat", header=T ))
n <- dim(Penn)[1]
p_1 <- dim(Penn)[2]
Penn<- subset(Penn, tg==4 | tg==0)
attach(Penn)

T4<- (tg==4)
summary(T4)



m <- lm(T4~(female+black+othrace+factor(dep)+q2+q3+q4+q5+q6+agelt35+agegt54+durable+lusd+husd)^2)
library(lmtest)
library(sandwich)
coeftest(m, vcov = vcovHC(m, type="HC1"))


# model specifications


# no adjustment (2-sample approach)
formula_cl <- log(inuidur1)~T4

# adding controls
formula_cra <- log(inuidur1)~T4+ (female+black+othrace+factor(dep)+q2+q3+q4+q5+q6+agelt35+agegt54+durable+lusd+husd)^2
# Omitted dummies: q1, nondurable, muld


ols.cl <- lm(formula_cl)
ols.cra <- lm(formula_cra)


ols.cl = coeftest(ols.cl, vcov = vcovHC(ols.cl, type="HC1"))
ols.cra = coeftest(ols.cra, vcov = vcovHC(ols.cra, type="HC1"))

print(ols.cl)
print(ols.cra)




#interactive regression model;

X = model.matrix (~ (female+black+othrace+factor(dep)+q2+q3+q4+q5+q6+agelt35+agegt54+durable+lusd+husd)^2)[,-1]
dim(X)
demean<- function(x){ x - mean(x)}
X = apply(X, 2, demean)

ols.ira = lm(log(inuidur1) ~ T4*X) 
ols.ira= coeftest(ols.ira, vcov = vcovHC(ols.ira, type="HC1"))
print(ols.ira)

X = model.matrix (~ (female+black+othrace+factor(dep)+q2+q3+q4+q5+q6+agelt35+agegt54+durable+lusd+husd)^2)


demean<- function(x){ x - mean(x)}
X = apply(X, 2, demean)
X = model.matrix (~ (female+black+othrace+factor(dep)+q2+q3+q4+q5+q6+agelt35+agegt54+durable+lusd+husd)^2)[,-1]
dim(X)
demean<- function(x){ x - mean(x)}
X = apply(X, 2, demean)


z = model.matrix ( ~ T4*X )
dim(z)

ols.ira = lm(log(inuidur1) ~ T4*X) 
= coeftest(ols.ira, vcov = vcovHC(ols.ira, type="HC1"))


dim(ols.ira)
T4 = demean(T4)

DX = model.matrix(~T4*X)[,-1]

T4*X
dim(DX)