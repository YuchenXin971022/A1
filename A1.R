# Exercise 1

# No. of students
library(tidyverse)
library(stringr)
data1 <- read.csv("datstu.csv",header = TRUE)
View(data1)

attach(data1)
students = sum(!is.na(X))
students

# No. of schools
school1 <- unique(data1$schoolcode1)
school2 <- unique(data1$schoolcode2)
school3 <- unique(data1$schoolcode3)
school4 <- unique(data1$schoolcode4)
school5 <- unique(data1$schoolcode5)
school6 <- unique(data1$schoolcode6)
school7 <- unique(na.omit(c(school1, school2, school3, school4, school5, school6)))
schools <- school7[school7 != ""]
length(schools)

# No. of programs
program1 <- unique(data1$choicepgm1)
program2 <- unique(data1$choicepgm2)
program3 <- unique(data1$choicepgm3)
program4 <- unique(data1$choicepgm4)
program5 <- unique(data1$choicepgm5)
program6 <- unique(data1$choicepgm6)
program7 <- unique(na.omit(c(program1, program2, program3, program4, program5, program6)))
programs <- program7[program7 != ""]
length(programs)

# No. of choices
choice1=paste0(schoolcode1,choicepgm1)
choice2=paste0(schoolcode2,choicepgm2)
choice3=paste0(schoolcode3,choicepgm3)
choice4=paste0(schoolcode4,choicepgm4)
choice5=paste0(schoolcode5,choicepgm5)
choice6=paste0(schoolcode6,choicepgm6)

c1 = unique(choice1)
c2 = unique(choice2)
c3 = unique(choice3)
c4 = unique(choice4)
c5 = unique(choice5)
c6 = unique(choice6)
c7 <- unique(na.omit(c(c1, c2, c3, c4, c5, c6)))
choices <- c7[c7 != ""]
length(choices)

# Missing test score
no_scores = sum(is.na(score))
no_scores

# Apply to the same school (different programs)
A1_coef_sc<-c("schoolcode1","schoolcode2","schoolcode3","schoolcode4","schoolcode5","schoolcode6")
A1_coef_sc
A1_sc<-data1[,A1_coef_sc]
A1_sc$Count <- apply(A1_sc[,1:6], 1, function(x) length(unique(na.omit(x))))
A1_sc$Count_all<-apply(A1_sc[,1:6], 1, function(x) 6-sum(is.na(x)))
length(as.matrix(A1_sc[A1_sc$Count<A1_sc$Count_all,"Count"]))

# Apply to less than 6 choices
data1 <- read.csv("datstu.csv",header = TRUE,sep = ",",na.strings = c(""))
pgm_all<-c("choicepgm1","choicepgm2","choicepgm3","choicepgm4","choicepgm5","choicepgm6")
A1_pgm_all<-as.matrix(data1[,pgm_all])
length(as.matrix(data1[!complete.cases(A1_pgm_all),"score"]))


# Exercise 2

# the district where the school is located
data1 <- read.csv("datstu.csv",header = TRUE)
attach(data1)
data2 <-read.csv("datjss.csv",header=TRUE)
data3 <-read.csv("datsss.csv",header=TRUE)
data1 = data1 %>% mutate(choice_1=paste0(schoolcode1,choicepgm1),
                         choice_2=paste0(schoolcode2,choicepgm2),
                         choice_3=paste0(schoolcode3,choicepgm3),
                         choice_4=paste0(schoolcode4,choicepgm4),
                         choice_5=paste0(schoolcode5,choicepgm5),
                         choice_6=paste0(schoolcode6,choicepgm6))
dat = select(data1,choice_1,choice_2,choice_3,choice_4,choice_5,choice_6)
View(dat)
dat_long = gather(dat,'key','value')
data3_unique<-na.omit(data3[,c("schoolcode","sssdistrict","ssslong","ssslat")])
dat_long = dat_long %>% mutate(schoolcode = gsub('\\D','',value),pgm = gsub('\\d','',value))
dat_long$schoolcode = as.numeric(dat_long$schoolcode)
dat_long = dat_long %>% inner_join(data3_unique)
data1_unique<- data1 %>% filter(!is.na(rankplace) & rankplace <= 6)
function_1 = function(X){
  a = as.numeric(X[18])
  result1 = X[a+4]
  return(result1)
}
data1_unique$schoolcode = apply(data1_unique,1,function_1)
View(data1_unique)
library(plyr)
library(dplyr)
data1_unique2 = ddply(data1_unique,c("schoolcode"),summarize, cutoff= min(score),quality=mean(score),size=length(score))
data1_unique2$schoolcode = as.numeric(data1_unique2$schoolcode)
dat_long = dat_long %>% inner_join(data1_unique2)
dat_long = dat_long %>% distinct()
View(dat_long)



# Exercise 3
names(data2)[3] = "jsslong"
names(data2)[4] = "jsslat"
data4 = left_join(data2, data3)
View(data4)
data4 = data4 %>% mutate(y=sqrt((69.172*(ssslong - jsslong)*cos(jsslat/57.3))^2 + (69.172 *(ssslat - jsslat))^2))
names(data4)[10] = "distance"
View(data4)

# Exercise 4
mean(dat_long$cutoff)
sd(dat_long$cutoff)
mean(dat_long$quality)
sd(dat_long$quality)
mean(data4$distance)
sd(data4$distance)
data1 <- read.csv("datstu.csv",header = TRUE)
data1$score = as.numeric(data1$score)
data1$quantcut<-quantcut(data1$score, q=4, na.rm=TRUE)
View(data1)

# Exercise 5
set.seed(1)
X<-data.frame(matrix(ncol = 6, nrow = 10000))
colnames(X)<-c("x1","x2","x3","e","yhat","ydum")
X$x1 = runif(10000,1,3)
X$x2 = rgamma(10000,3,2)
X$x3 = rbinom(10000,1,0.3)
X$e = rnorm(10000,2,1)
X$yhat = 0.5 + 1.2*X$x1 - 0.9*X$x2 +0.1*X$x3 + X$e
X$yhat>mean(X$yhat)
X$ydum = as.numeric(X$yhat>mean(X$yhat))
X$ydum

# Exercise 6
cor(X$yhat,X$x1)
X$x0<-1
A1_X<-as.matrix(X[,c(7,1:3)])
A1_Y<-as.matrix(X[,5])
betas <- solve(t(A1_X) %*% A1_X) %*% t(A1_X) %*% A1_Y
residuals <- A1_Y - A1_X %*% betas
rows<-nrow(A1_X)
columns <- ncol(A1_X) - 1
residual_var <- t(residuals) %*% residuals / (rows - columns - 1)
residual_var <- as.numeric(residual_var)
beta_covar <- residual_var * solve(t(A1_X) %*% A1_X)
beta_covar
beta_SE <- data.frame(sqrt(diag(beta_covar)))
beta_SE
colnames(beta_SE)<-"SE"
X_result<-cbind(betas,beta_SE)
colnames(X_result)[1]<-"betas"
summary(lm(A1_Y~A1_X))


# Exercise 7
ydum<-X$ydum
# Probit
fprobit = function(par,x1,x2,x3,yvar)
{
  xbeta           = par[1] + par[2]*x1 + par[3]*x2 + par[4]*x3
  pr              = pnorm(xbeta)
  pr[pr>0.999999] = 0.999999
  pr[pr<0.000001] = 0.000001
  probit           = yvar*log(pr) + (1-yvar)*log(1-pr)
  return(-sum(probit))
}
set.seed(666)
start = runif(4)
prob = optim(start,fn=fprobit,method="BFGS",control=list(trace=6,REPORT=1,maxit=2000),x1=A1_X[,2],x2=A1_X[,3],x3=A1_X[,4],yvar=ydum,hessian=TRUE)
fisher_info = solve(prob$hessian)
prop_sigma  = sqrt(diag(fisher_info))
prop_sigma
glm_p<-glm(ydum~A1_X[,2:4],family = binomial(link = "probit"))
prob_est = cbind(summary(glm_p)$coefficients[,1],summary(glm_p)$coefficients[,2], prob$par,prop_sigma)
colnames(prob_est) = c("glm : est","glm :se","own : est","own :se")
prob_est
# Logit
flogit = function(par,x1,x2,x3,yvar)
{
  xbeta           = par[1] + par[2]*x1 + par[3]*x2 + par[4]*x3
  pr              = exp(xbeta)/(1+exp(xbeta))
  pr[pr>0.999999] = 0.999999
  pr[pr<0.000001] = 0.000001
  logit           = yvar*log(pr) + (1-yvar)*log(1-pr)
  return(-sum(logit))
}
start = runif(4)
logi = optim(start,fn=flogit,method="BFGS",control=list(trace=6,REPORT=1,maxit=1000),x1=A1_X[,2],x2=A1_X[,3],x3=A1_X[,4],yvar=ydum,hessian=TRUE)
fisher_info2 = solve(logi$hessian)
logit_sigma  = sqrt(diag(fisher_info2))
logit_sigma
glm_l<-glm(ydum~A1_X[,2:4],family = binomial(link = "logit"))
logit_est = cbind(summary(glm_l)$coefficients[,1],summary(glm_l)$coefficients[,2], logi$par,logit_sigma)
colnames(logit_est) = c("glm : est","glm :se","own : est","own :se")
logit_est
# linear probability
lp_betas <- solve(t(A1_X) %*% A1_X) %*% t(A1_X) %*% ydum
lp_residuals <- ydum - A1_X %*% lp_betas
rows<-nrow(A1_X)
columns <- ncol(A1_X) - 1
lp_residual_var <- t(lp_residuals) %*% lp_residuals / (rows - columns - 1)
lp_residual_var <- as.numeric(lp_residual_var)
lp_beta_covar <- lp_residual_var * solve(t(A1_X) %*% A1_X)
lp_beta_SE <- data.frame(sqrt(diag(lp_beta_covar)))
colnames(lp_beta_SE)<-"SE"
lp_result<-cbind(lp_betas,lp_beta_SE)
lp_result
colnames(lp_result)[1]<-"betas"
###check
lp<-lm(ydum~A1_X[,2:4])
lp_est = cbind(summary(lp)$coefficients[,1],summary(lp)$coefficients[,2], lp_result)
colnames(lp_est) = c("lm : est","lm :se","own : est","own :se")
lp_est


# Exercise 8
x1=A1_X[,2]
x2=A1_X[,3]
x3=A1_X[,4]
yvar=ydum
# probit
fprobit2 <- function(x1,x2,x3,yvar){
  start = runif(4)
  probit2 = optim(start,fn=fprobit,method="BFGS",control=list(trace=6,maxit=2000),x1=x1,x2=x2,x3=x3,yvar=yvar,hessian=TRUE)
  # get marginal effects
  pdf <- dnorm(A1_X %*% probit2$par)
  marginal.effects <- pdf%*%probit2$par
  probit_mean<-colMeans(marginal.effects)
  return(probit_mean)}
probit_me<-fprobit2(x1,x2,x3,yvar)
###logit
flogit2 <- function(x1,x2,x3,yvar){
  start = runif(4)
  logit2 = optim(start,fn=flogit,method="BFGS",control=list(trace=6,maxit=2000),x1=x1,x2=x2,x3=x3,yvar=yvar,hessian=TRUE)
  # get marginal effects
  pdf <- dlogis(A1_X %*% logit2$par)
  marginal.effects <- pdf%*%logit2$par
  logit_mean<-colMeans(marginal.effects)
  return(logit_mean)}
logit_me<-flogit2(x1,x2,x3,yvar)
# start bootstrap
rows1 = nrow(A1_X);  
pb<-data.frame(matrix(ncol = 4, nrow = 100))
lt<-data.frame(matrix(ncol = 4, nrow = 100))
for(i in 1:100){
  samp= sample(1:rows1,rows1,rep=TRUE)
  X2<-A1_X[samp,]
  ydum2<-ydum[samp]
  x1=X2[,2]
  x2=X2[,3]
  x3=X2[,4]
  yvar=ydum2
  pb[i,]<-fprobit2(x1,x2,x3,yvar)
  lt[i,]<-flogit2(x1,x2,x3,yvar)
}
pb<-sapply(pb,function(x) c( "Stand dev" = sd(x,na.rm=TRUE), 
                                               "Mean"= mean(x,na.rm=TRUE)))
pb
lt<-sapply(lt,function(x) c( "Stand dev" = sd(x,na.rm=TRUE), 
                                               "Mean"= mean(x,na.rm=TRUE)))
lt
colnames(pb)<-c("X0","X1","X2","X3")
colnames(lt)<-c("X0","X1","X2","X3")