
#================================================= R Script for Econ613_HW #3 ==========================================
#================================================= March 19,2019 ==========================================

# import dataset demos and product
demos <- read.csv(choose.files(), header = T, na.strings = c("","NA") , stringsAsFactors = F)
product <- read.csv(choose.files(), header = T, na.strings = c("","NA") , stringsAsFactors = F)



#========================================== EXERCISE 1  Data Description ==========================================
#=================================================================
# Question 1 : Average and dispersion in product characteristics
#=================================================================
# calculate average for each product choice
apply(product[,4:13], 2, mean)
# calculate variance for each product choice
apply(product[,4:13], 2, var)

#=================================================================
# Question 2 : Market share, and market share by product characteristics
#=================================================================
# (1) market share for each brand
# first, form a matrix that each rows is one brand and its frequency
market_freq <- as.matrix(c(nrow(subset(product,product$choice=="1"|product$choice=="8")),
                           nrow(subset(product,product$choice=="2")),
                           nrow(subset(product,product$choice=="3"|product$choice=="9")),
                           nrow(subset(product,product$choice=="4"|product$choice=="10")),
                           nrow(subset(product,product$choice=="5")),nrow(subset(product,product$choice=="6")),
                           nrow(subset(product,product$choice=="7"))
))
# name columns and rows                 
rownames(market_freq) <- c("PPk","PBB","PFl","PHse","PGen","Plmp","PSS") 
colnames(market_freq) <- "frequency"
# calculate proportion for each brand
share <- as.vector(prop.table(market_freq,2)*100)
# save the result
market_share <- cbind(market_freq,share)
write.csv(market_share,"Market Share by product brands.csv")

# (2) market share by product characteristics
# first, form a matrix that each rows is one type of product and its frequency
market_freq1 <- as.matrix(c(nrow(subset(product,product$choice <="6")),
                            nrow(subset(product,product$choice >="7"))
))
# name columns and rows  
rownames(market_freq1) <- c("Stk","Tub") 
colnames(market_freq1) <- "frequency1"
# calculate proportion for each type
share1 <- as.vector(prop.table(market_freq1,2)*100)
# save the result
market_share1 <- cbind(market_freq1,share1)
write.csv(market_share1,"Market Share by product types.csv")

#=================================================================
# Question 3 : Mapping between observed attributes and choices
#=================================================================
# form a matrix that contains each household's income level and choices
IncCho <- merge(demos[,c(2:3)],product[,c(2:3)], by="hhid")
# calculate choice frequency for each income level
E1Q3_freq <- as.data.frame(unclass(table(IncCho$Income,IncCho$choice)))
# calculate choice share for each income level
E1Q3 <- as.matrix(table(IncCho$Income,IncCho$choice))
E1Q3_share <- as.data.frame(unclass(prop.table(E1Q3,1)*100))



#========================================== EXERCISE 2  First Model ==========================================

## Since income is invariant across alternatives the appropriate model is /Conditional logit model/.

# first form a matrix that cotains price information
price <- cbind(product[,c(3:13)])
colnames(price) <- c("choice","P1","P2","P3","P4","P5","P6","P7","P8","P9","P10")

# set base choice and calculate tilde price
price_tilde <- NULL
for(i in 1:10){
  price_til <- price[,i+1] - price[,2]
  price_tilde <- cbind(price_tilde, price_til)
}

# form individual choice matrix dij
dij1 <- ifelse(product$choice == "1", 1, 0)
dij2 <- ifelse(product$choice == "2", 1, 0)
dij3 <- ifelse(product$choice == "3", 1, 0)
dij4 <- ifelse(product$choice == "4", 1, 0)
dij5 <- ifelse(product$choice == "5", 1, 0)
dij6 <- ifelse(product$choice == "6", 1, 0)
dij7 <- ifelse(product$choice == "7", 1, 0)
dij8 <- ifelse(product$choice == "8", 1, 0)
dij9 <- ifelse(product$choice == "9", 1, 0)
dij10 <- ifelse(product$choice == "10", 1, 0)
dij <- cbind(dij1,dij2,dij3,dij4,dij5,dij6,dij7,dij8,dij9,dij10)

# write log-likelihood function for conditional logit model
CLogit <- function(theta, X, Y){
  alpha = c(0,theta[2:10])
  alpha = sapply(alpha,rep,4470)
  Vij = X*theta[1]+alpha
  Pij = prop.table(exp(Vij),1)
  logl = sum(dij*log(Pij))
  Y = -logl
  return(Y)
}

# optimization
optim(c(0,0,0,0,0,0,0,0,0,0), CLogit, X=price_tilde, method = "BFGS")$par
# [1] -6.6566340 -0.9543259  1.2969965 -1.7173298 -2.9040264 -1.5153021
# [7]  0.2517927  1.4648942  2.3575437 -3.8966267

# interpretation
# For beta (theta[1]) here, which is equals to -6.6566340, the sign is negative, then if price increases, the demand of choosing one of the alternatives will decrease.
# alpha (theta[2:10]) are intercepts. 

#========================================== EXERCISE 3  Second Model ==========================================

## Since income is invariant across alternatives the appropriate model is /Multinomial logit model/.

# first form a matrix that cotains income information
income <- matrix(rep(IncCho[,2],10),ncol=10)

# write log-likelihood function for multinomial logit model
MLogit <- function(theta, X, Y){
  alpha = c(0,theta[10:18])
  alpha = sapply(alpha,rep,4470)
  beta = c(0,theta[1:9]) 
  beta = sapply(beta,rep,4470)
  Vij = X*beta + alpha
  Pij = prop.table(exp(Vij),1)
  logl = sum(dij*log(Pij))
  Y = -logl
  return(Y)
}

# optimization
optim(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), MLogit, X=income, method = "BFGS")$par
# [1] -0.003156338  0.014507166  0.003980338 -0.001328126  0.030527384 -0.007002723  0.022807121  0.017661767  0.010698254
# [10] -0.843545649 -2.397656003 -1.199428121 -1.688616844 -4.137055731 -1.529169108 -2.846055103 -2.573291074 -4.279712751

# interpretation
# For beta (theta[1:9]) here, which is equals to -0.003156338  0.014507166  0.003980338 -0.001328126  0.030527384 -0.007002723  0.022807121  0.017661767  0.010698254 respectively,
# For instance, if income increases, the demand of choosing the first choice will decrease, while the demand of choosing the second choice will increase, etc.
# alpha (theta[10:18]) are intercepts.


#========================================== EXERCISE 4  Marginal Effects ==========================================
## Compute and interprete the marginal effects for the first and second models.##

# (1) First Model —— Conditional Logit Model

# calculate marginal effect
theta1 <- as.vector(optim(c(0,0,0,0,0,0,0,0,0,0), CLogit, X=price_tilde, method = "BFGS")$par)
alpha1 = c(0,theta1[2:10])
alpha1 = sapply(alpha1,rep,4470)
Vij1 = price_tilde*theta1[1]+alpha1
Pij1 = prop.table(exp(Vij1),1)
P_j_bar = as.matrix(colMeans(Pij1))
P_j_bar = t(P_j_bar)
P_j_bar = sapply(P_j_bar,rep,10)
# create a indicator matrix
indicator <- as.matrix(diag(c(1,1,1,1,1,1,1,1,1,1)))
# calculate marginal effects based on formula
ME1 <- P_j_bar *(indicator - P_j_bar)*theta1[1]
# save result as csv.file
write.csv(ME1,"ME_CLgit.csv")

# interpretation
ME1[1,]
# [1] -1.5908791027  0.1627730651  0.0196723154  0.1171541253  0.0330561878  0.0018243221  0.0339023969  0.0137287546  0.0168658904  0.0003627794

# For instance,
# One unit increase in the price of first choice will decrease 1.5908791027 in the probability of choosing first choice, 
# and will increase 0.1627730651 in the probability of choosing the second choice, etc.

# (2) Second Model —— Multinomial Logit Model

# calculate marginal effect
theta2 <- as.vector(optim(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), MLogit, X=income, method = "BFGS")$par)
alpha2 = c(0,theta2[10:18])
alpha2ij = sapply(alpha2,rep,4470)
beta2 = c(0,theta2[1:9])
beta2ij = sapply(beta2,rep,4470) 
Vij2 = income*beta2ij + alpha2ij
Pij2 = prop.table(exp(Vij2),1)
beta_i_bar <- Pij2[,1]*beta2ij[,1]+Pij2[,2]*beta2ij[,2]+Pij2[,3]*beta2ij[,3]+Pij2[,4]*beta2ij[,4]+Pij2[,5]*beta2ij[,5]+Pij2[,6]*beta2ij[,6]+Pij2[,7]*beta2ij[,7]+Pij2[,8]*beta2ij[,8]+Pij2[,9]*beta2ij[,9]+Pij2[,10]*beta2ij[,10]
# calculate marginal effects based on formula ME = pij(betaj-beta_i_bar)
ME2 <- Pij2*(beta2ij - beta_i_bar)
# combine household income information with marginal effect matrix
ME_MLgit <- cbind(IncCho[,2],ME2)
# calculate marginal effect averagefor each choice
ME_MLgit_mean <- as.matrix(colMeans(ME2))
# save result
write.csv(ME_MLgit_mean,"ME_MLgit.csv")

# interpretation
ME_MLgit_mean
#                  [,1]
# choice1  -0.0010504137
# choice2  -0.0009016311
# choice3   0.0006266867
# choice4   0.0001660472
# choice5  -0.0002794477
# choice6   0.0004431356
# choice7  -0.0006821378
# choice8   0.0008861440
# choice9   0.0007338590
# choice10  0.0000577577

# For example, 
# One unit change increase in family income will decrease 0.0010504137 in the probability of choosing first choice compared with other 9 choices
# One unit change increase in family income will decrease 0.0009016311 in the probability of choosing second choice compared with other 9 choices
# One unit change increase in family income will increase 0.0006266867 in the probability of choosing third choice compared with other 9 choices


#========================================== EXERCISE 5  IIA ==========================================
## We are interested in the effect of price and family income.

# write log-likelihood function for Mixed Logit Model
MixLogit <- function(theta, X1, X2, Y){
  alpha = c(0,theta[11:19])
  alpha = sapply(alpha,rep,4470)
  beta = c(0,theta[2:10]) 
  beta = sapply(beta,rep,4470)
  Vij = X1*theta[1] + X2*beta + alpha
  Pij = prop.table(exp(Vij),1)
  logl = sum(dij*log(Pij))
  Y = -logl
  return(Y)
}
# denoted by betaf the estimated coefficients
thetaf <- as.vector(optim(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), MixLogit, X1=price_tilde, X2=income, method = "BFGS")$par)

## remove choice10 and form new matrixes
IncCho[IncCho==10] = NA
IncCho1 <- na.omit(IncCho)
income1 <- matrix(rep(IncCho1[,2],9),ncol=9)

productn <- subset(product[,3:13])
productn[productn==10] = NA
product1 <- na.omit(productn)

dij1n <- ifelse(product1$choice == "1", 1, 0)
dij2n <- ifelse(product1$choice == "2", 1, 0)
dij3n <- ifelse(product1$choice == "3", 1, 0)
dij4n <- ifelse(product1$choice == "4", 1, 0)
dij5n <- ifelse(product1$choice == "5", 1, 0)
dij6n <- ifelse(product1$choice == "6", 1, 0)
dij7n <- ifelse(product1$choice == "7", 1, 0)
dij8n <- ifelse(product1$choice == "8", 1, 0)
dij9n <- ifelse(product1$choice == "9", 1, 0)
dijnew <- cbind(dij1n,dij2n,dij3n,dij4n,dij5n,dij6n,dij7n,dij8n,dij9n)

price[price==10] = NA
price1 <- na.omit(price)
price1_tilde <- NULL
for(i in 1:9){
  price1_til <- price1[,i+1] - price1[,2]
  price1_tilde <- cbind(price1_tilde, price1_til)
}

# write log-likelihood function for new Mixed Logit Model
MixLogit1 <- function(theta, X1, X2, Y){
  alpha = c(0,theta[10:17])
  alpha = sapply(alpha,rep,4437)
  beta = c(0,theta[2:9]) 
  beta = sapply(beta,rep,4437)
  Vij = X1*theta[1] + X2*beta + alpha
  Pij = prop.table(exp(Vij),1)
  logl = sum(dijnew*log(Pij))
  Y = -logl
  return(Y)
}
# obtain optimized coefficients
thetar <- as.vector(optim(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), MixLogit1, X1=price1_tilde, X2=income1, method = "BFGS")$par)

# calculate likelihood for unsubset model
Lr_f <- - MixLogit1(thetaf[c(1:9,11:18)], X1=price1_tilde, X2=income1) 
# calculate likelihood for subset model
Lr_r <- - MixLogit1(thetar, X1=price1_tilde, X2=income1) 
# calculate MTT test statistics
MTT <- -2*(Lr_f-Lr_r) 
# [1] 0.006276177


# Critical Value of chi2(9) at 5% significance level = 16.92
# MTT < CV
# There is not a significant different, so IIA is not violated 
# Probably because the choice 10 is of little marketshare - 0.74%, so removing it will not casue big influence on the market


#==================== Thank you for reading! Feel free making comments! Have a good day! ==================== 


#================================================= The End =================================================


