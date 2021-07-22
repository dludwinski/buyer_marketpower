price<-function(alpha, lambda_0_pct, lambdas, WTP=TRUE){
  
  A<-matrix(0, nrow = length(lambdas), ncol = length(lambdas))
  b<-matrix(0, nrow = length(lambdas), 1) 
  
  Lambda<-sum(lambdas)/(1-lambda_0_pct)
    
  for(i in 1:length(lambdas)) {
    for(j in 1:length(lambdas)) {
      if (i==j) {
        A[j,i]<-1
      } else A[i,j]<- -alpha*lambdas[j]/(Lambda-lambdas[i])
    }
    if(isTRUE(WTP)){
      b<-matrix(10*(1-alpha), length(lambdas), 1)
    } else b[i,1]<-WTP[i]*(1-alpha)
  }
  A
  Prices<-solve(A)%*%b
  
  #max
  max_p <- ((1-alpha)*10)/((1-alpha)+lambda_0_pct*alpha)
  #min
  min_p <- (1-alpha)*10
  #Prices<-1/(1-(1-alpha)-lambda_0_pct*alpha)-(Prices/((1-alpha)*10))*((1-alpha)+lambda_0_pct*alpha)/(1-(1-alpha)-lambda_0_pct*alpha)
  
  #(Prices-min_p)/(max_p-min_p)
  
  return(Prices)
}

price(alpha=0.8, lambda_0_pct=0.2, cbind(1,1,1,1,1,1))

alpha<-0.8
p6<-price_hhi(alpha, p_m=0, lambda_m_pct=0, lambda_0_pct=0.2, HHI=10000/6)

lambda_0_pct<-0.2
max_p<-((1-alpha)*10)/((1-alpha)+lambda_0_pct*alpha)
min_p <- (1-alpha)*10

(p6/((1-alpha)*10)-1)*((1-alpha)+lambda_0_pct*alpha)/(alpha*(1-lambda_0_pct))

(p6-min_p)/(max_p-min_p)




prices_5_med(alpha,pm=-10000,lambda_m_pct=0,lambda_0_pct=0.2, 0.6, 0.2, 0.2, 0, 0)


price_med<-function(alpha, lambda_0_pct, lambda_m_pct, p_m, lambdas, WTP){
  
  A<-matrix(0, nrow = length(lambdas), ncol = length(lambdas))
  b<-matrix(0, nrow = length(lambdas), 1)  
  
  Lambda<-sum(lambdas)/(1-lambda_0_pct)
  lambda_m=lambda_m_pct/(1-lambda_m_pct)*sum(lambdas)
  Lambda<-sum(lambdas)/(1-lambda_0_pct)/(1-lambda_m_pct)
  
  for(i in 1:length(lambdas)) {
    for(j in 1:length(lambdas)) {
      if (i==j) {
        A[j,i]<-1
      } else A[i,j]<- -alpha*lambdas[j]/(Lambda-lambdas[i])
    }
    b[i,1]<-WTP[i]*(1-alpha)+alpha*lambda_m/(Lambda-lambdas[i])*p_m
  }
  A
  #solve(A)%*%b
}

price_med(alpha=0.8, lambda_0_pct=0.2, lambda_m_pct = 0, p_m=3.5
          , cbind(0.6, 0.2, 0.2), matrix(10, nrow = 3, 1) )


prices_5_med(alpha,pm=3.5,lambda_m_pct=0.4,lambda_0_pct=0.2, 0.6, 0.2, 0.2, 0, 0)


a<-cbind(r1, r2)

0.25

price_hhi<-function(alpha, p_m, lambda_m_pct, lambda_0_pct, HHI){
  
  Y<-(1-lambda_0_pct)
  M<-(1-lambda_m_pct)
  n<-1/(HHI/10000)
  a<-alpha
  
  WTP<-10
#  price<-((n-(1-lambda_0_pct))*(1-a)*WTP)/(n-(1+a*(n-1))*(1-lambda_0_pct))
#  price<-((n-(1-lambda_0_pct))*(1-a)*WTP)/(n-1+lambda_0_pct-a*(n-1)*(1-lambda_0_pct))
  
  price<-((n-Y*M)*(1-a)*WTP+a*(1/(HHI/10000))*(lambda_m_pct)*(1-lambda_0_pct)*p_m)/(n-(1+a*(n-1))*Y*M)
#  price<-((n-Y*M)*(1-a)*WTP+a*n*(1-M)*Y*p_m)/(n-(1+a*(n-1))*Y*M)
#  price<-((n-Y*M)*(1-a)*WTP)/(n-(1+a*(n-1))*Y*M)/3
  return(price)
}


price_hhi(0.8,3.5,0.35,0.2,5000)


prices_10_med(alpha,pm=3.5,lambda_m_pct=0,lambda_0_pct=0.2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
prices_10_med(alpha,pm=3.5,lambda_m_pct=0,lambda_0_pct=0.2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
prices_10_med(alpha,pm=3.5,lambda_m_pct=0,lambda_0_pct=0.2, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0)

prices_10_med(alpha,pm=3.5,lambda_m_pct=0.35,lambda_0_pct=0.2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
prices_10_med(alpha,pm=3.5,lambda_m_pct=0.35,lambda_0_pct=0.2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
prices_10_med(alpha,pm=3.5,lambda_m_pct=0.35,lambda_0_pct=0.2, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0)




view_in_excel <- function(tablename) {
  tmp_a<-paste(deparse(substitute(tablename)),".csv",sep="")
  write.csv(tablename,tmp_a, row.names = TRUE)
  shell(tmp_a, wait=FALSE)
}


setwd("C:\\Users\\dludwin\\OneDrive\\Docs\\Research\\Bargaining\\Simulations") 
#install.packages("matlib")
#library(matlib)

# Functions ---------------------------------------------------------------



prices_10_med <- function(alpha, pm, lambda_m_pct, lambda_0_pct,lambda_1, lambda_2, lambda_3, lambda_4, lambda_5, lambda_6, lambda_7, lambda_8, lambda_9, lambda_10){
  
  #WTP_0 <- 10
  WTP_1 <- 10
  WTP_2 <- 10
  WTP_3 <- 10
  WTP_4 <- 10
  WTP_5 <- 10
  WTP_6 <- 10
  WTP_7 <- 10
  WTP_8 <- 10
  WTP_9 <- 10
  WTP_10 <- 10
  
  lambda_m = lambda_m_pct/(1-lambda_m_pct)*(lambda_1 +lambda_2 +lambda_3 +lambda_4 +lambda_5 +lambda_6 +lambda_7 +lambda_8 +lambda_9 +lambda_10)
  
  lambda_0<-lambda_0_pct/(1-lambda_0_pct)*(lambda_1 +lambda_2 +lambda_3 +lambda_4 +lambda_5 +lambda_6 +lambda_7 +lambda_8 +lambda_9 +lambda_10 + lambda_m)
  
  Lambda <- lambda_0+lambda_1 +lambda_2 +lambda_3 +lambda_4 +lambda_5 +lambda_6 +lambda_7 +lambda_8 +lambda_9 +lambda_10+ lambda_m
  
  A10 <- rbind(    c(1, -alpha*lambda_2/(Lambda-lambda_1), -alpha*lambda_3/(Lambda-lambda_1), -alpha*lambda_4/(Lambda-lambda_1), -alpha*lambda_5/(Lambda-lambda_1), -alpha*lambda_6/(Lambda-lambda_1), -alpha*lambda_7/(Lambda-lambda_1), -alpha*lambda_8/(Lambda-lambda_1), -alpha*lambda_9/(Lambda-lambda_1), -alpha*lambda_10/(Lambda-lambda_1)),
                   c(-alpha*lambda_1/(Lambda-lambda_2), 1, -alpha*lambda_3/(Lambda-lambda_2), -alpha*lambda_4/(Lambda-lambda_2), -alpha*lambda_5/(Lambda-lambda_2), -alpha*lambda_6/(Lambda-lambda_2), -alpha*lambda_7/(Lambda-lambda_2), -alpha*lambda_8/(Lambda-lambda_2), -alpha*lambda_9/(Lambda-lambda_2), -alpha*lambda_10/(Lambda-lambda_2)),
                   c(-alpha*lambda_1/(Lambda-lambda_3), -alpha*lambda_2/(Lambda-lambda_3), 1, -alpha*lambda_4/(Lambda-lambda_3), -alpha*lambda_5/(Lambda-lambda_3), -alpha*lambda_6/(Lambda-lambda_3), -alpha*lambda_7/(Lambda-lambda_3), -alpha*lambda_8/(Lambda-lambda_3), -alpha*lambda_9/(Lambda-lambda_3), -alpha*lambda_10/(Lambda-lambda_3)),
                   c(-alpha*lambda_1/(Lambda-lambda_4), -alpha*lambda_2/(Lambda-lambda_4), -alpha*lambda_3/(Lambda-lambda_4), 1, -alpha*lambda_5/(Lambda-lambda_4), -alpha*lambda_6/(Lambda-lambda_4), -alpha*lambda_7/(Lambda-lambda_4), -alpha*lambda_8/(Lambda-lambda_4), -alpha*lambda_9/(Lambda-lambda_4), -alpha*lambda_10/(Lambda-lambda_4)),
                   c(-alpha*lambda_1/(Lambda-lambda_5), -alpha*lambda_2/(Lambda-lambda_5), -alpha*lambda_3/(Lambda-lambda_5), -alpha*lambda_4/(Lambda-lambda_5), 1, -alpha*lambda_6/(Lambda-lambda_5), -alpha*lambda_7/(Lambda-lambda_5), -alpha*lambda_8/(Lambda-lambda_5), -alpha*lambda_9/(Lambda-lambda_5), -alpha*lambda_10/(Lambda-lambda_5)),
                   c(-alpha*lambda_1/(Lambda-lambda_6), -alpha*lambda_2/(Lambda-lambda_6), -alpha*lambda_3/(Lambda-lambda_6), -alpha*lambda_4/(Lambda-lambda_6), -alpha*lambda_5/(Lambda-lambda_6), 1, -alpha*lambda_7/(Lambda-lambda_6), -alpha*lambda_8/(Lambda-lambda_6), -alpha*lambda_9/(Lambda-lambda_6), -alpha*lambda_10/(Lambda-lambda_6)),
                   c(-alpha*lambda_1/(Lambda-lambda_7), -alpha*lambda_2/(Lambda-lambda_7), -alpha*lambda_3/(Lambda-lambda_7), -alpha*lambda_4/(Lambda-lambda_7), -alpha*lambda_5/(Lambda-lambda_7), -alpha*lambda_6/(Lambda-lambda_7), 1, -alpha*lambda_8/(Lambda-lambda_7), -alpha*lambda_9/(Lambda-lambda_7), -alpha*lambda_10/(Lambda-lambda_7)),
                   c(-alpha*lambda_1/(Lambda-lambda_8), -alpha*lambda_2/(Lambda-lambda_8), -alpha*lambda_3/(Lambda-lambda_8), -alpha*lambda_4/(Lambda-lambda_8), -alpha*lambda_5/(Lambda-lambda_8), -alpha*lambda_6/(Lambda-lambda_8), -alpha*lambda_7/(Lambda-lambda_8), 1, -alpha*lambda_9/(Lambda-lambda_8), -alpha*lambda_10/(Lambda-lambda_8)),
                   c(-alpha*lambda_1/(Lambda-lambda_9), -alpha*lambda_2/(Lambda-lambda_9), -alpha*lambda_3/(Lambda-lambda_9), -alpha*lambda_4/(Lambda-lambda_9), -alpha*lambda_5/(Lambda-lambda_9), -alpha*lambda_6/(Lambda-lambda_9), -alpha*lambda_7/(Lambda-lambda_9), -alpha*lambda_8/(Lambda-lambda_9), 1, -alpha*lambda_10/(Lambda-lambda_9)),
                   c(-alpha*lambda_1/(Lambda-lambda_10), -alpha*lambda_2/(Lambda-lambda_10), -alpha*lambda_3/(Lambda-lambda_10), -alpha*lambda_4/(Lambda-lambda_10), -alpha*lambda_5/(Lambda-lambda_10), -alpha*lambda_6/(Lambda-lambda_10), -alpha*lambda_7/(Lambda-lambda_10), -alpha*lambda_8/(Lambda-lambda_10), -alpha*lambda_9/(Lambda-lambda_10), 1)
  )
  
  WTP_10I <- rbind((1-alpha)*WTP_1+alpha*lambda_m/(Lambda-lambda_1)*pm, 
                   (1-alpha)*WTP_2+alpha*lambda_m/(Lambda-lambda_2)*pm, 
                   (1-alpha)*WTP_3+alpha*lambda_m/(Lambda-lambda_3)*pm, 
                   (1-alpha)*WTP_4+alpha*lambda_m/(Lambda-lambda_4)*pm, 
                   (1-alpha)*WTP_5+alpha*lambda_m/(Lambda-lambda_5)*pm,
                   (1-alpha)*WTP_6+alpha*lambda_m/(Lambda-lambda_6)*pm,
                   (1-alpha)*WTP_7+alpha*lambda_m/(Lambda-lambda_7)*pm,
                   (1-alpha)*WTP_8+alpha*lambda_m/(Lambda-lambda_8)*pm,
                   (1-alpha)*WTP_9+alpha*lambda_m/(Lambda-lambda_9)*pm,
                   (1-alpha)*WTP_10+alpha*lambda_m/(Lambda-lambda_10)*pm )
  
  p10 <- solve(A10)%*%WTP_10I
  
  s<-t(c(p10, 1/det(A10)) )
  return(s)
}



prices_5_med <- function(alpha, pm, lambda_m_pct, lambda_0_pct,lambda_1, lambda_2, lambda_3, lambda_4, lambda_5){
    
    WTP_0 <- 10
    WTP_1 <- 10
    WTP_2 <- 10
    WTP_3 <- 10
    WTP_4 <- 10
    WTP_5 <- 10
    
    lambda_m = lambda_m_pct/(1-lambda_m_pct)*(lambda_1 +lambda_2 +lambda_3 +lambda_4 +lambda_5)
    
    lambda_0<-lambda_0_pct/(1-lambda_0_pct)*(lambda_1 +lambda_2 +lambda_3 +lambda_4 +lambda_5 + lambda_m)
  Lambda <- lambda_0+lambda_1 +lambda_2 +lambda_3 +lambda_4 +lambda_5 + lambda_m
  
  A5 <- rbind(    c(1, -alpha*lambda_2/(Lambda-lambda_1), -alpha*lambda_3/(Lambda-lambda_1), -alpha*lambda_4/(Lambda-lambda_1), -alpha*lambda_5/(Lambda-lambda_1)),
                  c(-alpha*lambda_1/(Lambda-lambda_2), 1, -alpha*lambda_3/(Lambda-lambda_2), -alpha*lambda_4/(Lambda-lambda_2), -alpha*lambda_5/(Lambda-lambda_2)),
                  c(-alpha*lambda_1/(Lambda-lambda_3), -alpha*lambda_2/(Lambda-lambda_3), 1, -alpha*lambda_4/(Lambda-lambda_3), -alpha*lambda_5/(Lambda-lambda_3)),
                  c(-alpha*lambda_1/(Lambda-lambda_4), -alpha*lambda_2/(Lambda-lambda_4), -alpha*lambda_3/(Lambda-lambda_4), 1, -alpha*lambda_5/(Lambda-lambda_4)),
                  c(-alpha*lambda_1/(Lambda-lambda_5), -alpha*lambda_2/(Lambda-lambda_5), -alpha*lambda_3/(Lambda-lambda_5), -alpha*lambda_4/(Lambda-lambda_5), 1)
  )
  
  WTP_5I <- rbind((1-alpha)*WTP_1+alpha*lambda_m/(Lambda-lambda_1)*pm, 
                  (1-alpha)*WTP_2+alpha*lambda_m/(Lambda-lambda_2)*pm, 
                  (1-alpha)*WTP_3+alpha*lambda_m/(Lambda-lambda_3)*pm, 
                  (1-alpha)*WTP_4+alpha*lambda_m/(Lambda-lambda_4)*pm, 
                  (1-alpha)*WTP_5+alpha*lambda_m/(Lambda-lambda_5)*pm )
  
  p5 <- solve(A5)%*%WTP_5I
  
  s<-t(p5)
  return(s)
}



# Analysis for paper ------------------------------------------------------
  
#GA vs AL (and merger)
  lambda0pct<-0.20
  alpha<-0.8
  pct_med <- 0.35
  med_price <- 3.5
  
    GA_AL_Sim<-data.frame(
      MS_AL=c(75, 10, 7.5, 5, 2.5),
      MS_GA=c(35.0, 27.5, 15.0, 12.5, 10.0),
      MS_GA_2=c(45.0, 27.5, 15.0, 12.5, 0.0),
      AL_P=t(prices_5_med(alpha,pm=med_price,lambda_m_pct=pct_med,lambda0pct, 75, 10, 7.5, 5, 2.5)),
      GA_P=t(prices_5_med(alpha,pm=med_price,lambda_m_pct=pct_med,lambda0pct, 35.0, 27.5, 15.0, 12.5, 10.0)),
      GA_PostMerge_P=t(prices_5_med(alpha,pm=med_price,lambda_m_pct=pct_med,lambda0pct, 45.0, 27.5, 15.0, 12.5, 0.0))
    )
    
    view_in_excel(GA_AL_Sim)
    
  
  #Alabama
    prices_5_med(alpha,pm=-10000,lambda_m_pct=0,lambda0pct, 75, 10, 7.5, 5, 2.5)
  
  #Georgia
    prices_5_med(alpha,pm=-10000,lambda_m_pct=0,lambda0pct, 30, 25, 15, 10, 10)
  
  #Georgia - post merger
    prices_5_med(alpha,pm=-10000,lambda_m_pct=0,lambda0pct, 0.3240, 0.2781, 0.1400, 0.1358, 0.1221)
  

    prices_5_med(alpha,pm=3.5,lambda_m_pct=0.1,0.2, 50, 50, 0, 0, 0)
    prices_10_med(alpha,pm=3.5,lambda_m_pct=0.1,0.2, 50, 50, 0, 0, 0, 0, 0, 0, 0, 0)
    
#Other paper comparisons
    #Compare to McKeller et al. (2013)
    alpha <- 0.8
    med_price <- 3.5
    med_pct <- 0.3
    
    
    b<-cbind(.5, .5, .0, .0, .0)
    (t(matrix(prices_5_med(alpha, pm=med_price, lambda_m_pct=med_pct, lambda_0_pct=0.2,b[1],b[2],b[3],b[4],b[5]))) %*% matrix(b))[1,1] / sum(b)
    
    b<-cbind(1/3, 1/3, 1/3, .0, .0)
    (t(matrix(prices_5_med(alpha, pm=med_price, lambda_m_pct=med_pct, lambda_0_pct=0.2,b[1],b[2],b[3],b[4],b[5]))) %*% matrix(b))[1,1] / sum(b)
    
      
    
    
    #Moriya: A 5 to 4 merger
    alpha <- 0.8
    med_price <- 3.5
    med_pct <- 0
    
    
    b<-cbind(.2, .2, .2, .2, .2)
    (t(matrix(prices_5_med(alpha, pm=med_price, lambda_m_pct=med_pct, lambda_0_pct=0.2,b[1],b[2],b[3],b[4],b[5]))) %*% matrix(b))[1,1] / sum(b)
    
    b<-cbind(.4, .2, .2, .2, 0)
    (t(matrix(prices_5_med(alpha, pm=med_price, lambda_m_pct=med_pct, lambda_0_pct=0.2,b[1],b[2],b[3],b[4],b[5]))) %*% matrix(b))[1,1] / sum(b)
    
    
    #Halbersma et al 2011: A 5 to 4 merger. 3 to 2
    alpha <- 0.8
    med_price <- 3.5
    med_pct <- 0.3
    
    
    b<-cbind(1/3, 1/3, 1/3, 0, 0)
    (t(matrix(prices_5_med(alpha, pm=med_price, lambda_m_pct=med_pct, lambda_0_pct=0.2,b[1],b[2],b[3],b[4],b[5]))) %*% matrix(b))[1,1] / sum(b)
    
    
    b<-cbind(2/3, 1/3, 0, 0, 0)
    (t(matrix(prices_5_med(alpha, pm=med_price, lambda_m_pct=med_pct, lambda_0_pct=0.2,b[1],b[2],b[3],b[4],b[5]))) %*% matrix(b))[1,1] / sum(b)
    
    
    
    #Roberts, E.T., Chernew, M.E., Michael McWilliams, J., 2017
    alpha <- 0.8
    med_price <- 3
    med_pct <- 0.3
    
    
    
  
# Other Analysis (checking stuff)  ------------------------------------------------------



#HHI Simulation (just random MS)
coeftable <- data.frame("lambda_pct"=double(),
                        "ln_HHI_Coef"=double(),
                        "HHI_Coef"=double(),
                        "HHI_sq_Coef"=double(),
                        "Avg_Price"=double(),
                        "Note"=character(),
                          stringsAsFactors=FALSE) 

  
  lambda_list <- list(0.10, 0.15, 0.20, 0.25, 0.30, 0.35)
  
  # loop over lambdas 
  for (lambda0pct in lambda_list){
    
    #lambda0pct<-0.10
    alpha <- 0.8
    
    pricetable <- data.frame(Doubles=double(),
                             Doubles=double(),
                             Doubles=double(),
                             Doubles=double(),
                             Doubles=double(),
                             Doubles=double(),
                             Doubles=double(),
                             Doubles=double(),
                             Doubles=double(),
                             Doubles=double(),
                             Doubles=double()) 
    
    names(pricetable) <- c("lambda pct", "l1", "l2", "l3", "l4", "l5", "p1", "p2", "p3", "p4", "p5")  
    
    
    
    #med_pct <- 0.3
    #med_price <- 3
    med_pct <- 0
    med_price <- -9999
    
    for(i in 1:1000) {
      r1<-1000 
      r2<-runif(1, 0, r1) 
      r3<-runif(1, 0, r2)
      r4<-runif(1, 0, r3)
      r5<-runif(1, 0, r4)
      a<-cbind(lambda0pct, r1, r2, r3, r4, r5)
      
      pricetable[nrow(pricetable)+1,] <-  cbind(a, 
                                                prices_5_med(alpha,pm=med_price,lambda_m_pct=med_pct,a[1],a[2],a[3],a[4],a[5],a[6])
      )
      
    }
    
    pricetable$MS1<-pricetable$l1/(pricetable$l1+pricetable$l2+pricetable$l3+pricetable$l4+pricetable$l5)
    pricetable$MS2<-pricetable$l2/(pricetable$l1+pricetable$l2+pricetable$l3+pricetable$l4+pricetable$l5)
    pricetable$MS3<-pricetable$l3/(pricetable$l1+pricetable$l2+pricetable$l3+pricetable$l4+pricetable$l5)
    pricetable$MS4<-pricetable$l4/(pricetable$l1+pricetable$l2+pricetable$l3+pricetable$l4+pricetable$l5)
    pricetable$MS5<-pricetable$l5/(pricetable$l1+pricetable$l2+pricetable$l3+pricetable$l4+pricetable$l5)
    
    pricetable$Avg_Price <- (pricetable$MS1*pricetable$p1+pricetable$MS2*pricetable$p2+pricetable$MS3*pricetable$p3+pricetable$MS4*pricetable$p4+pricetable$MS5*pricetable$p5)/(pricetable$MS1+pricetable$MS2+pricetable$MS3+pricetable$MS4+pricetable$MS5)
    
    pricetable$HHI <- (pricetable$MS1^2+pricetable$MS2^2+pricetable$MS3^2+pricetable$MS4^2+pricetable$MS5^2)*10000
    
    
    pricetable$ln_Avg_Price <- log(pricetable$Avg_Price)
    pricetable$ln_HHI <- log(pricetable$HHI)
    
    
    
    
    
    reg_lnP<-lm(formula=ln_Avg_Price~HHI, data=pricetable)
    coeftable[nrow(coeftable)+1,] <-c(lambda0pct, NA, summary(reg_lnP)$coefficients["HHI","Estimate"], NA, mean(pricetable$Avg_Price), "lnPrice~HHI all HHI")
    reg_lnP<-lm(formula=ln_Avg_Price~HHI, data=pricetable, HHI<5000)
    coeftable[nrow(coeftable)+1,] <-c(lambda0pct, NA, summary(reg_lnP)$coefficients["HHI","Estimate"], NA, mean(pricetable[pricetable$HHI<5000,]$Avg_Price), "lnPrice~HHI, <5k HHI")
    
    reg_lnP_lnHHI<-lm(formula=ln_Avg_Price~ln_HHI, data=pricetable, HHI<5000)
    coeftable[nrow(coeftable)+1,] <-c(lambda0pct, summary(reg_lnP_lnHHI)$coefficients["ln_HHI","Estimate"], NA, NA, mean(pricetable[pricetable$HHI<5000,]$Avg_Price), "lnPrice~ln_HHI <5k HHI")
    reg_lnP_lnHHI<-lm(formula=ln_Avg_Price~ln_HHI, data=pricetable)
    coeftable[nrow(coeftable)+1,] <-c(lambda0pct, summary(reg_lnP_lnHHI)$coefficients["ln_HHI","Estimate"], NA, NA, mean(pricetable$Avg_Price), "lnPrice~ln_HHI all HHI")
    
    reg_P_HHI2<-lm(formula=Avg_Price~HHI+I(HHI**2), data=pricetable)
    coeftable[nrow(coeftable)+1,] <-c(lambda0pct, summary(reg_P_HHI2)$coefficients["(Intercept)","Estimate"], summary(reg_P_HHI2)$coefficients["HHI","Estimate"], summary(reg_P_HHI2)$coefficients["I(HHI^2)","Estimate"], mean(pricetable$Avg_Price), "Price~HHI^2 all HHI")
    reg_P_HHI2<-lm(formula=Avg_Price~HHI+I(HHI**2), data=pricetable, HHI<5000)
    coeftable[nrow(coeftable)+1,] <-c(lambda0pct, summary(reg_P_HHI2)$coefficients["(Intercept)","Estimate"], summary(reg_P_HHI2)$coefficients["HHI","Estimate"], summary(reg_P_HHI2)$coefficients["I(HHI^2)","Estimate"], mean(pricetable[pricetable$HHI<5000,]$Avg_Price), "Price~HHI^2 <5k HHI")
  }

  
  view_in_excel(coeftable)
  




reg2<-lm(formula=pricetable$Avg_Price~I(pricetable$HHI/10000)+I((pricetable$HHI/10000)**2))
print(summary(reg2))


# library(sjPlot)
# library(sjmisc)
# library(sjlabelled)
# tab_model(reg2)


plot(pricetable$HHI,pricetable$ln_Avg_Price, main="Scatterplot",
     xlab="HHI", ylab="Avg Price")


print(summary(lm(ln_Avg_Price~I(HHI/10000), data=pricetable,HHI<5000)))



plot(pricetable$HHI,pricetable$Avg_Price, main="Scatterplot",
     xlab="HHI", ylab="Avg Price")
mean(pricetable$Avg_Price)



reg1<-lm(formula=pricetable$Avg_Price~I(pricetable$HHI/10000))
print(summary(reg1))

plot(I(pricetable$HHI/10000),pricetable$Avg_Price, col="blue", main="Scatterplot",
  abline(reg1)
  ,cex = 0.5,pch = 16, lty=1, lwd=25,
  xlab="HHI", ylab="Avg Price")



library(ggplot2)

fit <- lm(Avg_Price ~ HHI + I(HHI^2), data = pricetable)
prd <- data.frame(HHI = seq(from = range(pricetable$HHI)[1], to = range(pricetable$HHI)[2], length.out = 100))
err <- predict(fit, newdata = prd, se.fit = TRUE)

prd$lci <- err$fit - 1.96 * err$se.fit
prd$fit <- err$fit
prd$uci <- err$fit + 1.96 * err$se.fit

ggplot(prd, aes(x = HHI, y = fit)) +
  theme_bw() +
  geom_line() +
  geom_smooth(aes(ymin = lci, ymax = uci), stat = "identity") +
  geom_point(data = pricetable, aes(x = HHI, y = Avg_Price))




