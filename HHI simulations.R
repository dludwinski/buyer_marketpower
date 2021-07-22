
test<-(lambda_0_pct)/(1-alpha*(1-lambda_0_pct))

curve((x-1)/(x-1+test), 1, 100, n = 101, add = FALSE, type = "l",
      ylab = NULL, log = 'x')

curve((x-1)/(x+1), 1, 100, n = 101, add = TRUE, type = "l",
      ylab = NULL, log = 'x')
plot.function(fn, from = 0, to = 1, n = 101)


# HHI Simulation - by L 0 -------------------------------------------------
  set.seed(20210705)
  set.seed(NULL)

  #HHI Simulation (just random MS)
  alpha <- 0.8
  
  pricetable10 <- data.frame("lambda_pct"=double(),"med_pct"=double(),"med_price"=double(),
                              "l1"=double(), "l2"=double(), "l3"=double(), "l4"=double(), "l5"=double(), "l6"=double(), "l7"=double(), "l8"=double(), "l9"=double(), "l10"=double(),
                              "p1"=double(), "p2"=double(), "p3"=double(), "p4"=double(), "p5"=double(), "p6"=double(), "p7"=double(), "p8"=double(), "p9"=double(), "p10"=double()
                             , "inflator"=double()
    )
  
  
  #med_pct <- 0
  #med_price <- -99999
  med_pct <- 0
  med_price <- 0
  #lambda_list <- list(0.15, 0.20, 0.25, 0.30)
  lambda_list <- list(0.10, 0.15, 0.20, 0.25, 0.30, 0.35)
  
  for(i in 1:500) {
    #To make sure I get the higher HHIs
    r1<-1000
    r2<-sample(1:r1, 1)
    r3<-sample(1:r2, 1)
    r4<-sample(1:r3, 1)
    r5<-sample(1:r4, 1)
    r6<-sample(1:r5, 1) 
    r7<-sample(1:r6, 1)
    r8<-sample(1:r7, 1)
    r9<-sample(1:r8, 1)
    r10<-sample(1:r9, 1)
    
    # loop over lambdas 
    for (lambda0pct in lambda_list){
      a<-cbind(lambda0pct, med_pct, med_price, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10)
      pricetable10[nrow(pricetable10)+1,] <-  cbind(a, 
                                                      prices_10_med(alpha,pm=a[3],lambda_m_pct=a[2],lambda_0_pct=a[1],a[4],a[5],a[6],a[7],a[8],a[9],a[10],a[11],a[12],a[13])
      )
    }
  }
  
  for(i in 1:5) {
    r1<-runif(1, 0, 1000) 
    r2<-runif(1, 0, 1000) 
    r3<-runif(1, 0, 1000)
    r4<-runif(1, 0, 1000)
    r5<-runif(1, 0, 1000)
    r6<-runif(1, 0, 1000) 
    r7<-runif(1, 0, 1000)
    r8<-runif(1, 0, 1000)
    r9<-runif(1, 0, 1000)
    r10<-runif(1, 0, 1000)
    
    
    # loop over lambdas 
    for (lambda0pct in lambda_list){
      a<-cbind(lambda0pct, med_pct, med_price, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10)
      pricetable10[nrow(pricetable10)+1,] <-  cbind(a, 
                                                      prices_10_med(alpha,pm=a[3],lambda_m_pct=a[2],lambda_0_pct=a[1],a[4],a[5],a[6],a[7],a[8],a[9],a[10],a[11],a[12],a[13])
      )
    }
  }
  
  #Calculate market shares and average prices
    pricetable10$MS1<-100*pricetable10$l1/(pricetable10$l1+pricetable10$l2+pricetable10$l3+pricetable10$l4+pricetable10$l5+pricetable10$l6+pricetable10$l7+pricetable10$l8+pricetable10$l9+pricetable10$l10)
    pricetable10$MS2<-100*pricetable10$l2/(pricetable10$l1+pricetable10$l2+pricetable10$l3+pricetable10$l4+pricetable10$l5+pricetable10$l6+pricetable10$l7+pricetable10$l8+pricetable10$l9+pricetable10$l10)
    pricetable10$MS3<-100*pricetable10$l3/(pricetable10$l1+pricetable10$l2+pricetable10$l3+pricetable10$l4+pricetable10$l5+pricetable10$l6+pricetable10$l7+pricetable10$l8+pricetable10$l9+pricetable10$l10)
    pricetable10$MS4<-100*pricetable10$l4/(pricetable10$l1+pricetable10$l2+pricetable10$l3+pricetable10$l4+pricetable10$l5+pricetable10$l6+pricetable10$l7+pricetable10$l8+pricetable10$l9+pricetable10$l10)
    pricetable10$MS5<-100*pricetable10$l5/(pricetable10$l1+pricetable10$l2+pricetable10$l3+pricetable10$l4+pricetable10$l5+pricetable10$l6+pricetable10$l7+pricetable10$l8+pricetable10$l9+pricetable10$l10)
    pricetable10$MS6<-100*pricetable10$l6/(pricetable10$l1+pricetable10$l2+pricetable10$l3+pricetable10$l4+pricetable10$l5+pricetable10$l6+pricetable10$l7+pricetable10$l8+pricetable10$l9+pricetable10$l10)
    pricetable10$MS7<-100*pricetable10$l7/(pricetable10$l1+pricetable10$l2+pricetable10$l3+pricetable10$l4+pricetable10$l5+pricetable10$l6+pricetable10$l7+pricetable10$l8+pricetable10$l9+pricetable10$l10)
    pricetable10$MS8<-100*pricetable10$l8/(pricetable10$l1+pricetable10$l2+pricetable10$l3+pricetable10$l4+pricetable10$l5+pricetable10$l6+pricetable10$l7+pricetable10$l8+pricetable10$l9+pricetable10$l10)
    pricetable10$MS9<-100*pricetable10$l9/(pricetable10$l1+pricetable10$l2+pricetable10$l3+pricetable10$l4+pricetable10$l5+pricetable10$l6+pricetable10$l7+pricetable10$l8+pricetable10$l9+pricetable10$l10)
    pricetable10$MS10<-100*pricetable10$l10/(pricetable10$l1+pricetable10$l2+pricetable10$l3+pricetable10$l4+pricetable10$l5+pricetable10$l6+pricetable10$l7+pricetable10$l8+pricetable10$l9+pricetable10$l10)
    
    pricetable10$Avg_Price <- (pricetable10$MS1*pricetable10$p1+pricetable10$MS2*pricetable10$p2+pricetable10$MS3*pricetable10$p3+pricetable10$MS4*pricetable10$p4+pricetable10$MS5*pricetable10$p5+pricetable10$MS6*pricetable10$p6+pricetable10$MS7*pricetable10$p7 +pricetable10$MS8*pricetable10$p8 +pricetable10$MS9*pricetable10$p9 +pricetable10$MS10*pricetable10$p10)/(pricetable10$MS1+pricetable10$MS2+pricetable10$MS3+pricetable10$MS4+pricetable10$MS5+pricetable10$MS6+pricetable10$MS7+pricetable10$MS8+pricetable10$MS9+pricetable10$MS10)
    
    pricetable10$Markup <- pricetable10$Avg_Price / ((1-alpha)*10+alpha* pricetable10$med_price* pricetable10$med_pct*(1- pricetable10$lambda_pct)/(  pricetable10$med_pct*(1- pricetable10$lambda_pct)+ pricetable10$lambda_pct))
    
    pricetable10$HHI <- (pricetable10$MS1^2+pricetable10$MS2^2+pricetable10$MS3^2+pricetable10$MS4^2+pricetable10$MS5^2+pricetable10$MS6^2+pricetable10$MS7^2+pricetable10$MS8^2+pricetable10$MS9^2+pricetable10$MS10^2)
    
    pricetable10$ln_Avg_Price <- log(pricetable10$Avg_Price)
    pricetable10$ln_HHI <- log(pricetable10$HHI)
    
    
    pricetable10$price_hhi <- price_hhi(alpha,pricetable10$med_price,pricetable10$med_pct,pricetable10$lambda_pct,pricetable10$HHI)
    pricetable10$HHI_n <- 1/(pricetable10$HHI/10000)
    
    pricetable10$Avg_HHI_Rat <- pricetable10$Avg_Price/pricetable10$price_hhi
    
    ggplot(pricetable10, aes(x=HHI_n, y=Avg_HHI_Rat)) + geom_point() +
      geom_point(aes(col=lambda_pct), size=3) 
    
    ggplot(pricetable10, aes(x=Avg_Price, y=price_hhi)) + geom_point() +
      geom_point(aes(col=lambda_pct), size=3) 
    
    
  #Graphing
    
    #Set colors and plot
    pricetable10$color<-"gray0" #0.10
    pricetable10[pricetable10$lambda_pct==0.15, c("color")] <- "gray22"
    pricetable10[pricetable10$lambda_pct==0.20, c("color")] <- "gray40"
    pricetable10[pricetable10$lambda_pct==0.25, c("color")] <- "gray56"
    pricetable10[pricetable10$lambda_pct==0.30, c("color")] <- "gray72"
    pricetable10[pricetable10$lambda_pct==0.35, c("color")] <- "gray90"
    
    pricetable10$color<-"gray0" #0.15
    pricetable10[pricetable10$lambda_pct==0.20, c("color")] <- "gray40"
    pricetable10[pricetable10$lambda_pct==0.25, c("color")] <- "gray70"
    pricetable10[pricetable10$lambda_pct==0.30, c("color")] <- "gray90"
    
    
    
    #Sorted for order on graph
    #pricetable10 <- pricetable10[order(-pricetable10$lambda_pct),]
    
    png(file="Price and HHI - by lambda.png", width=1000, height=750)
    
    plot(pricetable10[pricetable10$HHI<10000 & pricetable10$lambda_pct > 0.1 & pricetable10$lambda_pct < 0.35 ,]$HHI,pricetable10[pricetable10$HHI<10000 & pricetable10$lambda_pct > 0.1 & pricetable10$lambda_pct < 0.35 ,]$Markup)
    
    plot(pricetable10[pricetable10$HHI<10000 & pricetable10$lambda_pct > 0.1 & pricetable10$lambda_pct < 0.35 ,]$HHI,pricetable10[pricetable10$HHI<10000 & pricetable10$lambda_pct > 0.1 & pricetable10$lambda_pct < 0.35 ,]$Markup
         , main= expression(paste("Price and HHI: by ", gamma[0])), cex.main = 2.5
         ,pch = 16
         , col=pricetable10[pricetable10$HHI<10000 & pricetable10$lambda_pct > 0.1 & pricetable10$lambda_pct < 0.35 ,]$color
         , cex.lab = 1.5, xlab="MCO Herfindahl-Hirschman Index (HHI)", ylab="Average MCO price / Monopsonist Price"
         , plot.window(xlim=c(1000,10000), ylim=c(1.4,3.1), xaxs="i", yaxs="i")
         , cex.axis	= 1.1
    )
    grid(nx = NULL, ny = NULL, col = "lightgray", lty = 1,lwd = par("lwd"), equilogs = FALSE)
    points(pricetable10[pricetable10$HHI<10000 & pricetable10$lambda_pct > 0.1 & pricetable10$lambda_pct < 0.35 ,]$HHI,pricetable10[pricetable10$HHI<10000 & pricetable10$lambda_pct > 0.1 & pricetable10$lambda_pct < 0.35 ,]$Markup
           ,pch = 16
         ,col=pricetable10[pricetable10$HHI<10000 & pricetable10$lambda_pct > 0.1 & pricetable10$lambda_pct < 0.35 ,]$color
         , cex.lab = 1.5, xlab="MCO Herfindahl-Hirschman Index (HHI)", ylab="Average MCO price / Monopsonist Price"
    ) 
    
    legend(6400,3, legend=c(expression(paste(gamma[0], " = 15%") ), expression(paste(gamma[0], " = 20%") ), expression(paste(gamma[0], " = 25%") ), expression(paste(gamma[0], " = 30%") )),
           col=c("gray0", "gray40", "gray70", "gray90"), lty=1, lwd=8, cex=1.3
           ,box.lty=0)
    
    dev.off()
    
  
  
  # Coefficients from regressions
  coeftable <- data.frame("lambda_pct"=double(),
                          "med_pct"=double(),
                          "ln_HHI_Coef"=double(),
                          "HHI_Coef"=double(),
                          "HHI_sq_Coef"=double(),
                          "Avg_Price"=double(),
                          "Note"=character(),
                          stringsAsFactors=FALSE) 
  
  
  for (lambda0pct in lambda_list){
    print(lambda0pct)
  
    reg_lnP<-lm(formula=ln_Avg_Price~HHI, data=pricetable10[pricetable10$lambda_pct==lambda0pct,])
    coeftable[nrow(coeftable)+1,] <-c(lambda0pct, med_pct, NA, summary(reg_lnP)$coefficients["HHI","Estimate"], NA, mean(pricetable10[pricetable10$lambda_pct==lambda0pct,]$Avg_Price), "lnPrice~HHI all HHI")
    reg_lnP<-lm(formula=ln_Avg_Price~HHI, data=pricetable10[pricetable10$lambda_pct==lambda0pct,], HHI<5000)
    coeftable[nrow(coeftable)+1,] <-c(lambda0pct, med_pct, NA, summary(reg_lnP)$coefficients["HHI","Estimate"], NA, mean(pricetable10[pricetable10$HHI<5000 & pricetable10$lambda_pct==lambda0pct,]$Avg_Price), "lnPrice~HHI, <5k HHI")
    
    reg_lnP_lnHHI<-lm(formula=ln_Avg_Price~ln_HHI, data=pricetable10[pricetable10$lambda_pct==lambda0pct,])
    coeftable[nrow(coeftable)+1,] <-c(lambda0pct, med_pct, summary(reg_lnP_lnHHI)$coefficients["ln_HHI","Estimate"], NA, NA, mean(pricetable10[pricetable10$lambda_pct==lambda0pct,]$Avg_Price), "lnPrice~ln_HHI all HHI")
    reg_lnP_lnHHI<-lm(formula=ln_Avg_Price~ln_HHI, data=pricetable10[pricetable10$lambda_pct==lambda0pct,], HHI<5000)
    coeftable[nrow(coeftable)+1,] <-c(lambda0pct, med_pct, summary(reg_lnP_lnHHI)$coefficients["ln_HHI","Estimate"], NA, NA, mean(pricetable10[pricetable10$HHI<5000 & pricetable10$lambda_pct==lambda0pct,]$Avg_Price), "lnPrice~ln_HHI <5k HHI")
    
    reg_P_HHI2<-lm(formula=Avg_Price~HHI+I(HHI**2), data=pricetable10[pricetable10$lambda_pct==lambda0pct,])
    coeftable[nrow(coeftable)+1,] <-c(lambda0pct, med_pct, summary(reg_P_HHI2)$coefficients["(Intercept)","Estimate"], summary(reg_P_HHI2)$coefficients["HHI","Estimate"], summary(reg_P_HHI2)$coefficients["I(HHI^2)","Estimate"], mean(pricetable10[pricetable10$lambda_pct==lambda0pct,]$Avg_Price), "Price~HHI^2 all HHI")
    reg_P_HHI2<-lm(formula=Avg_Price~HHI+I(HHI**2), data=pricetable10[pricetable10$lambda_pct==lambda0pct,], HHI<5000)
    coeftable[nrow(coeftable)+1,] <-c(lambda0pct, med_pct, summary(reg_P_HHI2)$coefficients["(Intercept)","Estimate"], summary(reg_P_HHI2)$coefficients["HHI","Estimate"], summary(reg_P_HHI2)$coefficients["I(HHI^2)","Estimate"], mean(pricetable10[pricetable10$HHI<5000 & pricetable10$lambda_pct==lambda0pct,]$Avg_Price), "Price~HHI^2 <5k HHI")

  }
  
  
  #view_in_excel(coeftable) 
  
               
               
               
               
 # HHI Simulation - by Public pop 0 -------------------------------------------------
  
  pricetable10p <- data.frame("lambda_pct"=double(),"med_pct"=double(),"med_price"=double(),
                              "l1"=double(), "l2"=double(), "l3"=double(), "l4"=double(), "l5"=double(), "l6"=double(), "l7"=double(), "l8"=double(), "l9"=double(), "l10"=double(),
                              "p1"=double(), "p2"=double(), "p3"=double(), "p4"=double(), "p5"=double(), "p6"=double(), "p7"=double(), "p8"=double(), "p9"=double(), "p10"=double()
                              , "inflator"=double()
  )
  
  med_price <- 2
  lambda0pct <- 0.2
  alpha <- 0.8
  
  for(i in 1:500) {
    r1<-1000
    r2<-sample(1:r1, 1)
    # r3<-0
    # r4<-0
    # r5<-0
    # r6<-0 
    # r7<-0
    # r8<-0
    # r9<-0
    # r10<-0
    
    r3<-sample(1:r2, 1)
    r4<-sample(1:r3, 1)
    r5<-sample(1:r4, 1)
    r6<-sample(1:r5, 1)
    r7<-sample(1:r6, 1)
    r8<-sample(1:r7, 1)
    r9<-sample(1:r8, 1)
    r10<-sample(1:r9, 1)
    
    med_list <- list(0.45, 0.35, 0.25, 0.15, 0.0)
    
    # loop over med prices 
    for (med_pct in med_list){
      
      a<-cbind(lambda0pct, med_pct, med_price, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10)
      
      pricetable10p[nrow(pricetable10p)+1,] <-  cbind(a, 
                                                      prices_10_med(alpha,pm=a[3],lambda_m_pct=a[2],lambda_0_pct=a[1],a[4],a[5],a[6],a[7],a[8],a[9],a[10],a[11],a[12],a[13])
      )
    }
  }
  for(i in 1:100) {
    r1<-runif(1, 0, 1000) 
    r2<-runif(1, 0, 1000) 
    r3<-runif(1, 0, 1000)
    r4<-runif(1, 0, 1000)
    r5<-runif(1, 0, 1000)
    r6<-runif(1, 0, 1000) 
    r7<-runif(1, 0, 1000)
    r8<-runif(1, 0, 1000)
    r9<-runif(1, 0, 1000)
    r10<-runif(1, 0, 1000)
    
    
    #med_list <- list(0.45, 0.35, 0.25, 0.15, 0.0)
    
    # loop over med prices 
    for (med_pct in med_list){
      
      a<-cbind(lambda0pct, med_pct, med_price, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10)
      
      pricetable10p[nrow(pricetable10p)+1,] <-  cbind(a, 
                                                      prices_10_med(alpha,pm=a[3],lambda_m_pct=a[2],lambda_0_pct=a[1],a[4],a[5],a[6],a[7],a[8],a[9],a[10],a[11],a[12],a[13])
      )
    }
  }
  
  
  #Calculate market shares and average prices
    pricetable10p$MS1<-pricetable10p$l1/(pricetable10p$l1+pricetable10p$l2+pricetable10p$l3+pricetable10p$l4+pricetable10p$l5+pricetable10p$l6+pricetable10p$l7+pricetable10p$l8+pricetable10p$l9+pricetable10p$l10)
    pricetable10p$MS2<-pricetable10p$l2/(pricetable10p$l1+pricetable10p$l2+pricetable10p$l3+pricetable10p$l4+pricetable10p$l5+pricetable10p$l6+pricetable10p$l7+pricetable10p$l8+pricetable10p$l9+pricetable10p$l10)
    pricetable10p$MS3<-pricetable10p$l3/(pricetable10p$l1+pricetable10p$l2+pricetable10p$l3+pricetable10p$l4+pricetable10p$l5+pricetable10p$l6+pricetable10p$l7+pricetable10p$l8+pricetable10p$l9+pricetable10p$l10)
    pricetable10p$MS4<-pricetable10p$l4/(pricetable10p$l1+pricetable10p$l2+pricetable10p$l3+pricetable10p$l4+pricetable10p$l5+pricetable10p$l6+pricetable10p$l7+pricetable10p$l8+pricetable10p$l9+pricetable10p$l10)
    pricetable10p$MS5<-pricetable10p$l5/(pricetable10p$l1+pricetable10p$l2+pricetable10p$l3+pricetable10p$l4+pricetable10p$l5+pricetable10p$l6+pricetable10p$l7+pricetable10p$l8+pricetable10p$l9+pricetable10p$l10)
    pricetable10p$MS6<-pricetable10p$l6/(pricetable10p$l1+pricetable10p$l2+pricetable10p$l3+pricetable10p$l4+pricetable10p$l5+pricetable10p$l6+pricetable10p$l7+pricetable10p$l8+pricetable10p$l9+pricetable10p$l10)
    pricetable10p$MS7<-pricetable10p$l7/(pricetable10p$l1+pricetable10p$l2+pricetable10p$l3+pricetable10p$l4+pricetable10p$l5+pricetable10p$l6+pricetable10p$l7+pricetable10p$l8+pricetable10p$l9+pricetable10p$l10)
    pricetable10p$MS8<-pricetable10p$l8/(pricetable10p$l1+pricetable10p$l2+pricetable10p$l3+pricetable10p$l4+pricetable10p$l5+pricetable10p$l6+pricetable10p$l7+pricetable10p$l8+pricetable10p$l9+pricetable10p$l10)
    pricetable10p$MS9<-pricetable10p$l9/(pricetable10p$l1+pricetable10p$l2+pricetable10p$l3+pricetable10p$l4+pricetable10p$l5+pricetable10p$l6+pricetable10p$l7+pricetable10p$l8+pricetable10p$l9+pricetable10p$l10)
    pricetable10p$MS10<-pricetable10p$l10/(pricetable10p$l1+pricetable10p$l2+pricetable10p$l3+pricetable10p$l4+pricetable10p$l5+pricetable10p$l6+pricetable10p$l7+pricetable10p$l8+pricetable10p$l9+pricetable10p$l10)
    
    pricetable10p$Avg_Price <- (pricetable10p$MS1*pricetable10p$p1+pricetable10p$MS2*pricetable10p$p2+pricetable10p$MS3*pricetable10p$p3+pricetable10p$MS4*pricetable10p$p4+pricetable10p$MS5*pricetable10p$p5+pricetable10p$MS6*pricetable10p$p6+pricetable10p$MS7*pricetable10p$p7 +pricetable10p$MS8*pricetable10p$p8 +pricetable10p$MS9*pricetable10p$p9 +pricetable10p$MS10*pricetable10p$p10)/(pricetable10p$MS1+pricetable10p$MS2+pricetable10p$MS3+pricetable10p$MS4+pricetable10p$MS5+pricetable10p$MS6+pricetable10p$MS7+pricetable10p$MS8+pricetable10p$MS9+pricetable10p$MS10)
    
    pricetable10p$HHI <- (pricetable10p$MS1^2+pricetable10p$MS2^2+pricetable10p$MS3^2+pricetable10p$MS4^2+pricetable10p$MS5^2+pricetable10p$MS6^2+pricetable10p$MS7^2+pricetable10p$MS8^2+pricetable10p$MS9^2+pricetable10p$MS10^2)*10000
    
    pricetable10p$Markup <- pricetable10p$Avg_Price / ((1-alpha)*10+alpha* pricetable10p$med_price* pricetable10p$med_pct*(1- pricetable10p$lambda_pct)/(  pricetable10p$med_pct*(1- pricetable10p$lambda_pct)+ pricetable10p$lambda_pct))
    
    
    pricetable10p$ln_Avg_Price <- log(pricetable10p$Avg_Price)
    pricetable10p$ln_HHI <- log(pricetable10p$HHI)
    
    
    pricetable10p$price_hhi <- price_hhi(alpha,med_price,pricetable10p$med_pct,pricetable10p$lambda_pct,pricetable10p$HHI)
    
    pricetable10p$HHI_n <- 1/(pricetable10p$HHI/10000)
    
    pricetable10p$Avg_HHI_Rat <- pricetable10p$Avg_Price/pricetable10p$price_hhi
    
    ggplot(pricetable10p, aes(x=HHI_n, y=Avg_HHI_Rat)) + geom_point() +
      geom_point(aes(col=med_pct), size=3) 
    
    
    ggplot(pricetable10p, aes(x=Avg_Price, y=price_hhi)) + geom_point() +
      geom_point(aes(col=med_pct), size=3) 
    
    
    ggplot(pricetable10p, aes(x=HHI, y=Avg_Price)) + geom_point() +
      geom_point(aes(col=med_pct), size=3) 
    
    
    ggplot(pricetable10p[pricetable10p$HHI<5000,], aes(x=HHI, y=Avg_Price)) + geom_point() +
      geom_point(aes(col=med_pct), size=3) 
    
  
  #Graphing
    
    #Set colors and plot
    pricetable10p$color<-"gray0" #med_pct==0
    pricetable10p[pricetable10p$med_pct==0.15, c("color")] <- "gray30"
    pricetable10p[pricetable10p$med_pct==0.25, c("color")] <- "gray55"
    pricetable10p[pricetable10p$med_pct==0.35, c("color")] <- "gray75"
    pricetable10p[pricetable10p$med_pct==0.45, c("color")] <- "gray90"
    
    
    #Order determines plot order
    pricetable10p <- pricetable10p[order(-pricetable10p$med_pct),]
    
    png(file="Price and HHI - by public.png", width=1000, height=750)
    
    plot(pricetable10p[pricetable10p$HHI<10000,]$HHI,pricetable10p[pricetable10p$HHI<10000,]$Avg_Price
         , main="Price and HHI: By public share", cex.main = 2
         , xlab="MCO Herfindahl-Hirschman Index (HHI)", ylab="Average MCO price / Monopsonist Price"
         #, plot.window(xlim=c(1000,10000), ylim=c(1,2.7), xaxs="i", yaxs="i")
    )
    grid(nx = NULL, ny = NULL, col = "lightgray", lty = 1,lwd = par("lwd"), equilogs = FALSE)
    points(pricetable10p[pricetable10p$HHI<10000,]$HHI,pricetable10p[pricetable10p$HHI<10000,]$Avg_Price
           ,cex = 1.2,pch = 16
           ,col=pricetable10p$color
    ) 
    
    legend(6400,2.6, legend=c(expression(paste(Med, " = 0%") ), expression(paste(Med, " = 10%") ), expression(paste(Med, " = 20%") ), expression(paste(Med, " = 30%") ), expression(paste(Med, " = 40%") ) ),
           col=c("gray0", "gray30", "gray55", "gray75", "gray90"), lty=1, lwd=8, cex=1
           ,box.lty=0)
    
    dev.off()
  
  #Why do they intersect around 5000?
  #View(pricetable10p[pricetable10p$HHI>4990 & pricetable10p$HHI<5010,])
  
  
  #  coeftable <- data.frame("lambda_pct"=double(), "med_pct"=double(), "ln_HHI_Coef"=double(), "HHI_Coef"=double(), "HHI_sq_Coef"=double(), "Avg_Price"=double(), "Note"=character(), stringsAsFactors=FALSE) 
  
  
  # Coefficients from some regressions
    for (med_pct in med_list){
      print(med_pct)
      
      reg_lnP<-lm(formula=ln_Avg_Price~HHI, data=pricetable10p[pricetable10p$med_pct==med_pct,])
      coeftable[nrow(coeftable)+1,] <-c(lambda0pct, med_pct, NA, summary(reg_lnP)$coefficients["HHI","Estimate"], NA, mean(pricetable10[med_pct==med_pct,]$Avg_Price), "lnPrice~HHI all HHI")
      reg_lnP<-lm(formula=ln_Avg_Price~HHI, data=pricetable10p[pricetable10p$med_pct==med_pct,], HHI<5000)
      coeftable[nrow(coeftable)+1,] <-c(lambda0pct, med_pct, NA, summary(reg_lnP)$coefficients["HHI","Estimate"], NA, mean(pricetable10p[pricetable10p$HHI<5000 & pricetable10p$med_pct==med_pct,]$Avg_Price), "lnPrice~HHI, <5k HHI")
      
      reg_lnP_lnHHI<-lm(formula=ln_Avg_Price~ln_HHI, data=pricetable10p[pricetable10p$med_pct==med_pct,])
      coeftable[nrow(coeftable)+1,] <-c(lambda0pct, med_pct, summary(reg_lnP_lnHHI)$coefficients["ln_HHI","Estimate"], NA, NA, mean(pricetable10[med_pct==med_pct,]$Avg_Price), "lnPrice~ln_HHI all HHI")
      reg_lnP_lnHHI<-lm(formula=ln_Avg_Price~ln_HHI, data=pricetable10p[pricetable10p$med_pct==med_pct,], HHI<5000)
      coeftable[nrow(coeftable)+1,] <-c(lambda0pct, med_pct, summary(reg_lnP_lnHHI)$coefficients["ln_HHI","Estimate"], NA, NA, mean(pricetable10p[pricetable10p$HHI<5000 & pricetable10p$med_pct==med_pct,]$Avg_Price), "lnPrice~ln_HHI <5k HHI")
      
      reg_P_HHI2<-lm(formula=Avg_Price~HHI+I(HHI**2), data=pricetable10p[pricetable10p$med_pct==med_pct,])
      coeftable[nrow(coeftable)+1,] <-c(lambda0pct, med_pct, summary(reg_P_HHI2)$coefficients["(Intercept)","Estimate"], summary(reg_P_HHI2)$coefficients["HHI","Estimate"], summary(reg_P_HHI2)$coefficients["I(HHI^2)","Estimate"], mean(pricetable10[med_pct==med_pct,]$Avg_Price), "Price~HHI^2 all HHI")
      reg_P_HHI2<-lm(formula=Avg_Price~HHI+I(HHI**2), data=pricetable10p[pricetable10p$med_pct==med_pct,], HHI<5000)
      coeftable[nrow(coeftable)+1,] <-c(lambda0pct, med_pct, summary(reg_P_HHI2)$coefficients["(Intercept)","Estimate"], summary(reg_P_HHI2)$coefficients["HHI","Estimate"], summary(reg_P_HHI2)$coefficients["I(HHI^2)","Estimate"], mean(pricetable10p[pricetable10p$HHI<5000 & pricetable10p$med_pct==med_pct,]$Avg_Price), "Price~HHI^2 <5k HHI")
      
    }
  
    view_in_excel(coeftable) 
    