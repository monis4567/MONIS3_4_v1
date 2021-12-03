  #!/usr/bin/env Rscript
  # -*- coding: utf-8 -*-
  
  
  #################################################################################
  # Code developed by Steen W. Knudsen at NIVA-DK
  
  # The 20 non indeginuous marine species targeted for analysis are the same 20 
  # species that the MONIS2, MONIS3 and MONIS4 project, carried out at NIVA-DK
  #
  # The overall purpose of the present code is to analyse the eDNA levels inferred
  # in the qPCR setups performed over 2017-2018.
  #################################################################################
  #remove everything in the working environment, 
  #without a warning!!
  rm(list=ls())
  
  #install packages
  if(!require(magrittr)){
    install.packages("magrittr")
  }
  if(!require(dplyr)){
    install.packages("dplyr")
  }
  library(magrittr) # needs to be run every time you start R and want to use %>%
  library(dplyr)    # alternatively, this also loads %>%
  
  wd00 <- "/home/hal9000/Documents/Documents/NIVA_Ansaettelse_2020/MS_eDNA_MONIS3_4/MONIS3_4_v1"
  # set working directory
  wd10 <- "suppmatr10_plots_and_tables_from_Rcode"
  wd09 <- "suppmatr09_Rcodes_for_analysing_qPCR_eDNA_data"
  wd08 <- "suppmatr08_merged_qPCRdata_txt_reports"
  #paste dirs together
  wd <- paste0(wd00,wd09)
  setwd(wd)
  getwd()
  
  #define path and file name for csv file to read in
  #the input file prepared with the previous code
  in.csv.file01 <- paste(wd00,wd10,"/suppmatr_10.12c_MONIS4eDNA02_df.csv",sep="")
  #read in csv file from previous code
  df_MONIS4_02 <-as.data.frame(read.csv(in.csv.file01,
                                        header = TRUE, sep = ",", quote = "\"",
                                        dec = ".", fill = TRUE, comment.char = "", stringsAsFactors = FALSE))
  #add a column indicating if the copies are above zero, above LOD
  # above LOQ
  df_MONIS4_02$edna_abzero <- (df_MONIS4_02$meanQuantitycopies>0)*1
  df_MONIS4_02$edna_abLOD <- (df_MONIS4_02$meanQuantitycopies>df_MONIS4_02$LOD)*1
  df_MONIS4_02$edna_abLOQ <- (df_MONIS4_02$meanQuantitycopies>df_MONIS4_02$LOQ)*1
  #add a column indicating if conventional monitoring
  # in the present survey or in the past has found the species
  df_MONIS4_02$conv_rec_val2 <- (df_MONIS4_02$conv_rec_val>0)*1
  #use dplyr to count by group
  # see this example: https://www.miskatonic.org/2012/09/24/counting-and-aggregating-in-r/
  #count for harbours above zero
  tibl_abze <- df_MONIS4_02 %>% dplyr::group_by(Harbour.season) %>% dplyr::count(edna_abzero)
  #count for harbours above LOD
  tibl_abLD <- df_MONIS4_02 %>% dplyr::group_by(Harbour.season) %>% dplyr::count(edna_abLOD)
  #count for harbours above LOQ
  tibl_abLQ <- df_MONIS4_02 %>% dplyr::group_by(Harbour.season) %>% dplyr::count(edna_abLOQ)
  #count for harbours by conventional monitoring
  tibl_cvr2 <- df_MONIS4_02 %>% dplyr::group_by(Harbour.season) %>% dplyr::count(conv_rec_val2)
  #subset the tibbles by only including rows that had recorded
  #presence of the species, and make them data frames
  df_abze <-as.data.frame(subset(tibl_abze,edna_abzero==1))
  df_abLD <-as.data.frame(subset(tibl_abLD,edna_abLOD==1))
  df_abLQ <-as.data.frame(subset(tibl_abLQ,edna_abLOQ==1))
  df_cvr2 <-as.data.frame(subset(tibl_cvr2,conv_rec_val2==1))
  #copy data frame
  df_MO4_10 <- df_abze
  #match to data frame
  df_MO4_10$cnt_eDabze <- df_abze$n[match(df_MO4_10$Harbour.season, df_abze$Harbour.season)]
  #remove columns not needed
  df_MO4_10$n <- NULL
  df_MO4_10$edna_abzero <- NULL
  #match counts back to data frame
  df_MO4_10$cnt_eDabLD <- df_abLD$n[match(df_MO4_10$Harbour.season, df_abLD$Harbour.season)]
  df_MO4_10$cnt_eDabLQ <- df_abLQ$n[match(df_MO4_10$Harbour.season, df_abLQ$Harbour.season)]
  df_MO4_10$cnt_cvr2 <- df_cvr2$n[match(df_MO4_10$Harbour.season, df_cvr2$Harbour.season)]
  #replace any NAs with zeroes
  df_MO4_10[is.na(df_MO4_10)] <- 0
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  # Plot linear regression models that compares conventional monitoring and eDNA monitoring  - start
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #see this website:  http://r-statistics.co/Linear-Regression.html
  #plot conventional monitoring against eDNA monitoring 
  dev.off()
  #for both seasons
  plot(df_MO4_10$cnt_cvr2,df_MO4_10$cnt_eDabze)
  plot(df_MO4_10$cnt_cvr2,df_MO4_10$cnt_eDabLD)
  plot(df_MO4_10$cnt_cvr2,df_MO4_10$cnt_eDabLQ)
  #rename the variable
  no.spc.conv <- df_MO4_10$cnt_cvr2
  no.spc.abzero <- df_MO4_10$cnt_eDabze
  no.spc.abLOD <- df_MO4_10$cnt_eDabLD
  no.spc.abLOQ <- df_MO4_10$cnt_eDabLQ
  #try with a smooth curve
  #scatter.smooth(x=no.spc.conv, y=no.spc.abzero, main="no.spc.abzero ~ no.spc.conv")  # scatterplot
  #dev.off()
  
  #BoxPlot – Check for outliers
  par(mfrow=c(1, 2))  # divide graph area in 2 columns
  boxplot(no.spc.conv, main="no.spc.conv", sub=paste("Outlier rows: ", boxplot.stats(no.spc.conv)$out))  # box plot for 'no.spc.conv'
  boxplot(no.spc.abzero, main="no.spc.abzero", sub=paste("Outlier rows: ", boxplot.stats(no.spc.abzero)$out))  # box plot for 'no.spc.abzero'
  #dev.off()
  
  #Density plot – Check if the response variable is close to normality
  library(e1071)
  par(mfrow=c(4, 1))  # divide graph area in 1 column and 4 rows
  # density plot for 'no.spc.conv'
  plot(density(no.spc.conv), main="Density Plot: no.spc.conv", ylab="Frequency", 
       sub=paste("Skewness:", round(e1071::skewness(no.spc.conv), 2)))  
  polygon(density(no.spc.conv), col="red")
  # density plot for 'no.spc.abzero'
  plot(density(no.spc.abzero), main="Density Plot: Sno.spc.abzero", ylab="Frequency", 
       sub=paste("Skewness:", round(e1071::skewness(no.spc.abzero), 2)))  
  polygon(density(no.spc.abzero), col="blue")
  # density plot for 'no.spc.abLOD'
  plot(density(no.spc.abLOD), main="Density Plot: no.spc.abLOD", ylab="Frequency", 
       sub=paste("Skewness:", round(e1071::skewness(no.spc.abLOD), 2)))  
  polygon(density(no.spc.abLOD), col="blue")
  # density plot for 'no.spc.abLOQ'
  plot(density(no.spc.abLOQ), main="Density Plot: no.spc.abLOQ", ylab="Frequency", 
       sub=paste("Skewness:", round(e1071::skewness(no.spc.abLOQ), 2)))  
  polygon(density(no.spc.abLOQ), col="blue")
  #log 10 transform
  l10.no.spc.conv <- log10(no.spc.conv) 
  l10.no.spc.abzero <- log10(no.spc.abzero)
  l10.no.spc.abLOD <- log10(no.spc.abLOD)
  l10.no.spc.abLOQ <- log10(no.spc.abLOQ)
  #par(mfrow=c(4, 1))  # divide graph area in 1 column and 4 rows
  # density plot for 'no.spc.conv'
  plot(density(l10.no.spc.conv), main="Density Plot: l10.no.spc.conv", ylab="Frequency", 
       sub=paste("Skewness:", round(e1071::skewness(l10.no.spc.conv), 2)))  
  polygon(density(l10.no.spc.conv), col="red")
  # density plot for 'l10.no.spc.abzero'
  plot(density(l10.no.spc.abzero), main="Density Plot: l10.no.spc.abzero", ylab="Frequency", 
       sub=paste("Skewness:", round(e1071::skewness(l10.no.spc.abzero), 2)))  
  polygon(density(l10.no.spc.abzero), col="blue")
  # density plot for 'l10.no.spc.abLOD'
  plot(density(l10.no.spc.abLOD), main="Density Plot: l10.no.spc.abLOD", ylab="Frequency", 
       sub=paste("Skewness:", round(e1071::skewness(l10.no.spc.abLOD), 2)))  
  polygon(density(l10.no.spc.abLOD), col="blue")
  # density plot for 'l10.no.spc.abLOQ'
  plot(density(l10.no.spc.abLOQ), main="Density Plot: l10.no.spc.abLOQ", ylab="Frequency", 
       sub=paste("Skewness:", round(e1071::skewness(l10.no.spc.abLOQ), 2)))  
  polygon(density(l10.no.spc.abLOQ), col="blue")
  #log10 transform did not improve the distribution!!
  
  
  #Correlation - A low correlation (-0.2 < x < 0.2) probably suggests that much of variation of 
  #the response variable (Y) is unexplained by the predictor (X), in which case, we should 
  #probably look for better explanatory variables.
  cor.spc.conv.eDNA <- cor(no.spc.conv, no.spc.abzero)
  -0.2 < cor.spc.conv.eDNA
  cor.spc.conv.eDNA < 0.2
  # There is a correlation - I should look for another explanatory variable
  # But I do not have a better explanatory variable to use instead.
  
  #try linear regression anyway
  linearMod.no.spc.edna_abzero <- lm(no.spc.conv ~ no.spc.abzero, data=df_MO4_10)
  linearMod.no.spc.edna_abLOD <- lm(no.spc.conv ~ no.spc.abLOD, data=df_MO4_10)
  linearMod.no.spc.edna_abLOQ <- lm(no.spc.conv ~ no.spc.abLOQ, data=df_MO4_10)
  #plot linear models in Q-Q-plots
  #http://data.princeton.edu/R/linearModels.html
  #plot for eDNA lvls above zero
  par(mfrow=c(2,2),
      oma=c(1,1,1,1), mar=c(3,3,3,3))
  plot(linearMod.no.spc.edna_abzero)
  pltnm1 <- "ab_zero"
  mtext(text=paste(pltnm1,", qq-plot", sep=""),side=1,line=1,outer=TRUE, cex=1.6)
  #dev.off()
  #plot for eDNA lvls above LOD
  par(mfrow=c(2,2),
      oma=c(1,1,1,1), mar=c(3,3,3,3))
  plot(linearMod.no.spc.edna_abLOD)
  pltnm1 <- "ab_LOD"
  mtext(text=paste(pltnm1,", qq-plot", sep=""),side=1,line=1,outer=TRUE, cex=1.6)
  #dev.off()
  #plot for eDNA lvls above LOQ
  par(mfrow=c(2,2),
      oma=c(1,1,1,1), mar=c(3,3,3,3))
  plot(linearMod.no.spc.edna_abLOQ)
  pltnm1 <- "ab_LOQ"
  mtext(text=paste(pltnm1,", qq-plot", sep=""),side=1,line=1,outer=TRUE, cex=1.6)
  #dev.off()
  
  
  #see if AIC and BIC makes a model preferable
  AIC(linearMod.no.spc.edna_abzero)
  #BIC(linearMod.no.spc.edna_abzero)
  AIC(linearMod.no.spc.edna_abLOD)
  #BIC(linearMod.no.spc.edna_abLOD)
  AIC(linearMod.no.spc.edna_abLOQ)
  #BIC(linearMod.no.spc.edna_abLOQ)
  
  #calculate the t Statistic and p-Values for the 'linearMod.no.spc.edna_abzero'-model
  modelSummary <- summary(linearMod.no.spc.edna_abzero)  # capture model summary as an object
  modelCoeffs <- modelSummary$coefficients  # model coefficients
  beta.estimate <- modelCoeffs["no.spc.abzero", "Estimate"]  # get beta estimate for no.spc.edna_abzero
  std.error <- modelCoeffs["no.spc.abzero", "Std. Error"]  # get std.error for no.spc.edna_abzero
  t_value <- beta.estimate/std.error  # calc t statistic
  p_value <- 2*pt(-abs(t_value), df=nrow(df_MO4_10)-ncol(df_MO4_10))  # calc p Value
  f_statistic <- linearMod.no.spc.edna_abzero$fstatistic[1]  # fstatistic
  f <- summary(linearMod.no.spc.edna_abzero)$fstatistic  # parameters for model p-value calc
  model_p <- pf(f[1], f[2], f[3], lower=FALSE)
  #is the p-value less than 0.05
  model_p < 0.05
  
  
  # Step 1: Create the training (development) and test (validation) data samples from original data.
  # Create Training and Test data -
  set.seed(100)  # setting seed to reproduce results of random sampling
  trainingRowIndex <- sample(1:nrow(df_MO4_10), 0.8*nrow(df_MO4_10))  # row indices for training data
  trainingData <- df_MO4_10[trainingRowIndex, ]  # model training data
  testData  <- df_MO4_10[-trainingRowIndex, ]   # test data
  # Step 2: Develop the model on the training data and use it to predict the distance on test data
  # Build the model on training data -
  lmMod <- lm(cnt_cvr2 ~ cnt_eDabze, data=trainingData)  # build the model
  no.spc.edna_abzeroPred <- predict(lmMod, testData)  # predict distance
  #Step 3: Review diagnostic measures.
  summary (lmMod) 
  #Step 4: Calculate prediction accuracy and error rates
  actuals_preds <- data.frame(cbind(actuals=testData$cnt_eDabze, predicteds=no.spc.edna_abzeroPred))  # make actuals_predicteds dataframe.
  correlation_accuracy <- cor(actuals_preds)  # 82.7%
  #head(actuals_preds)
  min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
  #min_max_accuracy
  mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
  #mape
  #k- Fold Cross validation
  #head(df_MO4_10)
  
  #get DAAG package
  if(!require(DAAG)){
    install.packages("DAAG")
  }  
  library(DAAG)
  #set plot area
  par(mfrow=c(1,1),
      oma=c(1,1,1,1), mar=c(3,3,3,3))
  cvResults <- suppressWarnings(CVlm(data=df_MO4_10, 
                                     form.lm=formula(cnt_eDabze ~ cnt_cvr2),
                                     m=3, dots=FALSE, 
                                     seed=29, legend.pos="topleft",  
                                     printit=FALSE, 
                                     main="Small symbols are predicted values while bigger ones are actuals."));  # performs the CV
  attr(cvResults, 'ms')  # => 251.2783 mean squared error
  
  
  #get ggplot2 package
  if(!require(ggplot2)){
    install.packages("ggplot2")
  }  
  library(ggplot2)
  #plot using ggplot
  head(df_MO4_10,4)
  dev.off()
  #see : https://www.guru99.com/r-simple-multiple-linear-regression.html
  ggplot(df_MO4_10,aes(x=cnt_cvr2, y =   cnt_eDabze))+
    geom_point()
  #estimate beta
  beta <- cov(df_MO4_10$cnt_cvr2, df_MO4_10$cnt_eDabze) / var (df_MO4_10$cnt_cvr2)
  beta
  #estimate alpha
  alpha <- mean(df_MO4_10$cnt_eDabze) - beta * mean(df_MO4_10$cnt_cvr2)
  alpha
  #try a simple model
  model <- df_MO4_10$cnt_eDabze~df_MO4_10$cnt_cvr2
  fit1 <- lm(model, df_MO4_10)
  
    summary(fit1)$r.squared # get Multiple R-squared
    summary(fit1)$adj.r.squared #get adjusted r.squared
    as.numeric(((summary(fit1)$coefficients)[, "Std. Error"])[1])
    as.numeric(((summary(fit1)$coefficients)[, "t value"])[1])
    as.numeric(((summary(fit1)$coefficients)[, "Pr(>|t|)"])[1])
    summary(fit1)$coefficients
    names(summary(fit1))
    coef(summary(fit1))[, "Std. Error"]
      
  #plot the qq-plot from the summary of the fitted model
  pdffn = paste(wd00,wd10,"/suppmatr_10.15a_App_H1_plot_no_spc_conv_vs_eDNA.pdf", sep="")
  pdf(pdffn 
      ,width=8, height=8
      #,width=(1.6*8.2677),height=(1.6*2.9232)
  )
  par(mfrow=c(2,2))
  plot(fit1)
  #end plotting to pdf
  dev.off()
  #make a second plot
  #make a name for the files to be saved
  pdffn = paste(wd00,wd10,"/suppmatr_10.15a_App_H2_plot_no_spc_conv_vs_eDNA.pdf", sep="")
  pdf(pdffn 
      ,width=8, height=8
      #,width=(1.6*8.2677),height=(1.6*2.9232)
  )
  par(mfrow=c(1,1) # c(2,1) will give 2 rows and 1 column
      , mar=c(4,4,1,1)
      , oma=c(2,2,1,1)
  )
  #put varaibles in to x and y
  x<-df_MO4_10$cnt_eDabze 
  y<-df_MO4_10$cnt_cvr2
  plot(x,y,xlim=c(1,12),ylim=c(1,12),
       main="eDNA and conventional species", offset = 2,
       xlab="no of species recorded by eDNA, incl below LOD",
       ylab="no of species recorded by conventional monitoring"
  )
  mylm<-lm(y~x)
  #get r values
  m.rsq <- summary(mylm)$r.squared # get Multiple R-squared
  a.rsq <- summary(mylm)$adj.r.squared #get adjusted r.squared
  # as.numeric(((summary(mylm)$coefficients)[, "Std. Error"])[1])
  # as.numeric(((summary(mylm)$coefficients)[, "t value"])[1])
  # as.numeric(((summary(mylm)$coefficients)[, "Pr(>|t|)"])[1])
  # summary(mylm)$coefficients
  # summary(mylm)
  intcp <- summary(mylm)$coefficients[1]
  slp <- summary(mylm)$coefficients[2]
  
  intcp2 <- round(intcp, digits = 2)
  slp2 <- round(slp , digits = 2)
  a.rsq2 <- round(a.rsq , digits = 3)
  #names(summary(mylm))
  #https://boostedml.com/2019/06/linear-regression-in-r-interpreting-summarylm.html
  #https://feliperego.github.io/blog/2015/10/23/Interpreting-Model-Output-In-R
  #https://quantifyinghealth.com/f-statistic-in-linear-regression/
  #get p-value
  pvl <- summary(mylm)$coefficients[,"Pr(>|t|)"][[2]]
  pvl2 <- round(pvl, digits = 2)
  abline(mylm,col="red")
  
  newx<-seq(1,12)
  prd<-predict(mylm,newdata=data.frame(x=newx),interval = c("confidence"), 
               level = 0.90,type="response")
  lines(newx,prd[,2],col="red",lty=2)
  lines(newx,prd[,3],col="red",lty=2)
  #add legend to plot
  # add a third legend for efficiency and R2
  legend("bottomright",
         inset=0.02,
         c(paste("R2: ",a.rsq2,sep=""),
           paste("equation: y=",slp2,"(x)+",intcp2,sep=""),
           paste("F-stat,p-value: ",pvl2,sep="")),
         #pch=c(24), #uncomment to get triangles on the line in the legend
         cex=0.8,
         bg="white",
         #lty=c(1), col=c("black","red"),
         y.intersp= 1.0,
         box.col="white")
  #end plotting to pdf
  dev.off()
  #head(df_MO4_10,4)
  #make a plot with limitations on the x-axis and on the y-axis
  ggplot(df_MO4_10,aes(x=cnt_eDabze, y = cnt_cvr2))+
    geom_point() +
    #and add a linear model
    stat_smooth(method = "lm", col = "red") +
    xlim(0, 12) + ylim(0, 12)
  
  #
  #Package ‘RSvgDevice’ to prepare svg-files with editable text
  if(!require(RSvgDevice)){
    install.packages("RSvgDevice")
    library(RSvgDevice)
  }
  library(RSvgDevice)
  svgf01 <- paste(wd00,wd10,"/suppmatr_10.15a_App_H3_plot_no_spc_conv_vs_eDNA.svg",sep=".")
  #plot is svg file
  devSVG(svgf01,width=(1*8.2677),height=(4*2.9232))
  par(mfrow=c(1,1) # c(2,1) will give 2 rows and 1 column
      , mar=c(4,4,1,1)
      , oma=c(6,1,1,1)
  )
  #put varaibles in to x and y
  x<-df_MO4_10$cnt_eDabze 
  y<-df_MO4_10$cnt_cvr2
  plot(x,y,xlim=c(1,12),ylim=c(1,12),
       main="eDNA and conventional species", offset = 2,
       xlab="no of species recorded by eDNA, incl below LOD",
       ylab="no of species recorded by conventional monitoring"
  )
  mylm<-lm(y~x)
  #get r values
  m.rsq <- summary(mylm)$r.squared # get Multiple R-squared
  a.rsq <- summary(mylm)$adj.r.squared #get adjusted r.squared
  # as.numeric(((summary(mylm)$coefficients)[, "Std. Error"])[1])
  # as.numeric(((summary(mylm)$coefficients)[, "t value"])[1])
  # as.numeric(((summary(mylm)$coefficients)[, "Pr(>|t|)"])[1])
  # summary(mylm)$coefficients
  # summary(mylm)
  intcp <- summary(mylm)$coefficients[1]
  slp <- summary(mylm)$coefficients[2]
  
  intcp2 <- round(intcp, digits = 2)
  slp2 <- round(slp , digits = 2)
  a.rsq2 <- round(a.rsq , digits = 3)
  #names(summary(mylm))
  #https://boostedml.com/2019/06/linear-regression-in-r-interpreting-summarylm.html
  #https://feliperego.github.io/blog/2015/10/23/Interpreting-Model-Output-In-R
  #https://quantifyinghealth.com/f-statistic-in-linear-regression/
  #get p-value
  pvl <- summary(mylm)$coefficients[,"Pr(>|t|)"][[2]]
  pvl2 <- round(pvl, digits = 2)
  abline(mylm,col="red")
  newx<-seq(1,12)
  prd<-predict(mylm,newdata=data.frame(x=newx),interval = c("confidence"), 
               level = 0.90,type="response")
  lines(newx,prd[,2],col="red",lty=2)
  lines(newx,prd[,3],col="red",lty=2)
  # add a third legend for efficiency and R2
  legend("bottomright",
         inset=0.02,
         c(paste("R2: ",a.rsq2,sep=""),
           paste("equation: y=",slp2,"(x)+",intcp2,sep=""),
           paste("F-stat,p-value: ",pvl2,sep="")),
         #pch=c(24), #uncomment to get triangles on the line in the legend
         cex=0.8,
         bg="white",
         #lty=c(1), col=c("black","red"),
         y.intersp= 1.0,
         box.col="white")
  #end plotting to svg
  dev.off()
  
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  # Plot linear regression models that compares conventional monitoring and eDNA monitoring  - end
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  
  
  
  
  
  
  
  
  
  
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #  Compare with traffic in Danish harbours - statestikbanken.dk - start
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #the dplyr package is required for dplyr::group_by function below
  #install.packages("dplyr")
  if(!require(dplyr)){
    install.packages("dplyr", dependencies = TRUE)
    library(dplyr)
  }
  
  #get files from https://www.statistikbanken.dk/
  # select: "Erhvervslivets sektorer"
  #
  #https://www.statistikbanken.dk/statbank5a/default.asp?w=1366
  # select: "Godstransport med skib"
  #
  # select: "SKIB72Godsomsætning på større danske havne efter havn, retning og godsart (2000K1-2018K4) "
  # select : "SKIB23Fragtskibes og krydstogtskibes anløb på større danske havne efter havn og skibstype (1997-2018)"
  # select: "SKIB221Skibsanløb på danske havne efter havn, skibstype og bruttotonnage (BT) (2007-2018) "
  # select: "SKIB22: Skibsanløb på danske havne efter havn, skibstype og bruttotonnage (BT) (AFSLUTTET) "
  
  
  #recode the files with this bash code in a terminal:
  # ################################################################################################
  # # bash caode starts
  # ################################################################################################
  # 
  # #!/bin/bash
  # # -*- coding: utf-8 -*-
  # 
  # #code to convert multiple iso encoded txt files in to utf-8 encoed files
  # 
  # # put the present working directory in a variable
  # WD=$(pwd)
  # 
  # #define directory for input files
  # OUTDIR1="files_w_skibtrafik_from_statestikbanken"
  # 
  # #delete previous directory for input files
  # rm -rf "${OUTDIR1}"
  # #make directory for input files
  # mkdir "${OUTDIR1}"
  # #copy files into directory for input files
  # for FILE in skibtrafik_*.csv
  # do
  # cp "${FILE}" "${WD}"/"${OUTDIR1}"/.
  # 
  # done
  # 
  # #change in to this directory
  # cd "${WD}"/"${OUTDIR1}"
  # 
  # 
  # #enter input encoding here
  # FROM_ENCODING="ISO-8859-1"
  # #output encoding(UTF-8)
  # TO_ENCODING="UTF-8"
  # #convert
  # CONVERT=" iconv -f $FROM_ENCODING -t $TO_ENCODING"
  # #loop to convert multiple files 
  # for FILE in *.csv
  # do
  # NFNM2=$(echo $FILE | sed 's/\.csv/_02\.csv/g')
  # NFNM3=$(echo $FILE | sed 's/\.csv/_03\.csv/g')
  # #iconv -f iso-8859-1 -t utf-8 < file > file.new
  # $CONVERT  < "$FILE" > "${NFNM2}.utf8.converted"
  # cat "${NFNM2}.utf8.converted" | \
  # LC_ALL=C sed 's/ø/oe/g' | \
  # LC_ALL=C sed 's/æ/ae/g' | \
  # LC_ALL=C sed 's/å/aa/g' | \
  # LC_ALL=C sed 's/Ø/OE/g' | \
  # LC_ALL=C sed 's/Æ/AE/g' | \
  # LC_ALL=C sed 's/Å/AA/g' | \
  # LC_ALL=C sed 's/¿/oe/g' | \
  # LC_ALL=C sed 's/¾/ae/g' | \
  # LC_ALL=C sed 's:\/::g' > "${NFNM3}.utf8.converted"
  # #head -30 "${NFNM3}.utf8.converted"
  # file -I "${NFNM3}.utf8.converted"
  # done
  # rm *_02.csv.utf8.converted
  # 
  # 
  # ls -lh #*03.csv.utf8.converted
  # #exit 0
  # ################################################################################################
  # # bash caode ends
  # ################################################################################################
  #define input files
  #define 'SKIB22' input
  skibfile16 <-
    "suppmatr_09.16_inp_skibtrafik_2019321142935242910234SKIB22152257426947_04.csv.utf8.converted"
  #define 'SKIB72' input
  skibfile17 <-
    "suppmatr_09.17_inp_skibtrafik_2019321144619242910234SKIB7253194085905_04.csv.utf8.converted"
  #define 'SKIB42' input
  skibfile18 <-
    "suppmatr_09.18_inp_skibtrafik_201932114490242910234SKIB42153363062072_04.csv.utf8.converted"
  #define 'SKIB23' input
  skibfile19 <-
    "suppmatr_09.19_inp_skibtrafik_2019520153440247984933SKIB2356088802999_04.csv.utf8.converted"
  #define path and input files, by pasting path and file together
  pthfl16 <- paste(wd00,wd09,"/",skibfile16,sep="")
  pthfl17 <- paste(wd00,wd09,"/",skibfile17,sep="")
  pthfl18 <- paste(wd00,wd09,"/",skibfile18,sep="")
  pthfl19 <- paste(wd00,wd09,"/",skibfile19,sep="")
  
    # Read the .csv files
  SKIB22 <- read.csv(pthfl16, sep = ";",
                     stringsAsFactors = FALSE)
  #change column names
  colnames(SKIB22) <- c("aar","skibe_ialt","Havne","SKIBE_I_ALT_enhed_millioner_DKK", "Lastskibe_enhed_millioner_DKK", "Passagerskibe_og_faerge_enhed_millioner_DKK")
  #readin csv file
  SKIB23 <- read.csv(pthfl19, sep = ";",
                     skip = 1, stringsAsFactors = FALSE, header=F)
  #get rows with headers
  SKIB23.col.nms <- as.character(SKIB23[2,])
  #remove first element from this list
  SKIB23.col.nms.2 <- SKIB23.col.nms[-1]
  SKIB23.col.nms.2[1] <- "Havn"
  #get the same data frame but without the first column
  SKIB23.1 <- SKIB23[,-1]
  #get the same data frame but use a negative sequence to not list the first 1 to 3 rows
  SKIB23.2 <- SKIB23.1[-(1:3),]
  #get the number of rows
  n.skib23.2 <- nrow(SKIB23.2)
  #get the same data frame but use a negative sequence to not list the last rows
  SKIB23.3 <- SKIB23.2[-((n.skib23.2-4):n.skib23.2),]
  SKIB23 <- SKIB23.3
  #change column names
  colnames(SKIB23) <- SKIB23.col.nms.2
  #Read in skib72 csv from pthfl17 
  SKIB72 <- read.csv(pthfl17, sep = ";",
                     skip = 1, stringsAsFactors = FALSE)
  #change column names
  colnames(SKIB72) <- c("INDGAAENDE_OG_UDGAAENDE_GODS_I_ALT_i_1000ton", "Indgaaende_gods_i_alt_i_1000ton", "Havn", "Indgaaende_gods_fra_indland_i_1000ton", "Udgaaende_gods_i_alt_i_1000ton", "Udgaaende_gods_til_udland_i_1000ton", "Udgaaende_gods_til_indland_i_1000ton")
  #read in csv file
  #Read in skib42 csv from pthfl18 
  SKIB42 <- read.csv(pthfl18, sep = ";",
                     skip = 1, stringsAsFactors = FALSE)
  #change column names
  colnames(SKIB42) <- c("GODSMAENGDE_IALT","Havn", "godsmaengde_i_1000ton")
  
  ##########################################################################################
  # begin -  Function to fill NAs with previous value
  ##########################################################################################
  #fill NAs with latest non-NA value
  #http://www.cookbook-r.com/Manipulating_data/Filling_in_NAs_with_last_non-NA_value/
  #https://stackoverflow.com/questions/7735647/replacing-nas-with-latest-non-na-value
  
  fillNAgaps <- function(x, firstBack=FALSE) {
    ## NA's in a vector or factor are replaced with last non-NA values
    ## If firstBack is TRUE, it will fill in leading NA's with the first
    ## non-NA value. If FALSE, it will not change leading NA's.
    
    # If it's a factor, store the level labels and convert to integer
    lvls <- NULL
    if (is.factor(x)) {
      lvls <- levels(x)
      x    <- as.integer(x)
    }
    
    goodIdx <- !is.na(x)
    goodIdx <- !is.na(x)
    # These are the non-NA values from x only
    # Add a leading NA or take the first good value, depending on firstBack   
    if (firstBack)   goodVals <- c(x[goodIdx][1], x[goodIdx])
    else             goodVals <- c(NA,            x[goodIdx])
    
    # Fill the indices of the output vector with the indices pulled from
    # these offsets of goodVals. Add 1 to avoid indexing to zero.
    fillIdx <- cumsum(goodIdx)+1
    
    x <- goodVals[fillIdx]
    
    # If it was originally a factor, convert it back
    if (!is.null(lvls)) {
      x <- factor(x, levels=seq_along(lvls), labels=lvls)
    }
    
    x
  }
  ##########################################################################################
  # end -  Function to fill NAs with previous value
  ##########################################################################################
  
  ##########################################################################################
  # start -  Function to fill blanks with previous value
  ##########################################################################################
  #see this website: https://stackoverflow.com/questions/9514504/add-missing-value-in-column-with-value-from-row-above
  fillTheBlanks <- function(x, missing=""){
    rle <- rle(as.character(x))
    empty <- which(rle$value==missing)
    rle$values[empty] <- rle$value[empty-1] 
    inverse.rle(rle)
  }
  ##########################################################################################
  # end -  Function to fill blanks with previous value
  ##########################################################################################
  
  #fill blanks in column with the value above
  SKIB22$skibe_ialt <- fillTheBlanks(SKIB22$skibe_ialt)
  #make columns to characters
  SKIB22$Havne <- as.character(SKIB22$Havne)
  #make columns numeric
  SKIB22$aar <- as.numeric(SKIB22$aar)
  SKIB22$SKIBE_I_ALT_enhed_millioner_DKK <- as.numeric(SKIB22$SKIBE_I_ALT_enhed_millioner_DKK)
  SKIB22$Lastskibe_enhed_millioner_DKK <- as.numeric(SKIB22$Lastskibe_enhed_millioner_DKK)
  SKIB22$Passagerskibe_og_faerge_enhed_millioner_DKK <- as.numeric(SKIB22$Passagerskibe_og_faerge_enhed_millioner_DKK)
  #replace NA with zeroes in dataframe
  SKIB22[is.na(SKIB22)] <- 0
  #make columns to characters
  SKIB23$Havn <- as.character(SKIB23$Havn)
  #make columns numeric
  #change  a lot of columns to 'characters' and then to 'numeric'
  nskib23col <- ncol(SKIB23) # by first counting the number of columns
  cnm <- 2:nskib23col #defining a range of which columns to work on
  SKIB23[cnm] <- lapply(SKIB23[cnm], as.character) #apply the function on the columns
  SKIB23[cnm] <- lapply(SKIB23[cnm], as.numeric) #apply the function on the columns
  #replace NA with zeroes in dataframe
  SKIB23[is.na(SKIB23)] <- 0
  # use the function defined above
  #fill blanks in column with the value above
  SKIB42$GODSMAENGDE_IALT <- fillTheBlanks(SKIB42$GODSMAENGDE_IALT)
  #make columns to characters
  SKIB42$Havn <- as.character(SKIB42$Havn)
  #make columns numeric
  SKIB42$godsmaengde_i_1000ton <- as.numeric(SKIB42$godsmaengde_i_1000ton)
  #replace NA with zeroes in dataframe
  SKIB42[is.na(SKIB42)] <- 0
  #fill blanks in column with the value above
  SKIB72$INDGAAENDE_OG_UDGAAENDE_GODS_I_ALT_i_1000ton <- fillTheBlanks(SKIB72$INDGAAENDE_OG_UDGAAENDE_GODS_I_ALT_i_1000ton)
  SKIB72$Indgaaende_gods_i_alt_i_1000ton <- fillTheBlanks(SKIB72$Indgaaende_gods_i_alt_i_1000ton)
  #make columns to characters
  SKIB72$INDGAAENDE_OG_UDGAAENDE_GODS_I_ALT_i_1000ton <- as.character(SKIB72$INDGAAENDE_OG_UDGAAENDE_GODS_I_ALT_i_1000ton)
  SKIB72$Indgaaende_gods_i_alt_i_1000ton <- as.character(SKIB72$Indgaaende_gods_i_alt_i_1000ton)
  #make columns numeric
  SKIB72$Indgaaende_gods_i_alt_i_1000ton <- as.numeric(SKIB72$Indgaaende_gods_i_alt_i_1000ton)
  SKIB72$Indgaaende_gods_fra_indland_i_1000ton <- as.numeric(SKIB72$Indgaaende_gods_fra_indland_i_1000ton)
  SKIB72$Udgaaende_gods_i_alt_i_1000ton <- as.numeric(SKIB72$Udgaaende_gods_i_alt_i_1000ton)
  SKIB72$Udgaaende_gods_til_udland_i_1000ton <- as.numeric(SKIB72$Udgaaende_gods_til_udland_i_1000ton)
  SKIB72$Udgaaende_gods_til_indland_i_1000ton <- as.numeric(SKIB72$Udgaaende_gods_til_indland_i_1000ton)
  #replace NA with zeroes in dataframe
  SKIB72[is.na(SKIB72)] <- 0
  #delete rows with empty entry for havn
  SKIB22<-SKIB22[!(SKIB22$Havne==""),]
  SKIB23<-SKIB23[!(SKIB23$Havn==""),]
  SKIB42<-SKIB42[!(SKIB42$Havn==""),]
  SKIB72<-SKIB72[!(SKIB72$Havn==""),]
  #delete rows with "HAVNEIALT" entry for havn
  SKIB22<-SKIB22[!(SKIB22$Havne=="HAVNEIALT"),]
  SKIB42<-SKIB42[!(SKIB42$Havn=="HAVNEIALT"),]
  SKIB72<-SKIB72[!(SKIB72$Havn=="HAVNEIALT"),]
  #copy one column in to a new column
  SKIB42$gods_trafik <- SKIB42$GODSMAENGDE_IALT
  SKIB72$gods_type <- SKIB72$INDGAAENDE_OG_UDGAAENDE_GODS_I_ALT_i_1000ton
  SKIB22$skib_tonnage <- SKIB22$skibe_ialt
  #delete old columns
  SKIB42$GODSMAENGDE_IALT <- NULL
  SKIB72$INDGAAENDE_OG_UDGAAENDE_GODS_I_ALT_i_1000ton <- NULL
  SKIB22$skibe_ialt <- NULL
  SKIB22$aar <- NULL
  #reshape data frame long to wide based on 'Havn'
  SKIB42_2 <- reshape(SKIB42, idvar = "Havn", timevar = "gods_trafik", direction = "wide")
  #head(SKIB42_2)
  SKIB72_2 <- reshape(SKIB72, idvar = "Havn", timevar = "gods_type", direction = "wide")
  #head(SKIB72_2)
  
  #https://stackoverflow.com/questions/11612235/select-rows-from-a-data-frame-based-on-values-in-a-vector
  #define a vector that holds the 16 harbours
  havne.k.SKIB72 <- c("KoebenhavnsHavn", "HelsingoerHavn", "KoegeHavn", "KalundborgHavn", "Statoil_havnen", "GedserHavn", "RoedbyFaergehavn", "OdenseHavn", "EsbjergHavn", "FredericiaHavn", "GrenaaHavn", "AarhusHavn", "AalborgHavn", "AalborgPortlandHavn", "FrederikshavnHavn", "HirtshalsHavn")
  havne.k.SKIB42 <- c("KoebenhavnsHavn", "HelsingoerHavn", "KoegeHavn", "GedserHavn", "KalundborgHavn", "RoedbyFaergehavn", "Statoil_havnen", "OdenseHavn", "EsbjergHavn", "FredericiaHavn", "GrenaaHavn", "AarhusHavn", "FrederikshavnHavn", "HirtshalsHavn", "AalborgHavn", "AalborgPortlandHavn")
  havne.k.SKIB22 <- c("AalborgHavn", "AalborgPortlandHavn", "AarhusHavn", "EsbjergHavn", "FredericiaHavn", "FrederikshavnHavn", "GedserHavn", "GrenaaHavn", "HelsingoerHavn", "HirtshalsHavn", "KalundborgHavn", "KoebenhavnsHavn", "KoegeHavn", "OdenseHavn", "RoedbyHavn", "Statoil_havnen")
  #keep only rows that match the vector with harbours - i.e. delete the other rows that does not match the vvector
  SKIB72_3 <- SKIB72_2[SKIB72_2$Havn %in% havne.k.SKIB72,]
  SKIB42_3 <- SKIB42_2[SKIB42_2$Havn %in% havne.k.SKIB42,]
  SKIB22_2 <- SKIB22[SKIB22$Havn %in% havne.k.SKIB22,] # too many harbours made it easier to subset this data frame first
  SKIB23_3 <- SKIB23[SKIB23$Havn %in% havne.k.SKIB72,]
  #replace specific value in column based on condition
  SKIB22_2$Havne[SKIB22_2$Havne=="RoedbyHavn"] <- "RoedbyFaergehavn"
  #reshape data frame long to wide based on 'Havn'
  SKIB22_3 <- reshape(SKIB22_2, idvar = "Havne", timevar = "skib_tonnage", direction = "wide")
  #count columns and rows
  #ncol(SKIB23_3) # count the number of columns
  #nrow(SKIB23_3) # count the number of rows
  #ncol(SKIB22_3) # count the number of columns
  #nrow(SKIB22_3) # count the number of rows
  #change  a lot of columns to 'characters' and then to 'numeric'
  #ncol(SKIB72_3) # by first counting the number of columns
  cnm <- 2:116 #defining a range of which columns to work on
  SKIB72_3[cnm] <- lapply(SKIB72_3[cnm], as.character) #apply the function on the columns
  SKIB72_3[cnm] <- lapply(SKIB72_3[cnm], as.numeric) #apply the function on the columns
  #list harbours , and sort alphabetically
  harb.SKIB72_3 <- sort(unique(SKIB72_3$Havn))
  #
  #split string by point, bind the result as rows, 
  #get the first column, which is the harbours, 
  #and add back to the original data frame
  df_MO4_10$Harbour <- data.frame(do.call('rbind', strsplit(as.character(df_MO4_10$Harbour.season),'.',fixed=TRUE)))[,1]
  #list harbours , and sort alphabetically
  harb.df_MO4_10 <- sort(unique(df_MO4_10$Harbour))
  #make a new vector with harbours
  harb.SKIB72_3.2 <- c("AalborgHavn", "AalborgPortlandHavn", "AarhusHavn", "EsbjergHavn", "FredericiaHavn", "FrederikshavnHavn", "GedserHavn", "GrenaaHavn", "HelsingoerHavn", "HirtshalsHavn", "KalundborgHavn", "KoebenhavnsHavn", "KoegeHavn", "OdenseHavn", "RoedbyFaergehavn", "Statoil_havnen")
  harb.df_MO4_10.2 <- c("AalborgHavn", "Aalborgportland", "Aarhus", "Esbjerg", "Fredericia", "Frederikshavn", "Gedser", "Grenaa", "Helsingoer", "Hirtshals", "Kalundborg", "Koebenhavn", "Koege", "Odense", "Roedby", "KalundborgStatiolHavn")
  #bind vectors to a date frame and transpose 't' the dataframe
  hc <- t(data.frame(
    harb.SKIB72_3.2,
    harb.df_MO4_10.2))
  hc <- as.data.frame(t(hc))
  
  #colnames(df_MONIS4_02)
  #add one to all mean quantity copies - to be able to
  #take log10 to the value
  df_MONIS4_02$mqcp1 <- df_MONIS4_02$meanQuantitycopies
  
  df_MONIS4_02$mqcp1 <- df_MONIS4_02$meanQuantitycopies
  
  #also do the same for copie per L water -  this was already
  # done in the previous code
  df_MONIS4_02$cpLwp1beLD <- df_MONIS4_02$copies_per_Lwater_plone
  #If the copy number is above LOQ then return the
  #copy number per L water plus 1
  #first evaluate whether the value is above LOQ
  df_MONIS4_02$eval_eDabLQ<- (df_MONIS4_02$meanQuantitycopies>df_MONIS4_02$LOQ)*1
  #Multiplying with this factor includes eDNA levels above LOQ 
  df_MONIS4_02$cpLwabLQ <- df_MONIS4_02$eval_eDabLQ*df_MONIS4_02$copies_per_Lwater
  #Multiplying with this factor includes only eDNA 
  #levels above LOQ
  #df_MONIS4_02$cpLwp1 <- df_MONIS4_02$eval_eDabLQ*df_MONIS4_02$cpLwp1 
  #Note the addition of 1 to be able to take the log10 
  #to the number
  #and take the logarithm to this value
  #df_MONIS4_02$cpLwp1l10<- log10(df_MONIS4_02$cpLw+1)
  
  
  
  
  #drop the variables that are not needed
  #only keep the ones listed
  keep <- c("spc.season",
    "Harbour", 
    #"cpLwp1",
    #"mqcp1",
    "declat",
    "declon",
    "conv_rec_val",
    "cpLwp1beLD",
    "cpLwabLQ"
    #"cpLwp1l10"
    
  )
  #use the keep data frame to only keep selected columns
  df_MONIS4_03 <- df_MONIS4_02[ , (names(df_MONIS4_02) %in% keep)]
  
  #match harbour names to a new data frame
  df_MONIS4_03$Harbour.2 <- hc$harb.SKIB72_3.2[
    match(df_MONIS4_03$Harbour,hc$harb.df_MO4_10.2)]
  #drop the variables that are not needed
  drops <- c("Harbour")
  df_MONIS4_03 <- df_MONIS4_03[ ,
                                !(names(df_MONIS4_03) %in% drops)]
  #reshape data frame long to wide based on 'Harbour.2'
  df_MONIS4_04 <- reshape(df_MONIS4_03, idvar = "Harbour.2", 
                          timevar = "spc.season", 
                          direction = "wide")
  #merge dataframes by common column
  #https://stackoverflow.com/questions/35202380/merging-data-frames-with-different-number-of-rows-and-different-columns
  df_MONIS4_05 <- merge(data.frame(df_MONIS4_04, row.names=NULL),
                        data.frame(SKIB42_3, row.names=NULL), 
                        by.x = "Harbour.2", by.y = "Havn", all = TRUE)
  #merge dataframes by common column
  #https://stackoverflow.com/questions/35202380/merging-data-frames-with-different-number-of-rows-and-different-columns
  df_MONIS4_06 <- merge(data.frame(df_MONIS4_05, row.names=NULL), 
                        data.frame(SKIB72_3, row.names=NULL), 
                        by.x = "Harbour.2", by.y = "Havn", all = TRUE)
  #merge dataframes by common column
  #https://stackoverflow.com/questions/35202380/merging-data-frames-with-different-number-of-rows-and-different-columns
  df_MONIS4_07 <- merge(data.frame(df_MONIS4_06,
                                   row.names=NULL), 
                        data.frame(SKIB22_3, row.names=NULL), 
                        by.x = "Harbour.2", by.y = "Havne", all = TRUE)
  #change the hyphen in a column
  SKIB23_3$Havn <- gsub("-","_", SKIB23_3$Havn)
  #merge dataframes by common column
  #https://stackoverflow.com/questions/35202380/merging-data-frames-with-different-number-of-rows-and-different-columns
  df_MONIS4_08 <- merge(data.frame(df_MONIS4_07, row.names=NULL),
                        data.frame(SKIB23_3, row.names=NULL), 
                        by.x = "Harbour.2", by.y = "Havn", all = TRUE)
  #merge dataframes by common column
  #make a new dataframe based on the previous
  edna11 <- df_MONIS4_08
  
  #do a correlation test, to see what values are reflecting each other
  #Before evaluating the correlation for predictors of your dataset 
  #remove the zero variance predictors.
  #https://stackoverflow.com/questions/19113181/removing-na-in-correlation-matrix
  edna11.tmp <- apply(edna11, 2, function(x) length(unique(x)) == 1)
  edna11r <- edna11[, !edna11.tmp]
  n=length(colnames(edna11r))
  #only get numeric columns : https://stackoverflow.com/questions/5863097/selecting-only-numeric-columns-from-a-data-frame
  #I could not get this working with dplyr ?
  #edna12r <- dplyr::select_if(edna11r, is.numeric)
  #instead I use 'lapply'
  nums <- unlist(lapply(edna11r, is.numeric))
  #and then subset
  edna12r <- edna11r[ , nums]
  ##
  n=length(colnames(edna12r))
  #append the column with harbours back
  edna12r$harbour <- edna11r$Harbour.2
  #log-transform every variable in the dataframe
  #https://stackoverflow.com/questions/15215848/apply-function-to-every-value-in-an-r-dataframe
  #notice that you are adding "one" to all values, to be able to take the logarithm 
  #to the columns which have zeroes in them
  edna12r.log <- log10((edna12r[,1:n])+1)
  #append the column with harbours back
  #edna12r.log$harbour <- edna12r$harbour
  #Remove highly correlated variables, more than 0.7 correlated
  #https://stackoverflow.com/questions/18275639/remove-highly-correlated-variables
  tmp <- cor(edna12r.log,use="complete.obs")
  tmp[upper.tri(tmp)] <- 0
  diag(tmp) <- 0
  # Above two commands can be replaced with 
  # tmp[!lower.tri(tmp)] <- 0
  edna12r.log.2 <- edna12r.log[,!apply(tmp,2,function(x) any(abs(x) > 0.7))]
  #head(edna12r.log.2)
  #append the column with harbours back
  edna12r.log.2$harbour <- edna12r$harbour
  #repalce any NAs with zeros
  edna12r.log.2[is.na(edna12r.log.2)] <- 0
  #Now get the columns from the df_MONIS4_08-dataframe
  #where the copy per L is calculated and the values below LOD and below LOQ have been set to 1
  #this is the 
  #df_MONIS4_08$cpLwp1. columns
  #grep the columns that has the "cpLwp1." in the column name, and put this in a vector
  cols.from.df_MONIS4_08 <- grep("cpLwabLQ.", 
                                 names(df_MONIS4_08), value = TRUE)
  #also get the first column name - this column has the harbour names
  colnms.1.from.df_MONIS4_08 <- colnames(df_MONIS4_08)[1]
  #append this to the character vector
  cols.from.df_MONIS4_08.2 <- append(cols.from.df_MONIS4_08,
                                     colnms.1.from.df_MONIS4_08)
  #now use this vector to extract the column names from the dataframe
  keep <- c(cols.from.df_MONIS4_08.2)
  #use the keep data frame to only keep selected columns
  df_MONIS4_08.2 <- df_MONIS4_08[ , (names(df_MONIS4_08) %in% keep)]
  #head(df_MONIS4_08.2,5)
  #get the number of columns in this data frame
  n <- ncol(df_MONIS4_08.2)
  
  #log-transform every variable in the dataframe , but not the last, notice the 'n-1'
  #https://stackoverflow.com/questions/15215848/apply-function-to-every-value-in-an-r-dataframe
  #edna11.2.log <- log10(df_MONIS4_08.2[,1:(n-1)])
  #exclude the first column instead
  # add 1 to all values, to avoid taking
  # the log10 to zero
  edna11.2.log <- log10((df_MONIS4_08.2[,2:(n)])+1)
  
  #use the column defined with harbours above to extract the column with harbour names , 
  #and append this to the new data frame 
  edna11.2.log$harbours <- df_MONIS4_08[,colnms.1.from.df_MONIS4_08]
  #
  # identify columns with 'cpLwp1.' in the 'edna12r.log.2' data frame
  # they alos appear in the next data frame, and there is no need 
  # for duplicates
  colnm.e12 <- grep(paste("cpLwabLQ.", sep=""),
                      names(edna12r.log.2), value = TRUE)
  #rename this list to drop these columns from the  data frame
  drop <- colnm.e12
  #use the drop data frame to only keep selected columns
  edna12r.log.3 <- edna12r.log.2[ , !(names(edna12r.log.2) %in% drop)]
  # overwrite the previous data frame
  edna12r.log.2 <- edna12r.log.3
  
  #merge dataframes by common column
  df_MONIS4_08 <- merge(edna12r.log.2, edna11.2.log, 
                      by.x = "harbour", by.y = "harbours")
  
  #head(df_MONIS4_08,5)
  #colnames(df_MONIS4_08)
  #count number of columns in each data frame
  #ncol(edna12r.log.2)
  #ncol(edna11.2.log)
  #ncol(df_MONIS4_08)
  #remove columns that have zeroes in the entire column: https://stackoverflow.com/questions/21530168/remove-columns-with-zero-values-from-a-dataframe
  df_MONIS4_09 <- df_MONIS4_08[!is.na(colSums(df_MONIS4_08 != 0))
                               & colSums(df_MONIS4_08 != 0) > 0]
  #count number of columns in each data frame
  #ncol(df_MONIS4_09)
  df_MONIS4_10 <- df_MONIS4_09[,-1]
  
  
  
  # col.dfM4_10 <- grep(paste("Mne", sep=""),
  #                     names(df_MONIS4_10), value = TRUE)
  
  #head(df_MONIS4_09,4)
  #head(df_MONIS4_10,4)
  ## Note: This is a step wise regression to see if any combination of 
  #the variables can explain a correlation between the variables
  # Again, it looks like none of those variables can explain it well.
  # But you could put a much longer list of variables to explore which are good looking. 
  #install.packages("leaps")
  if(!require(leaps)){
    install.packages("leaps")
    library(leaps)
  }
  library(leaps)
  
  # All Subsets Regression
  library(leaps)
  
  #list unique species names
  unq.spc.seas <- unique(df_MONIS4_02$spc)
  
  #list unique species names
  unq_spc.nms <- unique(df_MONIS4_02$spc)
  #remove the species that have zero values in their columns 
  # - and not are possible to use the regsubsets function on
  # they are either missing values for spring or for
  # autumn , because zero eDNA levels were detected in these
  # seasons
  #______________________________________________________________
  # do this for season 1
  unq_spc.nms <- unq_spc.nms[!unq_spc.nms %in% c("Acipenser_baerii", 
                                                 "Bonnemaisonia_hamifera",
                                                 "Carassius_auratus",
                                                 "Colpomenia_peregrine",
                                                 "Cordylophora_caspia",
                                                 "Crassostrea_gigas",
                                                 "Cyprinus_carpio",
                                                 "Eriocheir_sinensis",
                                                 "Homarus_americanus",
                                                 "Karenia_mikimotoi",
                                                 "Neogobius_melanostomus",
                                                 "Oncorhynchus_mykiss",
                                                 "Oncorhyncus_gorbuscha",
                                                 "Paralithodes_camtschaticus",#)]#,
                                                 "Pseudochattonella_verruculosa",
                                                 "Rhithropanopeus_harrisii")] 
  # subset dataframe : https://stackoverflow.com/questions/10085806/extracting-specific-columns-from-a-data-frame
  AssID.to.spc <- unique(df_MONIS4_02[,c("spc","AssayIDNo")])
  
  #
  ########################################################
  # for loop start here for season 1 and 2
  ########################################################
  #spec.lat <- unq_spc.nms
  # loop over all species names in the unique list of species, and make plots. 
  #Notice that the curly bracket ends after the pdf file is closed
  for (spec.lat in unq_spc.nms){
    
    Ass.no <- AssID.to.spc$AssayIDNo[match(spec.lat, 
                                           as.character(AssID.to.spc$spc))]
    
    #substitute in names
    nms_1st <- gsub("_.*$","",spec.lat)
    nms_2nd <- gsub("^([^_]*)_([^_]*).*$", "\\2", spec.lat)
    genabb <- substring(nms_1st,1,3)
    spcabb <- substring(nms_2nd,1,3)
    spec.abbrnm <- paste(genabb, spcabb, sep="")
    #copy the data frame to preserve the original data frame
    df_MONIS4_10.2 <- df_MONIS4_10
    #colnames(df_MONIS4_10)
    #grep the columns that has the "copies per L water plus 1"- i.e.
    # "cpLwp1" and the species name, and the season
    col.m.q.cop02.eft <- grep(paste("cpLwabLQ.",spec.lat,".efteraar", sep=""), names(df_MONIS4_10.2), value = TRUE)
    col.m.q.cop02.for <- grep(paste("cpLwabLQ.",spec.lat,".foraar", sep=""), names(df_MONIS4_10.2), value = TRUE)
    #change column name for selected column
    #https://stackoverflow.com/questions/6081439/changing-column-names-of-a-data-frame
    # ti have an unchangeable column name in the leaps function
    #that will follow
    colnames(df_MONIS4_10.2)[which(names(df_MONIS4_10.2) == col.m.q.cop02.eft)] <- "cce2"
    colnames(df_MONIS4_10.2)[which(names(df_MONIS4_10.2) == col.m.q.cop02.for)] <- "ccf2"
    
    #head(df_MONIS4_10,3)
    pdffn = paste(wd00,wd10,"/suppmatr_10.15b_App_I_",Ass.no,"_rsu_aR2_",spec.abbrnm,"_spring_and_fall.pdf", sep="")
    pdf(pdffn 
        ,width=6, height=8
        #,width=(1.6*8.2677),height=(1.6*2.9232)
    )
    #plot.new()
    #dev.off()
    par(mfrow=c(2,2)
        , mar=c(0,0,0,0)
        , oma=c(7,0,0,0)
    ) 
    #see this question on how to get the actual values not just the variable name
    #https://stackoverflow.com/questions/5215481/remove-quotes-from-a-character-vector-in-r
    #https://stackoverflow.com/questions/18222286/dynamically-select-data-frame-columns-using-and-a-vector-of-column-names
    #spc.eft <- get(col.m.q.cop02.eft, df_MONIS4_10.2)
    #spc.for <- get(col.m.q.cop02.for, df_MONIS4_10.2)
    #now you can use this in the regsubsets function
    # forward leaps
    leaps_f.out.eft<-leaps::regsubsets(cce2 ~ . ,
                                #+copy.per.L.Mnemiopsis_leidyi ,
                                method = "forward",
                                nvmax=3,
                                data=df_MONIS4_10.2,nbest=12)
    # forward leaps
    leaps_f.out.for<-regsubsets(ccf2 ~ . ,#+copy.per.L.Mnemiopsis_leidyi ,
                                method = "forward",
                                nvmax=3,
                                data=df_MONIS4_10.2,nbest=12)
    
    # view results 
    #summary(leaps_f.out)
    
    #https://rstudio-pubs-static.s3.amazonaws.com/2897_9220b21cfc0c43a396ff9abf122bb351.html
    par(cex.axis=0.4)
    plot(leaps_f.out.eft, scale = "adjr2", main = paste("Adjusted R^2 forward\n",spec.lat,"_efteraar",sep=""),
         cex.lab=0.2,
         cex.axis=0.2,
         cex.sub=0.3, font= 1)
    par(cex.axis=1.0)
    #dev.off()
    par(cex.axis=0.4)
    plot(leaps_f.out.for, scale = "adjr2", main = paste("Adjusted R^2 forward\n",spec.lat,"_foraar",sep=""),
         cex.lab=0.2,
         cex.axis=0.2,
         cex.sub=0.3, font= 1)
    par(cex.axis=1.0)
    
    # backward leaps
    #see this question on how to get the actual values not just the variable name
    #https://stackoverflow.com/questions/5215481/remove-quotes-from-a-character-vector-in-r
    
    #now you can use this in the regsubsets function
    # forward leaps
    leaps_b.out.eft<-regsubsets(ccf2 ~ . ,#+copy.per.L.Mnemiopsis_leidyi ,
                                method = "backward",
                                nvmax=3,
                                data=df_MONIS4_10.2,nbest=12)
    
    # forward leaps
    leaps_b.out.for<-regsubsets(cce2 ~ . ,#+copy.per.L.Mnemiopsis_leidyi ,
                                method = "backward",
                                nvmax=3,
                                data=df_MONIS4_10.2,nbest=12)
    
    # view results 
    #summary(leaps_f.out)
    
    #https://rstudio-pubs-static.s3.amazonaws.com/2897_9220b21cfc0c43a396ff9abf122bb351.html
    par(cex.axis=0.4)
    plot(leaps_b.out.eft, scale = "adjr2", main = paste("Adjusted R^2 backw\n",spec.lat,"_efteraar",sep=""),
         cex.lab=0.2,
         cex.axis=0.2,
         cex.sub=0.3, font= 1)
    par(cex.axis=1.0)
    #dev.off()
    par(cex.axis=0.4)
    plot(leaps_b.out.for, scale = "adjr2", main = paste("Adjusted R^2 backw\n",spec.lat,"_foraar",sep=""),
         cex.lab=0.2,
         cex.axis=0.2,
         cex.sub=0.3, font= 1)
    par(cex.axis=1.0)
    
    
    # spc_var_f_copy_L <- get(paste("copy.per.L.",spec.lat, sep=""))
    # # backward leaps
    # leaps_b.out<-regsubsets(spc_var_f_copy_L ~ .  ,
    #                         method = "backward",
    #                         nvmax=3,
    #                         data=edna07r.log.2,nbest=30)
    # # view results 
    # #leaps_b.out.sum<- summary(leaps_b.out)
    # # view results
    # leaps_b.out.sum <- summary(leaps_b.out, matrix.logical=TRUE)
    # 
    
    #https://rstudio-pubs-static.s3.amazonaws.com/2897_9220b21cfc0c43a396ff9abf122bb351.html
    #plot(leaps_b.out, scale = "adjr2", main = paste("Adjusted R^2 backward\n",spec.lat,sep=""), 
    #     cex.lab=0.2, font= 5)
    # detach()
    # end pdf file
    # end file to save as file
    dev.off()
    #open the pdffn aftwerwards
    #cmdstr = paste("open ", pdffn, sep="")
    #system(cmdstr)
    
    ########################################################
    # for loop on species end here for season 1 and 2
    ########################################################
  }
  
  
  
  #______________________________________________________________
  # do this for season 1
  unq_spc.nms <- unique(df_MONIS4_02$spc)
  unq_spc.nms <- unq_spc.nms[!unq_spc.nms %in% c("Acipenser_baerii", 
                                                 "Bonnemaisonia_hamifera",
                                                 "Carassius_auratus",
                                                 "Colpomenia_peregrine",
                                                 "Cordylophora_caspia",
                                                 "Crassostrea_gigas",
                                                 "Cyprinus_carpio",
                                                 "Eriocheir_sinensis",
                                                 "Homarus_americanus",
                                                 "Karenia_mikimotoi",
                                                 "Neogobius_melanostomus",
                                                 "Oncorhynchus_mykiss",
                                                 "Oncorhyncus_gorbuscha",
                                                 "Paralithodes_camtschaticus",#)]#,
                                                 "Pseudochattonella_verruculosa",
                                                 "Rhithropanopeus_harrisii")] 
  # subset dataframe : https://stackoverflow.com/questions/10085806/extracting-specific-columns-from-a-data-frame
  AssID.to.spc <- unique(df_MONIS4_02[,c("spc","AssayIDNo")])
  
  #
  ########################################################
  # for loop start here for season 1
  ########################################################
  
  # loop over all species names in the unique list of species, and make plots. 
  #Notice that the curly bracket ends after the pdf file is closed
  for (spec.lat in unq_spc.nms){
    #print(spec.lat)
    #}
    Ass.no <- AssID.to.spc$AssayIDNo[match(spec.lat, 
                                           as.character(AssID.to.spc$spc))]
    #substitute in names
    nms_1st <- gsub("_.*$","",spec.lat)
    nms_2nd <- gsub("^([^_]*)_([^_]*).*$", "\\2", spec.lat)
    genabb <- substring(nms_1st,1,3)
    spcabb <- substring(nms_2nd,1,3)
    spec.abbrnm <- paste(genabb, spcabb, sep="")
    #copy the data frame to preserve the original data frame
    df_MONIS4_10.2 <- df_MONIS4_10
    #colnames(df_MONIS4_10)
    #grep the columns that has the "copies per L water 
    #plus 1"- i.e.
    # "cpLwp1" and the species name, and the season
    # col.m.q.cop02.eft <- grep(paste("cpLwabLQ.",spec.lat,
    #           ".efteraar", sep=""), 
    #                           names(df_MONIS4_10.2), value = TRUE)
    col.m.q.cop02.for <- grep(paste("cpLwabLQ.",spec.lat,
                                    ".foraar", sep=""), names(df_MONIS4_10.2),
                              value = TRUE)
    #change column name for selected column
    #https://stackoverflow.com/questions/6081439/changing-column-names-of-a-data-frame
    # ti have an unchangeable column name in the leaps function
    #that will follow
    #colnames(df_MONIS4_10.2)[which(names(df_MONIS4_10.2) == col.m.q.cop02.eft)] <- "cce2"
    colnames(df_MONIS4_10.2)[which(names(df_MONIS4_10.2) == col.m.q.cop02.for)] <- "ccf2"
    
    #head(df_MONIS4_10,3)
    pdffn = paste(wd00,wd10,"/suppmatr_10.15b_App_I_",Ass.no,"_rsu_aR2_",spec.abbrnm,"_spring.pdf", sep="")
    pdf(pdffn 
        ,width=6, height=12
        #,width=(1.6*8.2677),height=(1.6*2.9232)
    )
    #plot.new()
    #dev.off()
    par(mfrow=c(2,1)
        , mar=c(0,0,0,0)
        , oma=c(7,0,0,0)
    ) 
    #see this question on how to get the actual values not just the variable name
    #https://stackoverflow.com/questions/5215481/remove-quotes-from-a-character-vector-in-r
    #https://stackoverflow.com/questions/18222286/dynamically-select-data-frame-columns-using-and-a-vector-of-column-names
    #spc.eft <- get(col.m.q.cop02.eft, df_MONIS4_10.2)
    #spc.for <- get(col.m.q.cop02.for, df_MONIS4_10.2)
    #now you can use this in the regsubsets function
    # # forward leaps for autumn
    # leaps_f.out.eft<-leaps::regsubsets(cce2 ~ . ,
    #                                    #+copy.per.L.Mnemiopsis_leidyi ,
    #                                    method = "forward",
    #                                    nvmax=3,
    #                                    data=df_MONIS4_10.2,nbest=12)
    # forward leaps for spring
    leaps_f.out.for<-regsubsets(ccf2 ~ . ,#+copy.per.L.Mnemiopsis_leidyi ,
                                method = "forward",
                                nvmax=3,
                                data=df_MONIS4_10.2,nbest=12)
    
    # view results 
    #summary(leaps_f.out.for)
    
    #https://rstudio-pubs-static.s3.amazonaws.com/2897_9220b21cfc0c43a396ff9abf122bb351.html
    # par(cex.axis=0.4)
    # plot(leaps_f.out.eft, scale = "adjr2", main = paste("Adjusted R^2 forward\n",spec.lat,"_spring",sep=""),
    #      cex.lab=0.2,
    #      cex.axis=0.2,
    #      cex.sub=0.3, font= 1)
    # par(cex.axis=1.0)
    #dev.off()
    par(cex.axis=0.4)
    plot(leaps_f.out.for, scale = "adjr2", 
         main = paste("Adjusted R^2 forward\n",
                      spec.lat,"_foraar",sep=""),
         cex.lab=0.2,
         cex.axis=0.2,
         cex.sub=0.3, font= 1)
    par(cex.axis=1.0)
    
    # backward leaps
    #see this question on how to get the actual values not just the variable name
    #https://stackoverflow.com/questions/5215481/remove-quotes-from-a-character-vector-in-r
    
    #now you can use this in the regsubsets function
    # forward leaps
    # leaps_b.out.eft<-regsubsets(cce2 ~ . ,#+copy.per.L.Mnemiopsis_leidyi ,
    #                             method = "backward",
    #                             nvmax=3,
    #                             data=df_MONIS4_10.2,nbest=12)
    # 
    # # forward leaps
    leaps_b.out.for<-regsubsets(ccf2 ~ . ,#+copy.per.L.Mnemiopsis_leidyi ,
                                method = "backward",
                                nvmax=3,
                                data=df_MONIS4_10.2,nbest=12)
    
    # view results 
    #summary(leaps_b.out.for)
    
    #https://rstudio-pubs-static.s3.amazonaws.com/2897_9220b21cfc0c43a396ff9abf122bb351.html
    # par(cex.axis=0.4)
    # plot(leaps_b.out.eft, scale = "adjr2", main = paste("Adjusted R^2 backw\n",spec.lat,"_efteraar",sep=""),
    #      cex.lab=0.2,
    #      cex.axis=0.2,
    #      cex.sub=0.3, font= 1)
    # par(cex.axis=1.0)
    #dev.off()
    par(cex.axis=0.4)
    plot(leaps_b.out.for, scale = "adjr2", main = paste("Adjusted R^2 backw\n",spec.lat,"_foraar",sep=""),
         cex.lab=0.2,
         cex.axis=0.2,
         cex.sub=0.3, font= 1)
    par(cex.axis=1.0)
    
    
    # spc_var_f_copy_L <- get(paste("copy.per.L.",spec.lat, sep=""))
    # # backward leaps
    # leaps_b.out<-regsubsets(spc_var_f_copy_L ~ .  ,
    #                         method = "backward",
    #                         nvmax=3,
    #                         data=edna07r.log.2,nbest=30)
    # # view results 
    # #leaps_b.out.sum<- summary(leaps_b.out)
    # # view results
    # leaps_b.out.sum <- summary(leaps_b.out, matrix.logical=TRUE)
    # 
    
    #https://rstudio-pubs-static.s3.amazonaws.com/2897_9220b21cfc0c43a396ff9abf122bb351.html
    #plot(leaps_b.out, scale = "adjr2", main = paste("Adjusted R^2 backward\n",spec.lat,sep=""), 
    #     cex.lab=0.2, font= 5)
    # detach()
    # end pdf file
    # end file to save as file
    dev.off()
    #open the pdffn aftwerwards
    #cmdstr = paste("open ", pdffn, sep="")
    #system(cmdstr)
    
    ########################################################
    # for loop on species end here for season 1
    ########################################################
  }
  
  #__________________________________________________________
  
  #______________________________________________________________
  unq_spc.nms <- unique(df_MONIS4_02$spc)
  #unq_spc.nms <- "Bonnemaisonia_hamifera"
  # do this for season 2
  unq_spc.nms <- unq_spc.nms[!unq_spc.nms %in% c("Acipenser_baerii", 
                                                 #"Bonnemaisonia_hamifera",
                                                 "Carassius_auratus",
                                                 #"Colpomenia_peregrine",
                                                 "Cordylophora_caspia",
                                                 "Crassostrea_gigas",
                                                 "Cyprinus_carpio",
                                                 "Eriocheir_sinensis",
                                                 "Homarus_americanus",
                                                 #"Karenia_mikimotoi",
                                                 "Neogobius_melanostomus",
                                                 "Oncorhynchus_mykiss",
                                                 "Oncorhyncus_gorbuscha",
                                                 #"Pseudochattonella_farcimen",
                                                 "Paralithodes_camtschaticus", #)]#,
                                                 #"Pseudochattonella_verruculosa",
                                                 "Rhithropanopeus_harrisii")] 
  # subset dataframe : https://stackoverflow.com/questions/10085806/extracting-specific-columns-from-a-data-frame
  #unq_spc.nms <- "Bonnemaisonia_hamifera"
  AssID.to.spc <- unique(df_MONIS4_02[,c("spc","AssayIDNo")])
  
  #
  ########################################################
  # for loop start here for season 2
  ########################################################
  
  # loop over all species names in the unique list of species, and make plots. 
  #Notice that the curly bracket ends after the pdf file is closed
  for (spec.lat in unq_spc.nms){
    #print(spec.lat)
    #}
    Ass.no <- AssID.to.spc$AssayIDNo[match(spec.lat, 
                                           as.character(AssID.to.spc$spc))]
    
    #substitute in names
    nms_1st <- gsub("_.*$","",spec.lat)
    nms_2nd <- gsub("^([^_]*)_([^_]*).*$", "\\2", spec.lat)
    genabb <- substring(nms_1st,1,3)
    spcabb <- substring(nms_2nd,1,3)
    spec.abbrnm <- paste(genabb, spcabb, sep="")
    #copy the data frame to preserve the original data frame
    df_MONIS4_10.2 <- df_MONIS4_10
    #colnames(df_MONIS4_10)
    #grep the columns that has the "copies per L water 
    #plus 1"- i.e.
    # "cpLwp1" and the species name, and the season
    col.m.q.cop02.eft <- grep(paste("cpLwabLQ.",spec.lat,
                                    ".efteraar", sep=""),
                              names(df_MONIS4_10.2), value = TRUE)
    # col.m.q.cop02.for <- grep(paste("cpLwabLQ.",spec.lat,
    #                                 ".foraar", sep=""), names(df_MONIS4_10.2),
    #                           value = TRUE)
    #change column name for selected column
    #https://stackoverflow.com/questions/6081439/changing-column-names-of-a-data-frame
    # ti have an unchangeable column name in the leaps function
    #that will follow
    colnames(df_MONIS4_10.2)[which(names(df_MONIS4_10.2) == col.m.q.cop02.eft)] <- "cce2"
    # colnames(df_MONIS4_10.2)[which(names(df_MONIS4_10.2) == col.m.q.cop02.for)] <- "ccf2"
    
    #head(df_MONIS4_10,3)
    pdffn = paste(wd00,wd10,"/suppmatr_10.15b_App_I_",Ass.no,"_rsu_aR2_",spec.abbrnm,"_fall.pdf", sep="")
    pdf(pdffn 
        ,width=6, height=12
        #,width=(1.6*8.2677),height=(1.6*2.9232)
    )
    #plot.new()
    #dev.off()
    par(mfrow=c(2,1)
        , mar=c(0,0,0,0)
        , oma=c(7,0,0,0)
    ) 
    #see this question on how to get the actual values not just the variable name
    #https://stackoverflow.com/questions/5215481/remove-quotes-from-a-character-vector-in-r
    #https://stackoverflow.com/questions/18222286/dynamically-select-data-frame-columns-using-and-a-vector-of-column-names
    #spc.eft <- get(col.m.q.cop02.eft, df_MONIS4_10.2)
    #spc.for <- get(col.m.q.cop02.for, df_MONIS4_10.2)
    #now you can use this in the regsubsets function
    # # forward leaps for autumn
    leaps_f.out.eft<-leaps::regsubsets(cce2 ~ . ,
                                       #+copy.per.L.Mnemiopsis_leidyi ,
                                       method = "forward",
                                       nvmax=3,
                                       data=df_MONIS4_10.2,nbest=12)
    #forward leaps for spring
    # leaps_f.out.for<-regsubsets(ccf2 ~ . ,#+copy.per.L.Mnemiopsis_leidyi ,
    #                             method = "forward",
    #                             nvmax=3,
    #                             data=df_MONIS4_10.2,nbest=12)
    # 
    # view results 
    #summary(leaps_f.out.for)
    
    #https://rstudio-pubs-static.s3.amazonaws.com/2897_9220b21cfc0c43a396ff9abf122bb351.html
    
    par(cex.axis=0.4)
    plot(leaps_f.out.eft, scale = "adjr2", 
         main = paste("Adjusted R^2 forward\n",
                      spec.lat,"_efteraar",sep=""),
         cex.lab=0.2,
         cex.axis=0.2,
         cex.sub=0.3, font= 1)
    par(cex.axis=1.0)
    
    # backward leaps
    #see this question on how to get the actual values not just the variable name
    #https://stackoverflow.com/questions/5215481/remove-quotes-from-a-character-vector-in-r
    
    #now you can use this in the regsubsets function
    # forward leaps
    leaps_b.out.eft<-regsubsets(cce2 ~ . ,#+copy.per.L.Mnemiopsis_leidyi ,
                                method = "backward",
                                nvmax=3,
                                data=df_MONIS4_10.2,nbest=12)
    
    # # forward leaps
    # leaps_b.out.for<-regsubsets(ccf2 ~ . ,#+copy.per.L.Mnemiopsis_leidyi ,
    #                             method = "backward",
    #                             nvmax=3,
    #                             data=df_MONIS4_10.2,nbest=12)
    
    # view results 
    #summary(leaps_b.out.for)
    
    #https://rstudio-pubs-static.s3.amazonaws.com/2897_9220b21cfc0c43a396ff9abf122bb351.html
    
    par(cex.axis=0.4)
    plot(leaps_b.out.eft, scale = "adjr2",
         main = paste("Adjusted R^2 backw\n",spec.lat,"_efteraar",sep=""),
         cex.lab=0.2,
         cex.axis=0.2,
         cex.sub=0.3, font= 1)
    par(cex.axis=1.0)
    
    
    #https://rstudio-pubs-static.s3.amazonaws.com/2897_9220b21cfc0c43a396ff9abf122bb351.html
    # end pdf file
    # end file to save as file
    dev.off()
    #open the pdffn aftwerwards
    #cmdstr = paste("open ", pdffn, sep="")
    #system(cmdstr)
    
    ########################################################
    # for loop on species end here for season 2
    ########################################################
  }
  
  
  #__________________________________________________________
  
  
  
  #__________________________________________________________
  
  
  
  
  
  #########################################################
  
  #drop the variables that cannot explain the variable
  drops <- c(
    
    #"conv_rec_val.Colpomenia_peregrine.foraar",
    #"conv_rec_val.Cyprinus_carpio.foraar",
    #"conv_rec_val.Mnemiopsis_leidyi.foraar",
    #"cpLwp1beLD.Neogobius_melanostomus.efteraar",
    #"conv_rec_val.Neogobius_melanostomus.foraar",
    #"conv_rec_val.Oncorhynchus_mykiss.foraar",
    #"cpLwp1beLD.Pseudochattonella_verruculosa.foraar",
    #"cpLwp1beLD.Rhithropanopeus_harrisii.foraar",
    #"godsmaengde_i_1000ton.Udgaaendefaergegodstilindland",
    #"Udgaaende_gods_til_indland_i_1000ton.Raaolie",
    #"Udgaaende_gods_til_indland_i_1000ton.Malmeogmetalaffald",
    #"Indgaaende_gods_fra_indland_i_1000ton.Fastekemikalier",
    #"Udgaaende_gods_til_udland_i_1000ton.Fastekemikalier",
    #"Udgaaende_gods_til_indland_i_1000ton.Fastekemikalier",
    #"Udgaaende_gods_til_indland_i_1000ton.Goedningsstoffer",
    #"Udgaaende_gods_til_indland_i_1000ton.Kalk_cement_gipsmv.",
    "Udgaaende_gods_til_indland_i_1000ton.Trae",
    "Passagerskibe_og_faerge_enhed_millioner_DKK.SKIBEIALT",
    "Lastskibe_enhed_millioner_DKK.250_499BT",
    "Passagerskibe_og_faerge_enhed_millioner_DKK.250_499BT",
    #"Lastskibe_enhed_millioner_DKK.500_1499BT",
    "Passagerskibe_og_faerge_enhed_millioner_DKK.1500_4999BT",
    "Passagerskibe_og_faerge_enhed_millioner_DKK.10000_24999BT",
    #"Koeleskibe",
    "Tankskibe",
    "Stykgodsskibeioevrigt",
    "Krydstogtskibe")#,
    #"cpLwabLQ.Bonnemaisonia_hamifera.efteraar",
    #"cpLwabLQ.Colpomenia_peregrine.efteraar",
    #"cpLwabLQ.Karenia_mikimotoi.efteraar",
    #"cpLwabLQ.Mnemiopsis_leidyi.efteraar",
    #"cpLwabLQ.Mnemiopsis_leidyi.foraar",
    #"cpLwabLQ.Mya_arenaria.efteraar",
    #"cpLwabLQ.Mya_arenaria.foraar",
    #"cpLwabLQ.Prorocentrum_minimum.efteraar",
    #"cpLwabLQ.Prorocentrum_minimum.foraar",
    #"cpLwabLQ.Pseudochattonella_farcimen.efteraar",
    #"cpLwabLQ.Pseudochattonella_farcimen.foraar")#,
    #"cpLwabLQ.Pseudochattonella_verruculosa.efteraar"
    
    
  #colnames(df_MONIS4_10)
  loc_edna15 <- df_MONIS4_10[ , !(names(df_MONIS4_10) %in% drops)]
  #colnames(loc_edna15)
  
  unq_spc.nms <- unique(df_MONIS4_02$spc)
  unq_spc.nms <- unq_spc.nms[!unq_spc.nms %in% c("Acipenser_baerii", 
                                                 "Bonnemaisonia_hamifera",
                                                 "Carassius_auratus",
                                                 "Colpomenia_peregrine",
                                                 "Cordylophora_caspia",
                                                 "Crassostrea_gigas",
                                                 "Cyprinus_carpio",
                                                 "Eriocheir_sinensis",
                                                 "Homarus_americanus",
                                                 "Karenia_mikimotoi",
                                                 "Neogobius_melanostomus",
                                                 "Oncorhynchus_mykiss",
                                                 "Oncorhyncus_gorbuscha",
                                                 "Paralithodes_camtschaticus",#)]#,
                                                 "Pseudochattonella_verruculosa",
                                                 "Rhithropanopeus_harrisii")] 
  # subset dataframe : https://stackoverflow.com/questions/10085806/extracting-specific-columns-from-a-data-frame
  AssID.to.spc <- unique(df_MONIS4_02[,c("spc","AssayIDNo")])
  
  ########################################################
  # for loop start here
  ########################################################
  
  # loop over all species names in the unique list of species, and make plots. 
  #Notice that the curly bracket ends after the pdf file is closed
  for (spec.lat in unq_spc.nms){
    
    Ass.no <- AssID.to.spc$AssayIDNo[match(spec.lat, 
                                           as.character(AssID.to.spc$spc))]
    
    #substitute in names
    nms_1st <- gsub("_.*$","",spec.lat)
    nms_2nd <- gsub("^([^_]*)_([^_]*).*$", "\\2", spec.lat)
    genabb <- substring(nms_1st,1,3)
    spcabb <- substring(nms_2nd,1,3)
    spec.abbrnm <- paste(genabb, spcabb, sep="")
    #grep the columns that has the "copy.per.L." and the species name, and the season
    col.m.q.cop02.eft <- grep(paste("cpLwabLQ.",spec.lat,".efteraar", sep=""), names(loc_edna15), value = TRUE)
    col.m.q.cop02.for <- grep(paste("cpLwabLQ.",spec.lat,".foraar", sep=""), names(loc_edna15), value = TRUE)
  
    #change column name for selected column
    #https://stackoverflow.com/questions/6081439/changing-column-names-of-a-data-frame
    # ti have an unchangeable column name in the leaps function
    #that will follow
    #colnames(loc_edna15)[which(names(loc_edna15) == col.m.q.cop02.for)] <- "ccf2"
    #colnames(loc_edna15)[which(names(loc_edna15) == col.m.q.cop02.eft)] <- "cce2"
    
    pdffn = paste(wd00,wd10,"/suppmatr_10.15d_App_I_",Ass.no,"_rsu_aR2_",spec.abbrnm,"_all_ok_var.pdf", sep="")
    pdf(pdffn 
        ,width=6, height=8
        #,width=(1.6*8.2677),height=(1.6*2.9232)
    )
    #plot.new()
    #dev.off()
    par(mfrow=c(2,2)
        , mar=c(0,0,0,0)
        , oma=c(7,0,0,0)
    ) 
    #see this question on how to get the actual values not just the variable name
    #https://stackoverflow.com/questions/5215481/remove-quotes-from-a-character-vector-in-r
    #https://stackoverflow.com/questions/18222286/dynamically-select-data-frame-columns-using-and-a-vector-of-column-names
    spc.eft <- get(col.m.q.cop02.eft, loc_edna15)
    spc.for <- get(col.m.q.cop02.for, loc_edna15)
    # grep the variables with the species being tested
    drops <- grep(spec.lat, names(loc_edna15), value = TRUE)
    #remonve this species from the data frame
    loc_edna16 <- loc_edna15[ , !(names(loc_edna15) %in% drops)]
    
    #now you can use this in the regsubsets function
    # forward leaps
    leaps_f.out.eft<-regsubsets(spc.eft ~ . ,#+copy.per.L.Mnemiopsis_leidyi ,
                                method = "forward",
                                nvmax=3,
                                data=loc_edna16,nbest=12)
    # forward leaps
    leaps_f.out.for<-regsubsets(spc.for ~ . ,#+copy.per.L.Mnemiopsis_leidyi ,
                                method = "forward",
                                nvmax=3,
                                data=loc_edna16,nbest=12)
    
    # view results 
    #summary(leaps_f.out)
    
    #https://rstudio-pubs-static.s3.amazonaws.com/2897_9220b21cfc0c43a396ff9abf122bb351.html
    par(cex.axis=0.4)
    plot(leaps_f.out.eft, scale = "adjr2", main = paste("Adjusted R^2 forward\n",spec.lat,"_efteraar",sep=""),
         cex.lab=0.2,
         cex.axis=0.2,
         cex.sub=0.3, font= 1)
    par(cex.axis=1.0)
    #dev.off()
    par(cex.axis=0.4)
    plot(leaps_f.out.for, scale = "adjr2", main = paste("Adjusted R^2 forward\n",spec.lat,"_foraar",sep=""),
         cex.lab=0.2,
         cex.axis=0.2,
         cex.sub=0.3, font= 1)
    par(cex.axis=1.0)
    
    # backward leaps
    #see this question on how to get the actual values not just the variable name
    #https://stackoverflow.com/questions/5215481/remove-quotes-from-a-character-vector-in-r
    
    #now you can use this in the regsubsets function
    # forward leaps
    leaps_b.out.eft<-regsubsets(spc.eft ~ . ,#+copy.per.L.Mnemiopsis_leidyi ,
                                method = "backward",
                                nvmax=3,
                                data=loc_edna16,nbest=12)
    # forward leaps
    leaps_b.out.for<-regsubsets(spc.for ~ . ,#+copy.per.L.Mnemiopsis_leidyi ,
                                method = "backward",
                                nvmax=3,
                                data=loc_edna16,nbest=12)
    
    # view results 
    #summary(leaps_f.out)
    
    #https://rstudio-pubs-static.s3.amazonaws.com/2897_9220b21cfc0c43a396ff9abf122bb351.html
    par(cex.axis=0.4)
    plot(leaps_b.out.eft, scale = "adjr2", main = paste("Adjusted R^2 backw\n",spec.lat,"_efteraar",sep=""),
         cex.lab=0.2,
         cex.axis=0.2,
         cex.sub=0.3, font= 1)
    par(cex.axis=1.0)
    #dev.off()
    par(cex.axis=0.4)
    plot(leaps_b.out.for, scale = "adjr2", main = paste("Adjusted R^2 backw\n",spec.lat,"_foraar",sep=""),
         cex.lab=0.2,
         cex.axis=0.2,
         cex.sub=0.3, font= 1)
    par(cex.axis=1.0)
    
    
    # spc_var_f_copy_L <- get(paste("copy.per.L.",spec.lat, sep=""))
    # # backward leaps
    # leaps_b.out<-regsubsets(spc_var_f_copy_L ~ .  ,
    #                         method = "backward",
    #                         nvmax=3,
    #                         data=edna07r.log.2,nbest=30)
    # # view results 
    # #leaps_b.out.sum<- summary(leaps_b.out)
    # # view results
    # leaps_b.out.sum <- summary(leaps_b.out, matrix.logical=TRUE)
    # 
    
    #https://rstudio-pubs-static.s3.amazonaws.com/2897_9220b21cfc0c43a396ff9abf122bb351.html
    #plot(leaps_b.out, scale = "adjr2", main = paste("Adjusted R^2 backward\n",spec.lat,sep=""), 
    #     cex.lab=0.2, font= 5)
    # detach()
    # end pdf file
    # end file to save as file
    dev.off()
    #open the pdffn aftwerwards
    #cmdstr = paste("open ", pdffn, sep="")
    #system(cmdstr)
    
    ########################################################
    # for loop on species end here
    ########################################################
  }
  #########################################################
  
  
  
  
  
  #_________________________________________________________________________________________________
  # GLM test of different explanatory variables for Promin
  #_________________________________________________________________________________________________
  #colnames(loc_edna15)
  M01.Promin <- glm(cpLwabLQ.Prorocentrum_minimum.efteraar~ 
                      #cpLwp1.Mya_arenaria.foraar.x,
                      cpLwabLQ.Mya_arenaria.efteraar,
                    family = gaussian(), data = loc_edna15)
  #summary(M01.Promin)
  M02.Promin <- glm(cpLwabLQ.Prorocentrum_minimum.efteraar~ 
                      cpLwp1beLD.Neogobius_melanostomus.efteraar, # +
                    family = gaussian(), data = loc_edna15)
  #summary(M02.Promin)
  
  M03.Promin <- glm(cpLwabLQ.Prorocentrum_minimum.efteraar~ 
                      Indgaaende_gods_fra_indland_i_1000ton.Fastekemikalier,
                    family = gaussian(), data = loc_edna15)
  #summary(M03.Promin)
  
  M04.Promin <- glm(cpLwabLQ.Prorocentrum_minimum.efteraar~ 
                      cpLwabLQ.Colpomenia_peregrine.efteraar,
                    family = gaussian(), data = loc_edna15)
  #summary(M04.Promin)
  
  
  M05.Promin <- glm(cpLwabLQ.Prorocentrum_minimum.efteraar~ 
                      cpLwabLQ.Pseudochattonella_farcimen.efteraar,
                    family = gaussian(), data = loc_edna15)
  #summary(M05.Promin)
  
  M06.Promin <- glm(cpLwabLQ.Prorocentrum_minimum.foraar~ 
                      conv_rec_val.Oncorhynchus_mykiss.foraar,
                    family = gaussian(), data = loc_edna15)
  #summary(M06.Promin)
  
  M07.Promin <- glm(cpLwabLQ.Prorocentrum_minimum.foraar~ 
                      cpLwabLQ.Pseudochattonella_farcimen.efteraar,
                    family = gaussian(), data = loc_edna15)
  #summary(M07.Promin)
  
  M08.Promin <- glm(cpLwabLQ.Prorocentrum_minimum.foraar~ 
                      Udgaaende_gods_til_indland_i_1000ton.Goedningsstoffer,
                    family = gaussian(), data = loc_edna15)
  #summary(M08.Promin)
  
  M09.Promin <- glm(cpLwabLQ.Prorocentrum_minimum.foraar~ 
                      cpLwabLQ.Bonnemaisonia_hamifera.efteraar,
                    family = gaussian(), data = loc_edna15)
  #summary(M09.Promin)
  
  M10.Promin <- glm(cpLwabLQ.Prorocentrum_minimum.foraar~ 
                      cpLwabLQ.Colpomenia_peregrine.efteraar,
                    family = gaussian(), data = loc_edna15)
  #summary(M10.Promin)
  
  
  M11.Promin <- glm(cpLwabLQ.Prorocentrum_minimum.efteraar~ 
                      cpLwp1beLD.Neogobius_melanostomus.efteraar, # + M02
                    cpLwabLQ.Pseudochattonella_farcimen.efteraar,
                    family = gaussian(), data = loc_edna15)
  #summary(M11.Promin)
  
  M12.Promin <- glm(cpLwabLQ.Prorocentrum_minimum.foraar~ 
                      conv_rec_val.Oncorhynchus_mykiss.foraar,
                    
                    family = gaussian(), data = loc_edna15)
  #summary(M12.Promin)
  
  M13.Promin <- glm(cpLwabLQ.Prorocentrum_minimum.foraar~ 
                      cpLwabLQ.Prorocentrum_minimum.efteraar,
                    family = gaussian(), data = loc_edna15)
  #summary(M13.Promin)
  
  #make a new data frame to collect aic scores
  models.Promin <- as.data.frame(cbind(seq(1:13)))
  models.Promin$M.no <- paste("M",models.Promin$V1,sep="")
  models.Promin$spc <- "Promin"
  #add the aic scores to this data frame
  models.Promin$aic.score <- c(M01.Promin$aic,
                               M02.Promin$aic,
                               M03.Promin$aic,
                               M04.Promin$aic,
                               M05.Promin$aic,
                               M06.Promin$aic,
                               M07.Promin$aic,
                               M08.Promin$aic,
                               M09.Promin$aic,
                               M10.Promin$aic,
                               M11.Promin$aic,
                               M12.Promin$aic,
                               M13.Promin$aic)
  models.Promin
  #M2 is best ? Because AIC is lowest!
  #https://stats.stackexchange.com/questions/129604/is-high-aic-a-bad-feature-of-the-model
  
  
  ####_________________________________________________________________________________________________
  # GLM test of different explanatory variables for Myaare
  #_________________________________________________________________________________________________
  #colnames(loc_edna15)
  M01.Myaare <- glm(cpLwabLQ.Mya_arenaria.foraar~ 
                      cpLwabLQ.Mnemiopsis_leidyi.efteraar, # +
                    #                  cpLwp1.Prorocentrum_minimum.foraar,
                    family = gaussian(), data = loc_edna15)
  #summary(M01.Myaare)
  
  M02.Myaare <- glm(cpLwabLQ.Mya_arenaria.foraar~ 
                      conv_rec_val.Cyprinus_carpio.foraar, # +
                    family = gaussian(), data = loc_edna15)
  #summary(M02.Myaare)
  
  M03.Myaare <- glm(cpLwabLQ.Mya_arenaria.foraar~ 
                      cpLwabLQ.Prorocentrum_minimum.efteraar, # +
                    family = gaussian(), data = loc_edna15)
  #summary(M03.Myaare)
  
  M04.Myaare <- glm(cpLwabLQ.Mya_arenaria.foraar~ 
                      cpLwabLQ.Prorocentrum_minimum.foraar, # +
                    family = gaussian(), data = loc_edna15)
  #summary(M04.Myaare)
  
  
  M05.Myaare <- glm(cpLwabLQ.Mya_arenaria.foraar~ 
                      cpLwabLQ.Mnemiopsis_leidyi.foraar,
                    family = gaussian(), data = loc_edna15)
  #summary(M05.Myaare)
  
  M06.Myaare <- glm(cpLwabLQ.Mya_arenaria.foraar~ 
                      cpLwabLQ.Pseudochattonella_farcimen.foraar ,
                    family = gaussian(), data = loc_edna15)
  #summary(M06.Myaare)
  
  M07.Myaare <- glm(cpLwabLQ.Mya_arenaria.efteraar~ 
                      conv_rec_val.Cyprinus_carpio.foraar,
                    family = gaussian(), data = loc_edna15)
  #summary(M07.Myaare)
  
  M08.Myaare <- glm(cpLwabLQ.Mya_arenaria.efteraar~ 
                      cpLwp1beLD.Neogobius_melanostomus.efteraar,
                    family = gaussian(), data = loc_edna15)
  #summary(M08.Myaare)
  
  M09.Myaare <- glm(cpLwabLQ.Mya_arenaria.efteraar ~ 
                      cpLwabLQ.Bonnemaisonia_hamifera.efteraar,
                    family = gaussian(), data = loc_edna15)
  #summary(M09.Myaare)
  
  M10.Myaare <- glm(cpLwabLQ.Mya_arenaria.efteraar~ 
                      conv_rec_val.Oncorhynchus_mykiss.foraar,
                    family = gaussian(), data = loc_edna15)
  #summary(M10.Myaare)
  
  M11.Myaare <- glm(cpLwabLQ.Mya_arenaria.efteraar~ 
                      Indgaaende_gods_fra_indland_i_1000ton.Fastekemikalier, # +
                    # cpLwp1.Pseudochattonella_farcimen.efteraar.x,
                    family = gaussian(), data = loc_edna15)
  #summary(M11.Myaare)
  
  M12.Myaare <- glm(cpLwabLQ.Mya_arenaria.foraar~ 
                      # conv_rec_val.Colpomenia_peregrine.foraar,
                      # cpLwp1.Pseudochattonella_farcimen.foraar,
                      #cpLwp1.Prorocentrum_minimum.foraar,
                      cpLwabLQ.Mnemiopsis_leidyi.foraar,
                    family = gaussian(), data = loc_edna15)
  #summary(M12.Myaare)
  
  M13.Myaare <- glm(cpLwabLQ.Mya_arenaria.foraar~ 
                      cpLwabLQ.Mya_arenaria.efteraar,
                    
                    family = gaussian(), data = loc_edna15)
  #summary(M13.Myaare)
  #make a new data frame to collect aic scores
  models.Myaare <- as.data.frame(cbind(seq(1:13)))
  models.Myaare$M.no <- paste("M",models.Myaare$V1,sep="")
  models.Myaare$spc <- "Myaare"
  #add the aic scores to this data frame
  models.Myaare$aic.score <- c(M01.Myaare$aic,
                               M02.Myaare$aic,
                               M03.Myaare$aic,
                               M04.Myaare$aic,
                               M05.Myaare$aic,
                               M06.Myaare$aic,
                               M07.Myaare$aic,
                               M08.Myaare$aic,
                               M09.Myaare$aic,
                               M10.Myaare$aic,
                               M11.Myaare$aic,
                               M12.Myaare$aic,
                               M13.Myaare$aic)
  models.Myaare
  #M8 appears to be the best model as it has the lowest AIC score
  
  
  #_________________________________________________________________________________________________
  # GLM test of different explanatory variables for Psefar
  #_________________________________________________________________________________________________
  
  
  #colnames(loc_edna15)
  M01.Psefar <- glm(cpLwabLQ.Pseudochattonella_farcimen.efteraar~ 
                      conv_rec_val.Oncorhynchus_mykiss.foraar,
                    family = gaussian(), data = loc_edna15)
  #summary(M01.Psefar)
  #names(loc_edna15)
  M02.Psefar <- glm(cpLwabLQ.Pseudochattonella_farcimen.efteraar~ 
                      conv_rec_val.Oncorhynchus_mykiss.foraar,
                    family = gaussian(), data = loc_edna15)
  #summary(M02.Psefar)
  
  M03.Psefar <- glm(cpLwabLQ.Pseudochattonella_farcimen.efteraar~ 
                      Udgaaende_gods_til_indland_i_1000ton.Malmeogmetalaffald,
                    family = gaussian(), data = loc_edna15)
  #summary(M03.Psefar)
  
  M04.Psefar <- glm(cpLwabLQ.Pseudochattonella_farcimen.efteraar~ 
                      Udgaaende_gods_til_udland_i_1000ton.Fastekemikalier,
                    family = gaussian(), data = loc_edna15)
  #summary(M04.Psefar)
  
  
  M05.Psefar <- glm(cpLwabLQ.Pseudochattonella_farcimen.efteraar~ 
                      godsmaengde_i_1000ton.Udgaaendefaergegodstilindland,
                    family = gaussian(), data = loc_edna15)
  #summary(M05.Psefar)
  
  M06.Psefar <- glm(cpLwabLQ.Pseudochattonella_farcimen.efteraar~ 
                      cpLwabLQ.Mya_arenaria.efteraar ,
                    family = gaussian(), data = loc_edna15)
  #summary(M06.Psefar)
  
  M07.Psefar <- glm(cpLwabLQ.Pseudochattonella_farcimen.foraar ~ 
                      conv_rec_val.Cyprinus_carpio.foraar,
                    family = gaussian(), data = loc_edna15)
  #summary(M07.Psefar)
  
  M08.Psefar <- glm(cpLwabLQ.Pseudochattonella_farcimen.foraar~ 
                      conv_rec_val.Colpomenia_peregrine.foraar,
                    family = gaussian(), data = loc_edna15)
  #summary(M08.Psefar)
  
  M09.Psefar <- glm(cpLwabLQ.Pseudochattonella_farcimen.foraar~ 
                      cpLwp1beLD.Rhithropanopeus_harrisii.foraar,
                    family = gaussian(), data = loc_edna15)
  #summary(M09.Psefar)
  
  M10.Psefar <- glm(cpLwabLQ.Pseudochattonella_farcimen.foraar~ 
                      cpLwabLQ.Mnemiopsis_leidyi.foraar,
                    family = gaussian(), data = loc_edna15)
  #summary(M10.Psefar)
  
  
  M11.Psefar <- glm(cpLwabLQ.Pseudochattonella_farcimen.foraar~ 
                      Indgaaende_gods_fra_indland_i_1000ton.Fastekemikalier, # +
                    # cpLwp1.Pseudochattonella_farcimen.efteraar,
                    family = gaussian(), data = loc_edna15)
  #summary(M11.Psefar)
  
  M12.Psefar <- glm(cpLwabLQ.Pseudochattonella_farcimen.efteraar~ 
                      # conv_rec_val.Colpomenia_peregrine.foraar,
                      # cpLwp1.Pseudochattonella_farcimen.foraar,
                      # cpLwp1.Prorocentrum_minimum.foraar,
                      cpLwabLQ.Bonnemaisonia_hamifera.efteraar,
                    #cpLwp1.Mya_arenaria.efteraar ,
                    # cpLwp1.Mnemiopsis_leidyi.foraar,
                    family = gaussian(), data = loc_edna15)
  #summary(M12.Psefar)
  
  M13.Psefar <- glm(cpLwabLQ.Pseudochattonella_farcimen.efteraar~
                      cpLwabLQ.Pseudochattonella_farcimen.foraar ,
                    
                    family = gaussian(), data = loc_edna15)
  #summary(M13.Psefar)
  #make a new data frame to collect aic scores
  models.Psefar <- as.data.frame(cbind(seq(1:13)))
  models.Psefar$M.no <- paste("M",models.Psefar$V1,sep="")
  models.Psefar$spc <- "Psefar"
  #add the aic scores to this data frame
  models.Psefar$aic.score <- c(M01.Psefar$aic,
                               M02.Psefar$aic,
                               M03.Psefar$aic,
                               M04.Psefar$aic,
                               M05.Psefar$aic,
                               M06.Psefar$aic,
                               M07.Psefar$aic,
                               M08.Psefar$aic,
                               M09.Psefar$aic,
                               M10.Psefar$aic,
                               M11.Psefar$aic,
                               M12.Psefar$aic,
                               M13.Psefar$aic)
  models.Psefar
  
  #M13 appears to be the best model
  
  #_________________________________________________________________________________________________
  # GLM test of different explanatory variables for Mnelei
  #_________________________________________________________________________________________________
  
  
  #colnames(loc_edna15)
  M01.Mnelei <- glm(cpLwabLQ.Mnemiopsis_leidyi.foraar~ 
                      conv_rec_val.Colpomenia_peregrine.foraar,
                    family = gaussian(), data = loc_edna15)
  #summary(M01.Mnelei)
  #names(loc_edna15)
  M02.Mnelei <- glm(cpLwabLQ.Mnemiopsis_leidyi.foraar~ 
                      cpLwabLQ.Colpomenia_peregrine.efteraar,
                    family = gaussian(), data = loc_edna15)
  #summary(M02.Mnelei)
  
  M03.Mnelei <- glm(cpLwabLQ.Mnemiopsis_leidyi.foraar~ 
                      cpLwp1beLD.Rhithropanopeus_harrisii.foraar,
                    family = gaussian(), data = loc_edna15)
  #summary(M03.Mnelei)
  
  M04.Mnelei <- glm(cpLwabLQ.Mnemiopsis_leidyi.foraar~ 
                      Lastskibe_enhed_millioner_DKK.500_1499BT,
                    family = gaussian(), data = loc_edna15)
  #summary(M04.Mnelei)
  
  M05.Mnelei <- glm(cpLwabLQ.Mnemiopsis_leidyi.efteraar~ 
                      cpLwp1beLD.Rhithropanopeus_harrisii.foraar,
                    family = gaussian(), data = loc_edna15)
  #summary(M05.Mnelei)
  
  M06.Mnelei <- glm(cpLwabLQ.Mnemiopsis_leidyi.efteraar~ 
                      cpLwabLQ.Bonnemaisonia_hamifera.efteraar,
                    family = gaussian(), data = loc_edna15)
  #summary(M06.Mnelei)
  
  M07.Mnelei <- glm(cpLwabLQ.Mnemiopsis_leidyi.efteraar~ 
                      Udgaaende_gods_til_udland_i_1000ton.Fastekemikalier,
                    family = gaussian(), data = loc_edna15)
  #summary(M07.Mnelei)
  
  M08.Mnelei <- glm(cpLwabLQ.Mnemiopsis_leidyi.efteraar~ 
                      cpLwabLQ.Pseudochattonella_verruculosa.efteraar,
                    family = gaussian(), data = loc_edna15)
  #summary(M08.Mnelei)
  
  M09.Mnelei <- glm(cpLwabLQ.Mnemiopsis_leidyi.efteraar~ 
                      godsmaengde_i_1000ton.Udgaaendefaergegodstilindland,
                    family = gaussian(), data = loc_edna15)
  #summary(M09.Mnelei)
  
  M10.Mnelei <- glm(cpLwabLQ.Mnemiopsis_leidyi.efteraar~ 
                      cpLwabLQ.Mnemiopsis_leidyi.foraar,
                    family = gaussian(), data = loc_edna15)
  #summary(M10.Mnelei)
  
  
  M11.Mnelei <- glm(cpLwabLQ.Mnemiopsis_leidyi.efteraar~ 
                      #  cpLwp1.Bonnemaisonia_hamifera.efteraar,
                      cpLwabLQ.Pseudochattonella_farcimen.efteraar,
                    family = gaussian(), data = loc_edna15)
  #summary(M11.Mnelei)
  
  M12.Mnelei <- glm(cpLwabLQ.Mnemiopsis_leidyi.foraar~ 
                      # conv_rec_val.Colpomenia_peregrine.foraar,
                      # cpLwp1.Pseudochattonella_farcimen.foraar,
                      cpLwabLQ.Prorocentrum_minimum.foraar,
                    cpLwabLQ.Mya_arenaria.foraar,
                    # cpLwp1.Bonnemaisonia_hamifera.efteraar,
                    #cpLwp1.Mya_arenaria.efteraar ,
                    # cpLwp1.Mnemiopsis_leidyi.foraar,
                    family = gaussian(), data = loc_edna15)
  #summary(M12.Mnelei)
  #colnames(loc_edna15)
  M13.Mnelei <- glm(cpLwabLQ.Mnemiopsis_leidyi.foraar~
                    #Passagerskibe_og_faerge_enhed_millioner_DKK.1500_4999BT,
                    #Koeleskibe,
                      Udgaaende_gods_til_indland_i_1000ton.Fastekemikalier,
                    #Stykgodsskibeioevrigt,
                    #Krydstogtskibe,
                    family = gaussian(), data = loc_edna15)
  #summary(M13.Mnelei)
  
  M14.Mnelei <- glm(cpLwabLQ.Mnemiopsis_leidyi.foraar~
                      #Passagerskibe_og_faerge_enhed_millioner_DKK.1500_4999BT,
                      Koeleskibe,
                      #Tankskibe,
                    #Stykgodsskibeioevrigt,
                    #Krydstogtskibe,
                    family = gaussian(), data = loc_edna15)
  #summary(M15.Mnelei)
  M15.Mnelei <- glm(cpLwabLQ.Mnemiopsis_leidyi.efteraar~
                      #Passagerskibe_og_faerge_enhed_millioner_DKK.1500_4999BT,
                      Koeleskibe,
                      #Tankskibe,
                      #Stykgodsskibeioevrigt,
                    #Krydstogtskibe,
                    family = gaussian(), data = loc_edna15)
  #summary(M15.Mnelei)
  
  
  #make a new data frame to collect aic scores
  models.Mnelei <- as.data.frame(cbind(seq(1:15)))
  models.Mnelei$M.no <- paste("M",models.Mnelei$V1,sep="")
  models.Mnelei$spc <- "Mnelei"
  #add the aic scores to this data frame
  models.Mnelei$aic.score <- c(M01.Mnelei$aic,
                               M02.Mnelei$aic,
                               M03.Mnelei$aic,
                               M04.Mnelei$aic,
                               M05.Mnelei$aic,
                               M06.Mnelei$aic,
                               M07.Mnelei$aic,
                               M08.Mnelei$aic,
                               M09.Mnelei$aic,
                               M10.Mnelei$aic,
                               M11.Mnelei$aic,
                               M12.Mnelei$aic,
                               M13.Mnelei$aic,
                               M14.Mnelei$aic,
                               M15.Mnelei$aic)
  #M13.Mnelei$aic)
  models.Mnelei
  ####
  # Best model M01.Mnelei$aic
  
  loc_edna15$harbours <- edna11.2.log$harbours
  
  #head(df_MONIS4_02)
  # Best models:
  # M01.Mnelei # $aic : cpLwabLQ.Mnemiopsis_leidyi.foraar ~ conv_rec_val.Colpomenia_peregrine.foraar
  # M02.Promin # $aic : cpLwabLQ.Prorocentrum_minimum.efteraar ~ cpLwp1beLD.Neogobius_melanostomus.efteraar
  # M08.Myaare # $aic : cpLwabLQ.Mya_arenaria.efteraar ~ cpLwp1beLD.Neogobius_melanostomus.efteraar
  # M13.Psefar # $aic : cpLwabLQ.Pseudochattonella_farcimen.efteraar ~ cpLwabLQ.Pseudochattonella_farcimen.foraar
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #  Compare with traffic in Danish harbours - statestikbanken.dk - end
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #  make plots of any eventual correlated variables  - start
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #define path to a previously prepared file
  pth_and_fl <- paste(wd00,wd10,"/suppmatr_10.04c_MONIS4_eDNA_smpls02.1.csv", sep="")
  
  # Read the .csv file
  smpls02.2 <-
    #"MONIS4_eDNA_total_spc_detected_per_harbour.csv"
    read.csv(pth_and_fl, sep = ",",
             stringsAsFactors = FALSE)
  # #copy the data frame
  smpls02.3 <-smpls02.2
  # subset based on variable values in row
  #only keep rows with water sample measurements
  smpls02.4 <- smpls02.3[ which(smpls02.3$WellType=="Unknown"),]
  #make a new variable by paste values together
  smpls02.4$gen_specnm.Harb.season <- paste(smpls02.4$gen_specnm, smpls02.4$Harbour, smpls02.4$season, sep=".")
  #head(df_MONIS4_02)
  # Read the .csv file
  # smpls06.2 <- 
  #   #"MONIS4_eDNA_total_spc_detected_per_harbour.csv"
  #   read.csv("MONIS4_eDNA_smpls06.1.csv", sep = ",",
  #            stringsAsFactors = FALSE)
  smpls06.3 <- df_MONIS4_02
  #make a new variable by paste values together
  smpls06.3$spc.Harbour.season <- paste(smpls06.3$spc.Harbour, smpls06.3$season, sep=".")
  #match the assay number to the data frame with species
  smpls02.4$LOD <- smpls06.3$LOD[match(smpls02.4$gen_specnm.Harb.season, smpls06.3$spc.Harbour.season)]
  smpls02.4$LOQ <- smpls06.3$LOQ[match(smpls02.4$gen_specnm.Harb.season, smpls06.3$spc.Harbour.season)]
  smpls02.4$conv_rec_val <- smpls06.3$conv_rec_val[match(smpls02.4$gen_specnm.Harb.season, smpls06.3$spc.Harbour.season)]
  smpls02.4$declat <- smpls06.3$declat[match(smpls02.4$gen_specnm.Harb.season, smpls06.3$spc.Harbour.season)]
  smpls02.4$declon <- smpls06.3$declon[match(smpls02.4$gen_specnm.Harb.season, smpls06.3$spc.Harbour.season)]
  #make new column by pasting columns together
  smpls02.4$spc.Harbour <- paste(smpls02.4$gen_specnm, smpls02.4$Harbour, sep=".")
  smpls02.4$Harbour.season <- paste(smpls02.4$Harbour, smpls02.4$season, sep=".")
  #head(smpls02.4)
  #keep only selected columns
  keeps <- c(
    "gen_specnm",
    "Harbour.season",
    "log.10_copies_L",
    "conv_rec_val")
  smpls02.5 <- smpls02.4[keeps]
  #paste columns together
  smpls02.5$Harbour.season.gen_specnm <- paste(smpls02.4$Harbour.season, smpls02.4$gen_specnm, sep=".")
  #write.csv(smpls02.5, file = "smpls02.5.csv")
  #reshape the data frame from long to wide
  
  #Numbering rows within groups in a data frame
  #https://stackoverflow.com/questions/12925063/numbering-rows-within-groups-in-a-data-frame
  
  
  # devtools::install_github("r-lib/rlang", build_vignettes = TRUE)
  # if(!require(dplyr)){
  #   install.packages("dplyr", dependencies = TRUE)
  #   library(dplyr)
  # }
  # library(dplyr)
  
  # Because I still am running R v3.3.1 I have difficulties getting the newest versions of various R
  # Packages. This means I cannot install the 'dplyr' package correctly as I cannot get
  # the latest version of 'rlang' installed, and 'dplyr' is dependent on 'rlang'
  # Instead I found a solution to count observations using base package in R
  #https://stackoverflow.com/questions/8209015/observation-number-by-group?noredirect=1&lq=1
  
  
  # smpls02.5 <- smpls02.5[order(smpls02.5$Harbour.season),]
  # colnames(smpls02.5)
  # smpls02.5$cnt <- do.call(c, lapply(unique(smpls02.5$Harbour.season.gen_specnm), function(curf){
  #   seq(sum(smpls02.5$Harbour.season.gen_specnm==curf))
  # }))
  # smpls02.5$Harbour.season.gen_specnm.cnt <- paste(smpls02.5$Harbour.season.gen_specnm, smpls02.5$cnt, sep=".")
  # smpls02.5$id <- smpls02.5$cnt
  
  smpls02.6 <- smpls02.5 %>% dplyr::group_by(Harbour.season.gen_specnm) %>% dplyr::mutate(id = row_number())
  smpls02.7 <- as.data.frame(smpls02.6)
  
  #smpls02.7 <- smpls02.6
  smpls02.7$Harbour.season.gen_specnm.id <- paste(smpls02.7$Harbour.season.gen_specnm, smpls02.7$id, sep=".")
  smpls02.7$Harbour.season.id <- paste(smpls02.7$Harbour.season, smpls02.7$id, sep=".")
  #because of an error in the qPCR ararngement of the set up,
  #Pseudochattonella_farcimen
  #Pseudochattonella_verruculosa
  # Are listed twice for each well
  # These wells are to be summed first before the dataframe can be reshaped from long to wide
  # The id number assigned can help identify these multiple occurences
  # i.e. for Pseudochattonella_farcimen in foraar id 1 and id 4 are to be summed
  # i.e. for Pseudochattonella_farcimen in efteraar id 1 and id 4 are to be summed
  # i.e. for Pseudochattonella_farcimen in foraar id 2 and id 5 are to be summed
  # and so on
  
  #but this might be an artefact of a previously loaded csv-data file
  #because I cannot recreate the double recordings of these two species:
  #Pseudochattonella_farcimen
  #Pseudochattonella_verruculosa
  # I have now commented out the part below 
  
  # # :::::::: section below to merge multiple recordings of :::::::: start
  # #Pseudochattonella_farcimen
  # #Pseudochattonella_verruculosa
  # # :::::::: ::::::::::::::::::::::::::::::::::::::::::::: :::::::: start
  # #subset the data frame by excluding rows that match the two criteria
  # #https://stackoverflow.com/questions/4935479/how-to-combine-multiple-conditions-to-subset-a-data-frame-using-or
  # smpls02.8 <- subset(smpls02.7, !grepl("Pseudochattonella_farcimen", smpls02.7$gen_specnm) & 
  #          !grepl("Pseudochattonella_verruculosa", smpls02.7$gen_specnm))
  # 
  # #Do the add up of values for "Pseudochattonella_farcimen" 
  # #subset the dataframe by matching two criteria
  # smpls02.Pf.1 <- subset(smpls02.7, smpls02.7$gen_specnm=="Pseudochattonella_farcimen"  &
  #                       smpls02.7$id==1)
  # #make a copy of this dataframe
  # smpls02.Pf.1.2 <- smpls02.Pf.1
  # #subset the dataframe by matching two criteria
  # smpls02.Pf.4 <- subset(smpls02.7, smpls02.7$gen_specnm=="Pseudochattonella_farcimen"  &
  #                          smpls02.7$id==4)
  # # add the two columns
  # smpls02.Pf.1$log.10_copies_L <- as.numeric(as.character(smpls02.Pf.1$log.10_copies_L))
  # smpls02.Pf.4$log.10_copies_L <- as.numeric(as.character(smpls02.Pf.4$log.10_copies_L))
  # 
  # 
  # smpls02.Pf.1.n <- smpls02.Pf.1$log.10_copies_L + smpls02.Pf.4$log.10_copies_L
  # #subset the dataframe by matching two criteria
  # smpls02.Pf.2 <- subset(smpls02.7, smpls02.7$gen_specnm=="Pseudochattonella_farcimen"  &
  #                          smpls02.7$id==2)
  # #make a copy of this dataframe
  # smpls02.Pf.2.2 <- smpls02.Pf.2
  # #subset the dataframe by matching two criteria
  # smpls02.Pf.5 <- subset(smpls02.7, smpls02.7$gen_specnm=="Pseudochattonella_farcimen"  &
  #                          smpls02.7$id==5)
  # # add the two columns
  # smpls02.Pf.2.n <- smpls02.Pf.2$log.10_copies_L + smpls02.Pf.5$log.10_copies_L
  # #subset the dataframe by matching two criteria
  # smpls02.Pf.3 <- subset(smpls02.7, smpls02.7$gen_specnm=="Pseudochattonella_farcimen"  &
  #                          smpls02.7$id==3)
  # #make a copy of this dataframe
  # smpls02.Pf.3.2 <- smpls02.Pf.3
  # #subset the dataframe by matching two criteria
  # smpls02.Pf.6 <- subset(smpls02.7, smpls02.7$gen_specnm=="Pseudochattonella_farcimen"  &
  #                          smpls02.7$id==6)
  # # add the two columns
  # smpls02.Pf.3.n <- smpls02.Pf.3$log.10_copies_L + smpls02.Pf.6$log.10_copies_L
  # #replace columns in the previous data frames
  # smpls02.Pf.1.2$log.10_copies_L <- smpls02.Pf.1.n
  # smpls02.Pf.2.2$log.10_copies_L <- smpls02.Pf.2.n
  # smpls02.Pf.3.2$log.10_copies_L <- smpls02.Pf.3.n
  # #combine the rows in these 3 dataframes
  # smpls02.Pf.repl <- rbind(smpls02.Pf.1.2, smpls02.Pf.2.2, smpls02.Pf.3.2)
  # 
  # #Do the add up of values for "Pseudochattonella_verruculosa" 
  # #subset the dataframe by matching two criteria
  # smpls02.Pv.1 <- subset(smpls02.7, smpls02.7$gen_specnm=="Pseudochattonella_verruculosa"  &
  #                          smpls02.7$id==1)
  # #make a copy of this dataframe
  # smpls02.Pv.1.2 <- smpls02.Pv.1
  # #subset the dataframe by matching two criteria
  # smpls02.Pv.4 <- subset(smpls02.7, smpls02.7$gen_specnm=="Pseudochattonella_verruculosa"  &
  #                          smpls02.7$id==4)
  # # add the two columns
  # smpls02.Pv.1.n <- smpls02.Pv.1$log.10_copies_L + smpls02.Pv.4$log.10_copies_L
  # #subset the dataframe by matching two criteria
  # smpls02.Pv.2 <- subset(smpls02.7, smpls02.7$gen_specnm=="Pseudochattonella_verruculosa"  &
  #                          smpls02.7$id==2)
  # #make a copy of this dataframe
  # smpls02.Pv.2.2 <- smpls02.Pv.2
  # #subset the dataframe by matching two criteria
  # smpls02.Pv.5 <- subset(smpls02.7, smpls02.7$gen_specnm=="Pseudochattonella_verruculosa"  &
  #                          smpls02.7$id==5)
  # # add the two columns
  # smpls02.Pv.2.n <- smpls02.Pv.2$log.10_copies_L + smpls02.Pv.5$log.10_copies_L
  # #subset the dataframe by matching two criteria
  # smpls02.Pv.3 <- subset(smpls02.7, smpls02.7$gen_specnm=="Pseudochattonella_verruculosa"  &
  #                          smpls02.7$id==3)
  # #make a copy of this dataframe
  # smpls02.Pv.3.2 <- smpls02.Pv.3
  # #subset the dataframe by matching two criteria
  # smpls02.Pv.6 <- subset(smpls02.7, smpls02.7$gen_specnm=="Pseudochattonella_verruculosa"  &
  #                          smpls02.7$id==6)
  # # add the two columns
  # smpls02.Pv.3.n <- smpls02.Pv.3$log.10_copies_L + smpls02.Pv.6$log.10_copies_L
  # #replace columns in the previous data frames
  # smpls02.Pv.1.2$log.10_copies_L <- smpls02.Pv.1.n
  # smpls02.Pv.2.2$log.10_copies_L <- smpls02.Pv.2.n
  # smpls02.Pv.3.2$log.10_copies_L <- smpls02.Pv.3.n
  # #combine the rows in these 3 dataframes
  # smpls02.Pv.repl <- rbind(smpls02.Pv.1.2, smpls02.Pv.2.2, smpls02.Pv.3.2)
  # 
  # #append the two new data frames for these two species back to the dataframe where they were removed from
  # smpls02.9 <- rbind(smpls02.8, smpls02.Pf.repl, smpls02.Pv.repl)
  # # :::::::: section below to merge multiple recordings of :::::::: end
  # #Pseudochattonella_farcimen
  # #Pseudochattonella_verruculosa
  # # :::::::: ::::::::::::::::::::::::::::::::::::::::::::: :::::::: end
  
  #copy the smpls02.7 into a new variable as the section above is not needed
  smpls02.9 <- smpls02.7
  #keep only selected columns
  keeps <- c(
    "gen_specnm",
    "Harbour.season.id",
    "log.10_copies_L")#,
  #"conv_rec_val")
  smpls02.10 <- smpls02.9[keeps]
  
  
  #reshape the data frame from long to wide - it works now because the 'idvar = "Harbour.season.id"' is unique,
  #also it works because the errors with "Pseudochattonella_verruculosa" and "Pseudochattonella_farcimen"  has been sorted
  smpls02.11 <- reshape(smpls02.10, idvar = "Harbour.season.id", timevar = "gen_specnm", direction = "wide")
  #replace NA with zero
  smpls02.11[is.na(smpls02.11)] <- 0
  #write.csv(smpls02.11, file = "smpls02.11.csv")
  #copy the dataframe
  smpls02.12 <- smpls02.11
  #https://stackoverflow.com/questions/43077846/how-to-replace-second-or-more-occurrences-of-a-dot-from-a-column-name
  #replace everything after the second dot - escape the first dot and everything up until the second dot
  smpls02.12$Harbour.season <- gsub("(\\..*?)\\..", "\\1", smpls02.12$Harbour.season.id)
  #delete the old column
  smpls02.12$Harbour.season.id <-NULL
  #get the number of columns, the column where the aggregating variable is the last column
  nc.sp02.12 <- ncol(smpls02.12)
  #take an average of the groups: see this website: https://stackoverflow.com/questions/21982987/mean-per-group-in-a-data-frame
  smpls02.13 <- aggregate(smpls02.12[, 1:(nc.sp02.12-1)], list(smpls02.12$Harbour.season), mean)
  #replace column name in a selected column
  colnames(smpls02.13)[1] <- "Harbour.season"
  # delete columns with all zeroes
  smpls02.13 <- smpls02.13[, colSums(smpls02.13 != 0) > 0]
  
  #count the number of columns
  nc.sp02.13 <- ncol(smpls02.13)
  #get all column names except the first column
  colnmsp02.13 <- colnames(smpls02.13)[2:nc.sp02.13]
  # grep for regex in column names : see this website : https://stackoverflow.com/questions/24561936/grep-to-search-column-names-of-a-dataframe
  colnmsp02.13.l10 <- grep("log.10_copies_L", colnmsp02.13, value = TRUE)
  #define the column name that is to be dropped from the data frame
  drops <- "Harbour.season"
  #remove these columns from the data frame
  smpls02.14 <- smpls02.13[ , !(names(smpls02.13) %in% drops)]
  #store the column with harbour names in variable
  harb.seas02.13 <- smpls02.13[ , (names(smpls02.13) %in% drops)]
  
  #Remove highly correlated variables, more than 0.9 correlated
  #https://stackoverflow.com/questions/18275639/remove-highly-correlated-variables
  tmp <- cor(smpls02.14,use="complete.obs")
  tmp[upper.tri(tmp)] <- 0
  diag(tmp) <- 0
  # Above two commands can be replaced with 
  # tmp[!lower.tri(tmp)] <- 0
  # setting the 'any(abs(x) > 0.90' part closer to 1.00 means that more correlated columna are allowed
  # setting the 'any(abs(x) > 0.90' part closer to 0.00 means that fewer correlated columna are allowed
  smpls02.15 <- smpls02.14[,!apply(tmp,2,function(x) any(abs(x) > 0.95))]
  #smpls02.15 <- smpls02.14[,!apply(tmp,2,function(x) any(abs(x) > 0.10))]
  #smpls02.14[,!apply(tmp,2,function(x) any(abs(x) > 0.8))]
  #ncol(smpls02.15)
  
  
  
  #colnmsp02.13.l10 <- "log.10_copies_L.Pseudochattonella_verruculosa"
  #nm <- "log.10_copies_L.Pseudochattonella_verruculosa"
  
  
  #drop the variables that cannot explain the variable
  drops <- c(#"log.10_copies_L.Bonnemaisonia_hamifera",
    "log.10_copies_L.Neogobius_melanostomus",#)#,
    "log.10_copies_L.Crassostrea_gigas",
    "log.10_copies_L.Mya_arenaria")
  #remove elements from character vector
  colnmsp02.14.l10 <- colnmsp02.13.l10
  colnmsp02.14.l10 <- colnmsp02.13.l10[!colnmsp02.13.l10 %in% drops]
  # loop over column names
  for (nm in colnmsp02.14.l10){
    print(nm)
    #print(nm)
  #}  
    # substitute and get only 3 first letters inn string
    genspcnm <- sub(".*\\.*\\.","",nm)
    genus <- sub("_.*","",genspcnm)
    spcnm <- sub("^((?:.*?_){1}.*?)_","",nm)
    tl.gen <- substr(genus,1, 3)
    tl.spcnm <- substr(spcnm,1, 3)
    #paste together to make a new variable name
    nm2 <- paste("log10.copy.per.L.",tl.gen,tl.spcnm, sep="")
  
     pdffn = paste(wd00,wd10,"/suppmatr_10.15e_App_I_rsu_aR2_",nm2,".pdf", sep="")
    pdf(pdffn 
        ,width=6, height=8
        #,width=(1.6*8.2677),height=(1.6*2.9232)
    )
    par(mfrow=c(2,1)
        , mar=c(1,1,1,1)
        , oma=c(1,1,1,1)
    )
    
    #drop the current column name 
    drops <- grep(nm, names(smpls02.15), value = TRUE)
    #remove these columns from the data frame
    smpls02.16 <- smpls02.15[ , !(names(smpls02.15) %in% drops)]
    #get the actual column contents instead of just the variable name
    #see this question on how to get the actual values not just the variable name
    #https://stackoverflow.com/questions/5215481/remove-quotes-from-a-character-vector-in-r
    #https://stackoverflow.com/questions/18222286/dynamically-select-data-frame-columns-using-and-a-vector-of-column-names
    val.in.col.nm <- get(nm, smpls02.15)
    # forward leaps
    leaps_f.out<-regsubsets(val.in.col.nm ~ .,
                            method = "forward",
                            nvmax=3,
                            data=smpls02.16,nbest=1)
    # backward leaps
    leaps_b.out<-regsubsets(val.in.col.nm ~ . ,
                            method = "backward",
                            nvmax=3,
                            data=smpls02.16,nbest=1)
    
    # view results 
    #summary(leaps_f.out)
    
    #https://rstudio-pubs-static.s3.amazonaws.com/2897_9220b21cfc0c43a396ff9abf122bb351.html
    #as.data.frame(leaps_b_PLEPLA.out$outmat)
    plot(leaps_f.out, scale = "adjr2", main = 
           paste("Adjusted R^2 forward\n",nm, sep=""), 
         cex.lab=0.6, font= 4)
    plot(leaps_b.out, scale = "adjr2", main = 
           paste("Adjusted R^2 backward\n",nm, sep=""), 
         cex.lab=0.6, font= 4)
    
    # end pdf file
    # end file to save as file
    dev.off()
  }
  ####################################################################################
  ####################################################################################
  
  
  
  ####################################################################################
  # make a data frame with colors and variables from the leaps plots
  ####################################################################################
  #get all the column headers with log10 levels and place in a list
  eDNA.l10.lvls.nms <- colnames(smpls02.15)
  # get the number of elements
  eDNA.l10.lvls.ct <- length(eDNA.l10.lvls.nms)
  
  #get the most distinctive colors 1st attempt
  #https://stackoverflow.com/questions/15282580/how-to-generate-a-number-of-most-distinctive-colors-in-r
  #install package if required
  if(!require(RColorBrewer)){
    install.packages("RColorBrewer")
    library(RColorBrewer)
  }
  library(RColorBrewer)
  
  #n <- 12
  n <- eDNA.l10.lvls.ct
  qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
  col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  pie(rep(1,n), col=sample(col_vector, n))
  
  #install package if required
  if(!require(curl)){
    install.packages("curl")
    library(curl)
  }
  library(curl)
  #install package if required
  if(!require(randomcoloR)){
    install.packages("randomcoloR")
    library(randomcoloR)
  }
  library(randomcoloR)
  #this gave an error message: 'there is no package called ‘curl’'
  # So I had to try and install 'curl' -  as I have done above
  
  #get the most distinctive colors 2nd attempt
  n <- eDNA.l10.lvls.ct
  palette <- distinctColorPalette(n)
  pie(rep(1, n), col=palette)
  #n <- 13
  n <- eDNA.l10.lvls.ct
  palette <- distinctColorPalette(n)
  pie(rep(1, n), col=palette)
  #see colors as hexadecimal codes
  palette
  
  #predictors inferred by the leaps package
  #I put this together manually from the leaps results for 
  #season : spring
  va01 <- c("log.10_copies_L.Bonnemaisonia_hamifera", "log.10_copies_L.Colpomenia_peregrine", "log.10_copies_L.Cyprinus_carpio")
  va02 <- c("log.10_copies_L.Prorocentrum_minimum", "log.10_copies_L.Cyprinus_carpio", "log.10_copies_L.Neogobius_melanostomus")
  va03 <- c("log.10_copies_L.Pseudochattonella_farcimen", "log.10_copies_L.Pseudochattonella_verruculosa", "log.10_copies_L.Mnemiopsis_leidyi")
  va04 <- c("log.10_copies_L.Pseudochattonella_verruculosa", "log.10_copies_L.Pseudochattonella_farcimen", "log.10_copies_L.Karenia_mikimotoi")
  va05 <- c("log.10_copies_L.Karenia_mikimotoi", "log.10_copies_L.Pseudochattonella_verruculosa", "log.10_copies_L.Crassostrea_gigas")
  va06 <- c("log.10_copies_L.Cyprinus_carpio", "log.10_copies_L.Bonnemaisonia_hamifera", "log.10_copies_L.Prorocentrum_minimum")
  va07 <- c("log.10_copies_L.Colpomenia_peregrine", "log.10_copies_L.Pseudochattonella_farcimen", "log.10_copies_L.Mnemiopsis_leidyi")
  va08 <- c("log.10_copies_L.Neogobius_melanostomus", "none", "none")
  va09 <- c("log.10_copies_L.Oncorhynchus_mykiss", "log.10_copies_L.Pseudochattonella_verruculosa", "log.10_copies_L.Crassostrea_gigas")
  va10 <- c("log.10_copies_L.Crassostrea_gigas", "none", "none")
  va11 <- c("log.10_copies_L.Mya_arenaria", "none", "none")
  va12 <- c("log.10_copies_L.Rhithropanopeus_harrisii", "log.10_copies_L.Prorocentrum_minimum", "log.10_copies_L.Pseudochattonella_verruculosa")
  va13 <- c("log.10_copies_L.Mnemiopsis_leidyi", "none", "none")
  
  lps.vars_df <- as.data.frame(rbind(va01, va02, va03, va04,
                                     va05, va06, va07, va08, 
                                     va09, va10, va11, va12,
                                     va13))
  # 
  #n <- 13
  n <- nrow(lps.vars_df)
  palette <- distinctColorPalette(n)
  pie(rep(1, n), col=palette)
  #see colors as hexadecimal codes
  palette
  #add this as a new column to the dataframe
  lps.vars_df$cols <- palette
  #Conditionally Remove Dataframe Rows with R
  #https://stackoverflow.com/questions/8005154/conditionally-remove-dataframe-rows-with-r
  # the V2 column and the v3 column will not work with "none"
  lps.vars_df<-lps.vars_df[!(lps.vars_df$V2=="none" | lps.vars_df$V3=="none"),]
  #make a vector with characters from the variable in the dataframe
  lps.v1 <- as.vector(as.character(lps.vars_df$V1))
  #make empty lists to collect AIC scores
  aic.2prdict <- list()
  aic.3prdict <- list()
  aic.4prdict <- list()
  #use the lines below to check the effect of the loop on individual elements
  #nm <- "log.10_copies_L.Bonnemaisonia_hamifera" #comment out if not needed
  #nm <- "log.10_copies_L.Colpomenia_peregrine" #comment out if not needed
  #use the lines below to check the effect of the loop on individual elements
  #lps.v1 <- "log.10_copies_L.Colpomenia_peregrine" #comment out if not needed
  
  
  #loop over these elements in this vector
  for (nm in lps.v1){
    #print the character 
    print(nm)
  #}
      #use match to get the matching value in one of the other columns
    V2.nm <- as.character(lps.vars_df$V2[match(nm, lps.vars_df$V1)])
    V3.nm <- as.character(lps.vars_df$V3[match(nm, lps.vars_df$V1)])
    col.f.pts1 <- as.character(lps.vars_df$cols[match(nm, lps.vars_df$V1)])
    
    
    ####################################################################################
    #
    # Producing 3D plot for surface of revolution (a GLM logistic curve example)
    #https://rstudio-pubs-static.s3.amazonaws.com/68452_ed50146f1ca24b568d4cfbd6db3b58d6.html
    #
    ####################################################################################
    #}
    #see the column headers
    #colnames(smpls02.15)
    #############____ BEGIN ____ plot 3D ______________
    # substitute and get only 3 first letters inn string
    genspcnm <- sub(".*\\.*\\.","",nm)
    genus <- sub("_.*","",genspcnm)
    spcnm <- sub("^((?:.*?_){1}.*?)_","",nm)
    tl.gen <- substr(genus,1, 3)
    tl.spcnm <- substr(spcnm,1, 3)
    #match to get Assay ID no
    assIdno <- AssID.to.spc$AssayIDNo[match(genspcnm,AssID.to.spc$spc)]
    #paste together to make a new variable name
    nm2 <- paste(tl.gen,tl.spcnm, sep="")
    #make a name for the files to be saved
    pdffn = paste(wd00,wd10,"/suppmatr_10.15f_App_J_eDNA_AssId",assIdno,"_",nm2,"_vs_2oth_pred.pdf", sep="")
    pdf(pdffn 
        ,width=6, height=1.5*8
        #,width=(1.6*8.2677),height=(1.6*2.9232)
    )
    par(mfrow=c(3,1) # c(2,1) will give 2 rows and 1 column
        , mar=c(4,4,1,1)
        , oma=c(2,2,1,1)
    ) 
    
    #define axis labels to use
    #these will be the variables you will use through out the 3D plot
    #this means you only need to modify the names of the columns here in this part right here below
    #this is the first predictor
    #in.su1.nm <- "log.10_copies_L.Cyprinus_carpio"
    #in.su1.nm <- "log.10_copies_L.Pseudochattonella_farcimen"
    in.su1.nm <- V2.nm  
    #this is the second predictor
    #in.su2.nm <- "log.10_copies_L.Colpomenia_peregrine"
    #in.su2.nm <- "log.10_copies_L.Mnemiopsis_leidyi"
    in.su2.nm <- V3.nm 
    #NOTE The third variable is the variable that is to be tested against the two other predictors
    #in.su3.nm <- "log.10_copies_L.Bonnemaisonia_hamifera"
    #in.su3.nm <- "log.10_copies_L.Colpomenia_peregrine" 
    in.su3.nm <- nm
    #define the background colours to be used for the points above and below the surface plane
    #notice that this color is defined by the dataframe and the palette above
    fillcol01 <- "green"
    fillcol01 <- col.f.pts1
    #get the actual column contents instead of just the variable name
    #see this question on how to get the actual values not just the variable name
    #https://stackoverflow.com/questions/5215481/remove-quotes-from-a-character-vector-in-r
    #https://stackoverflow.com/questions/18222286/dynamically-select-data-frame-columns-using-and-a-vector-of-column-names
    val.in.su1.nm <- get(in.su1.nm, smpls02.15)
    val.in.su2.nm <- get(in.su2.nm, smpls02.15)
    val.in.su3.nm <- get(in.su3.nm, smpls02.15)
    #colnames(smpls02.16)
    
    #make a 1st plot
    #make a model with the 2 best predictors inferred from the leaps set up
    M_spcabbr <- glm(val.in.su3.nm ~ 
                       val.in.su1.nm +
                       val.in.su2.nm, # +
                     family = gaussian(), data = smpls02.15)
    #summary(M_spcabbr)
    #collect the AIC score for the model
    aic.val01 <- M_spcabbr$aic
    #add the aic score to the empty list above : see : https://stackoverflow.com/questions/27153263/adding-elements-to-a-list-in-for-loop-in-r
    aic.2prdict[[nm]] <- aic.val01
    #use the model
    model <- M_spcabbr
    x1 <- smpls02.15[,in.su1.nm]
    x2 <- smpls02.15[,in.su2.nm]
    # ## Set range and domain of plot
    in.su1  <- seq(min(x1), max(x1), length.out = 25);
    in.su2  <- seq(min(x2), max(x2), length.out = 25);
    # ## Interpolate surface
    in.su3  <- outer(in.su1,
                     in.su2,
                     function(in.su1,
                              in.su2){predict(model, 
                                              data.frame(val.in.su1.nm=in.su1, 
                                                         val.in.su2.nm=in.su2))});
    
    #plot the perspective
    p  <- persp(in.su1,
                in.su2,
                in.su3, 
                theta = -30, phi = 30,
                col = "lightblue", # set the colour on the surface plane
                shade = 0.2, ticktype = "detailed",
                xlab=in.su1.nm, # use the labels for the axis defined above
                ylab=in.su2.nm, # use the labels for the axis defined above
                zlab=in.su3.nm) # use the labels for the axis defined above
    # 
    yobs <- val.in.su3.nm
    obs  <- trans3d(x1, x2, yobs, p);
    pred  <- trans3d(x1, x2, fitted(model), p);
    points(obs, col = "black", # set outline color on point
           bg=fillcol01,# "blue" # set background color on points
           cex=1.6, pch = 21);
    segments(obs$x, obs$y, pred$x, pred$y)
    #round the AIC score -  to be presented later on in plot legend
    aic.val01r <- round(aic.val01,2)
    #Add a legend
    legend("bottomright", legend=c(paste("AIC score: ",aic.val01r,sep="")),
           cex=0.8)
    
    #make a 2nd plot
    #make a model 
    M_spcabbr2 <- glm(val.in.su3.nm ~ 
                        val.in.su1.nm,
                      family = gaussian(), data = smpls02.15)
    #collect the aic score from the summary
    aic.val02 <- M_spcabbr2$aic
    #add the aic score to the empty list above : see : https://stackoverflow.com/questions/27153263/adding-elements-to-a-list-in-for-loop-in-r
    aic.3prdict[[nm]] <- aic.val02
    #round the AIC score -  to be presented later on in plot legend
    aic.val02r <- round(aic.val02,2)
    #get the maximum value of the predictor
    mp <- max(val.in.su1.nm)
    #round up this value
    mp<- ceiling(mp) 
    #make a series of numbers - see this website: https://www.theanalysisfactor.com/r-glm-plotting/
    xw <- seq(0, mp, 0.01)
    #predict a y value
    yw <- predict(M_spcabbr2, list(val.in.su1.nm = xw),type="response")
    #make a plot comparing only one predictor
    plot(val.in.su1.nm,val.in.su3.nm,
         bg=fillcol01,
         cex=1.6, pch = 21,
         xlab=in.su1.nm, # use the labels for the axis defined above
         ylab=in.su3.nm # use the labels for the axis defined above
    )
    #add a line from the model
    lines(xw, yw)
    #plot(xw, yw)
    #plot confidence interval - see: https://rpubs.com/Bio-Geek/71339
    M_spcabbr2.1 <- lm(val.in.su3.nm ~ val.in.su1.nm, data=smpls02.15)
    conf_interval <- predict(M_spcabbr2.1, newdata=data.frame(val.in.su1.nm=xw), interval="confidence",
                             level = 0.95, scale =0.95, type="response")
    #conf_interval
    lines(xw, conf_interval[,2], col="black", lty=2)
    lines(xw, conf_interval[,3], col="black", lty=2)
    # Add a legend
    legend("topleft", legend=c("regression", "95% confidence interv"),
           col=c("black", "black"), lty=1:2, cex=0.8)
    legend("topright", legend=c(paste("AIC score: ",aic.val02r,sep="")),
           cex=0.8)
    
    #make a 3rd plot
    #make a model 
    M_spcabbr3 <- glm(val.in.su3.nm ~ 
                        val.in.su2.nm,
                      family = gaussian(), data = smpls02.15)
    #collect the aic score from the summary
    aic.val03 <- M_spcabbr3$aic
    #add the aic score to the empty list above : see : https://stackoverflow.com/questions/27153263/adding-elements-to-a-list-in-for-loop-in-r
    aic.4prdict[[nm]] <- aic.val03
    #round the AIC score -  to be presented later on in plot legend
    aic.val03r <- round(aic.val03,2)
    #get the maximum value of the predictor
    mp <- max(val.in.su2.nm)
    #round up this value
    mp<- ceiling(mp) 
    #make a series of numbers - see this website: https://www.theanalysisfactor.com/r-glm-plotting/
    xw <- seq(0, mp, 0.01)
    #predict a y value
    yw <- predict(M_spcabbr3, list(val.in.su2.nm = xw),type="response")
    #make a plot comparing only one predictor
    #plot.new()
    #dev.off()
    plot(val.in.su2.nm,val.in.su3.nm,
         bg=fillcol01,
         cex=1.6, pch = 21,
         xlab=in.su2.nm, # use the labels for the axis defined above
         ylab=in.su3.nm # use the labels for the axis defined above
    )
    #add a line from the model
    lines(xw, yw)
    #summary(M_spcabbr3)
    #plot confidence interval - see: https://rpubs.com/Bio-Geek/71339
    M_spcabbr3.1 <- lm(val.in.su3.nm ~ val.in.su2.nm, data=smpls02.15)
    conf_interval <- predict(M_spcabbr3.1, newdata=data.frame(val.in.su2.nm=xw), interval="confidence",
                             level = 0.95, scale =0.95, type="response")
    #conf_interval
    lines(xw, conf_interval[,2], col="black", lty=2)
    lines(xw, conf_interval[,3], col="black", lty=2)
    # Add a legend
    legend("topleft", legend=c("regression", "95% confidence interv"),
           col=c("black", "black"), lty=1:2, cex=0.8)
    legend("topright", legend=c(paste("AIC score: ",aic.val03r,sep="")),
           cex=0.8)
    # end pdf file
    # end file to save as file
    dev.off()
    
    #end loop
  }
  
  #see the aic list
  aic.2prdict
  #bind to a dataframe 
  aic.pred <- as.data.frame(cbind(lps.v1,aic.2prdict, aic.3prdict, aic.4prdict))
  aic.pred
  #############____ END ____ plot 3D ______________
  
  #############
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #  make plots of any eventual correlated variables  - end
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #  make MultiDimensional Scaling plots of log10 per L eDNA values   - start
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  #make a name for the files to be saved
  pdffn = paste(wd00,wd10,"/suppmatr_10.15g_App_K_eDNAlog10_per_L_MDS_01.pdf", sep="")
  pdf(pdffn 
      ,width=6, height=8
      #,width=(1.6*8.2677),height=(1.6*2.9232)
  )
  par(mfrow=c(2,1) # c(2,1) will give 2 rows and 1 column
      , mar=c(4,4,1,1)
      , oma=c(2,2,1,1)
  )
  #dev.off()
  #https://www.statmethods.net/advstats/mds.html
  #smpls02.15 #<- smpls02.14
  mds01_df<- t(smpls02.15)
  #head(smpls02.15)
  #nrow(smpls02.15)
  #ncol(smpls02.15)
  # Classical MDS
  # N rows (objects) x p columns (variables)
  # each row identified by a unique row name
  d <- dist(mds01_df) # euclidean distances between the rows
  fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
  #fit # view results
  # plot solution
  x <- fit$points[,1]
  y <- fit$points[,2]
  plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
       main="Metric MDS", #type="n")
       pch=21, col="black", bg="red")
  text(x, y, labels = row.names(mds01_df), cex=.7) 
  
  
  # Nonmetric MDS
  # N rows (objects) x p columns (variables)
  # each row identified by a unique row name
  
  library(MASS)
  d <- dist(mds01_df) # euclidean distances between the rows
  fit <- isoMDS(d, k=2) # k is the number of dim
  #fit # view results
  
  # plot solution
  x <- fit$points[,1]
  y <- fit$points[,2]
  plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
       main="Nonmetric MDS", #type="n")
       pch=21, col="black", bg="blue")
  text(x, y, labels = row.names(mds01_df), cex=.7) 
  
  
  # end pdf file
  # end file to save as file
  dev.off()
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #  make MultiDimensional Scaling plots of log10 per L eDNA values   - end
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  
  
  
  
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #  make MultiDimensional Scaling plots of harbours   tryout 01 - start
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  dev.off()
  #make a name for the files to be saved
  pdffn = paste(wd00,wd10,"/suppmatr_10.15g_App_K_eDNAlog10_per_L_MDS_02.pdf", sep="")
  pdf(pdffn 
      ,width=6, height=8
      #,width=(1.6*8.2677),height=(1.6*2.9232)
  )
  par(mfrow=c(2,1) # c(2,1) will give 2 rows and 1 column
      , mar=c(4,4,4,4) # order: bottom, left, top, and right.
      , oma=c(2,2,1,1)
      #https://stackoverflow.com/questions/3932038/plot-a-legend-outside-of-the-plotting-area-in-base-graphics
      , xpd=TRUE
  )
  #https://www.statmethods.net/advstats/mds.html
  mds01_df<- t(smpls02.11)
  mds01_df<- smpls02.11
  #use strpsplit to split by point
  dat1 <- data.frame(do.call(rbind, strsplit(as.vector(smpls02.11$Harbour.season.id), split = "[.]")))
  #use strsplit to split the column with names
  names(dat1) <- c(unlist(strsplit(colnames(smpls02.11[1]),"[.]")))
  #head(dat1)
  #head(smpls02.11)
  # Classical MDS
  # N rows (objects) x p columns (variables)
  # each row identified by a unique row name
  
  d <- dist(mds01_df) # euclidean distances between the rows
  fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
  #fit # view results
  #replace for harbours that will end up being equal to others
  dat1$Harbour[(dat1$Harbour=="KalundborgStatiolHavn")] <- "KAS"
  dat1$Harbour[(dat1$Harbour=="Aalborgportland")] <- "ALP"
  dat1$Harbour[(dat1$Harbour=="Fredericia")] <- "FRC"
  dat1$Harbour[(dat1$Harbour=="Frederikshavn")] <- "FRH"
  dat1$Harbour[(dat1$Harbour=="Kalundborg")] <- "KAB"
  dat1$Harbour[(dat1$Harbour=="Koebenhavn")] <- "KOB"
  dat1$Harbour[(dat1$Harbour=="Koege")] <- "KOG"
  dat1$Harbour[(dat1$Harbour=="AalborgHavn")] <- "ALH"
  #convert to uppercase
  dat1$Harbour <- toupper(substr(dat1$Harbour,1,3))
  # get number of colors for harbours
  nch <- length(sort(unique(dat1$Harbour)))
  colvc <- c("white","yellow","orange","green","steelblue2",
             "darkcyan","purple","black")
  colfhrb <- (grDevices::colorRampPalette(colvc))(nch)
  # length(colfhrb)
  df_cfh01 <- as.data.frame(cbind(unique(dat1$Harbour),colfhrb))
  colnames(df_cfh01) <- c("harb.abbr2","hxcol")
  dat1$hxcl2 <- df_cfh01$hxcol[match(dat1$Harbour,
                                     df_cfh01$harb.abbr2)]
  
  #get column as a vector
  va <- dat1$Harbour
  #replace every third element
  va[seq(1, length(va), 3)] <- ""
  va[seq(2, length(va), 3)] <- ""
  #replace column - this will make only one harbour name appear per
  # cluster of three points
  dat1$va <- va
  # plot solution
  x <- fit$points[,1]
  y <- fit$points[,2]
  plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
       main="Metric MDS", #type="n")
       #, pch=21, col="black", bg="red")
       pch=21, col="black", bg=dat1$hxcl2)
  #text(x, y, labels = row.names(mds01_df), cex=.7) 
  text(x, y, labels = dat1$va, cex=.7, pos=3) 
  
  legend("topright",
         title="harb", 
         legend=sort(unique(dat1$Harbour)),
         pch=21,
         inset=c(-0.1, 0),
         pt.bg=unique(dat1$hxcl2),
         cex=0.6,
         text.width = 0.6)
  
  # Nonmetric MDS
  # N rows (objects) x p columns (variables)
  # each row identified by a unique row name
  
  library(MASS)
  d <- dist(mds01_df) # euclidean distances between the rows
  fit <- isoMDS(d, k=2) # k is the number of dim
  #fit # view results
  
  # plot solution
  x <- fit$points[,1]
  y <- fit$points[,2]
  plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
       main="Nonmetric MDS", #type="n")
       #, pch=21, col="black", bg="blue")
       pch=21, col="black", bg=dat1$hxcl2)
  #text(x, y, labels = row.names(mds01_df), cex=.7) 
  text(x, y, labels = dat1$va, cex=.7, pos=3) 
  
  legend("topright",
         title="harb", 
         legend=sort(unique(dat1$Harbour)),
         pch=21,
         inset=c(-0.1, 0),
         pt.bg=unique(dat1$hxcl2),
         cex=0.6,
         text.width = 0.6)
  # end pdf file
  # end file to save as file
  dev.off()
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #  make MultiDimensional Scaling plots of harbours   tryout 01  - end
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  # length(unique(dat1$hxcl2))
  # length(sort(unique(dat1$Harbour)))
  
  
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #  make MultiDimensional Scaling plots of harbours   tryout 02 - start
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #make a name for the files to be saved
  pdffn = paste(wd00,wd10,"/suppmatr_10.15g_App_K_eDNAlog10_per_L_MDS_03.pdf", sep="")
  pdf(pdffn 
      ,width=6, height=8
      #,width=(1.6*8.2677),height=(1.6*2.9232)
  )
  par(mfrow=c(2,1) # c(2,1) will give 2 rows and 1 column
      , mar=c(4,4,2,2)
      , oma=c(2,2,2,2)
      #https://stackoverflow.com/questions/3932038/plot-a-legend-outside-of-the-plotting-area-in-base-graphics
      , xpd=TRUE
  )
  #https://www.statmethods.net/advstats/mds.html
  mds01_df<- t(smpls02.11)
  mds01_df<- smpls02.11
  #use strpsplit to split by point
  dat1 <- data.frame(do.call(rbind, strsplit(as.vector(smpls02.11$Harbour.season.id), split = "[.]")))
  #use strsplit to split the column with names
  names(dat1) <- c(unlist(strsplit(colnames(smpls02.11[1]),"[.]")))
  #head(dat1)
  #head(smpls02.11)
  #replace for harbours that will end up being equal to others
  dat1$Harbour[(dat1$Harbour=="KalundborgStatiolHavn")] <- "KAS"
  dat1$Harbour[(dat1$Harbour=="Aalborgportland")] <- "ALP"
  dat1$Harbour[(dat1$Harbour=="Fredericia")] <- "FRC"
  dat1$Harbour[(dat1$Harbour=="Frederikshavn")] <- "FRH"
  dat1$Harbour[(dat1$Harbour=="Kalundborg")] <- "KAB"
  dat1$Harbour[(dat1$Harbour=="Koebenhavn")] <- "KOB"
  dat1$Harbour[(dat1$Harbour=="Koege")] <- "KOG"
  dat1$Harbour[(dat1$Harbour=="AalborgHavn")] <- "ALH"
  #convert to uppercase
  dat1$Harbour <- toupper(substr(dat1$Harbour,1,3))
  
  # get number of colors for harbours
  nch <- length(unique(dat1$season))
  colvc <- c("lightgreen","tan3")
  colvc <- rev(colvc)
  colfhrb <- (grDevices::colorRampPalette(colvc))(nch)
  # length(colfhrb)
  df_cfh01 <- as.data.frame(cbind(unique(dat1$season),colfhrb))
  colnames(df_cfh01) <- c("harb.abbr2","hxcol")
  dat1$hxcl2 <- df_cfh01$hxcol[match(dat1$season,
                                     df_cfh01$harb.abbr2)]
  
  #get column as a vector
  va <- dat1$Harbour
  #replace Danish seasons to months
  dat1$season <- gsub("foraar","Jun_Jul",dat1$season)
  dat1$season <- gsub("efteraar","Sep_Nov",dat1$season)
  #replace every third element
  va[seq(1, length(va), 3)] <- ""
  va[seq(2, length(va), 3)] <- ""
  #replace column - this will make only one harbour name appear per
  # cluster of three points
  dat1$va <- va
  # Classical MDS
  # N rows (objects) x p columns (variables)
  # each row identified by a unique row name
  d <- dist(mds01_df) # euclidean distances between the rows
  fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
  # plot solution
  x <- fit$points[,1]
  y <- fit$points[,2]
  plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
       main="Metric MDS", #type="n")
       pch=21, col="black", bg=dat1$hxcl2)
  #text(x, y, labels = row.names(mds01_df), cex=.7) 
  text(x, y, labels = dat1$va, cex=.7, pos=3) 
  legend("topright", legend=unique(dat1$season),
         pch=21,
         inset=c(-0.1, 0),
         pt.bg=unique(dat1$hxcl2),
         cex=0.6,
         text.width = 0.6)
  # Nonmetric MDS
  # N rows (objects) x p columns (variables)
  # each row identified by a unique row name
  library(MASS)
  d <- dist(mds01_df) # euclidean distances between the rows
  fit <- isoMDS(d, k=2) # k is the number of dim
  # plot solution
  x <- fit$points[,1]
  y <- fit$points[,2]
  plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
       main="Nonmetric MDS", #type="n")
       pch=21, col="black", bg=dat1$hxcl2)
  text(x, y, labels = dat1$va, cex=.7, pos=3) 
  legend("topright", legend=unique(dat1$season),
         pch=21,
         inset=c(-0.1, 0),
         pt.bg=unique(dat1$hxcl2),
         cex=0.6,
         text.width = 0.6)
  # end pdf file
  # end file to save as file
  dev.off()
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #  make MultiDimensional Scaling plots of harbours   tryout 02  - end
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  
  
  
  
  
  
  
  
  #define input file with path
  inpfilw <- paste(wd00,wd09,"/suppmatr_09.20_inp_MONIS4_harbours03.csv",sep="")
  #read in csv file
  harb03 <- read.csv(inpfilw,sep=",")
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #  make MultiDimensional Scaling plots of harbours   tryout 04 - start
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #make a name for the files to be saved
  pdffn = paste(wd00,wd10,"/suppmatr_10.15g_App_K_eDNAlog10_per_L_MDS_04.pdf", sep="")
  pdf(pdffn 
      ,width=6, height=8
      #,width=(1.6*8.2677),height=(1.6*2.9232)
  )
  par(mfrow=c(2,1) # c(2,1) will give 2 rows and 1 column
      , mar=c(4,4,2,2)
      , oma=c(2,2,2,2)
      #https://stackoverflow.com/questions/3932038/plot-a-legend-outside-of-the-plotting-area-in-base-graphics
      , xpd=TRUE
  )
  #https://www.statmethods.net/advstats/mds.html
  mds01_df<- t(smpls02.11)
  mds01_df<- smpls02.11
  #use strpsplit to split by point
  dat1 <- data.frame(do.call(rbind, strsplit(as.vector(smpls02.11$Harbour.season.id), split = "[.]")))
  #use strsplit to split the column with names
  names(dat1) <- c(unlist(strsplit(colnames(smpls02.11[1]),"[.]")))
  #use the salinity from earlier
  #dat1$Harbour <- as.character(dat1$Harbour)
  salin.f.harbour <- harb03$salinity[match(dat1$Harbour,harb03$Harbour)]
  #match 
  dat1$salin <- harb03$salinity[match(dat1$Harbour,harb03$Harbour)]
  dat1$salin <- as.numeric(as.character(dat1$salin))
  # reorder the data frame
  dat1 <- dat1[order(dat1$salin),]
  #get color palette
  #https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
  if(!require(viridis)){
    install.packages("viridis")
  }
  library(viridis)
  #Get unique salinity values and assign colors to them 
  un.sal <- unique(salin.f.harbour)
  l.un.sal <- length(un.sal)
  vcol.sal <- viridis(l.un.sal, option = "D")
  
  vir.col_df <- as.data.frame(cbind(un.sal,vcol.sal))
  vir.col2 <- vir.col_df$vcol.sal[match(salin.f.harbour,vir.col_df$un.sal)]
  # Classical MDS
  # N rows (objects) x p columns (variables)
  # each row identified by a unique row name
  
  d <- dist(mds01_df) # euclidean distances between the rows
  fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
  #fit # view results
  #replace for harbours that will end up being equal to others
  dat1$Harbour[(dat1$Harbour=="KalundborgStatiolHavn")] <- "KAS"
  dat1$Harbour[(dat1$Harbour=="Aalborgportland")] <- "ALP"
  dat1$Harbour[(dat1$Harbour=="Fredericia")] <- "FRC"
  dat1$Harbour[(dat1$Harbour=="Frederikshavn")] <- "FRH"
  dat1$Harbour[(dat1$Harbour=="Kalundborg")] <- "KAB"
  dat1$Harbour[(dat1$Harbour=="Koebenhavn")] <- "KOB"
  dat1$Harbour[(dat1$Harbour=="Koege")] <- "KOG"
  dat1$Harbour[(dat1$Harbour=="AalborgHavn")] <- "ALH"
  #convert to uppercase
  dat1$Harbour <- toupper(substr(dat1$Harbour,1,3))
  # get number of colors for harbours
  nch <- length(unique(dat1$salin))
  colvc <- c("white","yellow",
             "darkcyan","purple")
  #reverse the order of the vector
  #colvc <-  rev(colvc)
  colfhrb <- (grDevices::colorRampPalette(colvc))(nch)
  df_cfh01 <- as.data.frame(cbind(unique(dat1$salin),colfhrb))
  colnames(df_cfh01) <- c("salin2","hxcol")
  dat1$hxcl2 <- df_cfh01$hxcol[match(dat1$salin,
                                     df_cfh01$salin2)]
  #https://stackoverflow.com/questions/28461326/convert-hex-color-code-to-color-name
  library(plotrix)
  #sapply(unique(dat1$hxcl2), color.id)
  
  #get column as a vector
  va <- dat1$Harbour
  #replace every third element
  va[seq(1, length(va), 3)] <- ""
  va[seq(2, length(va), 3)] <- ""
  #replace column - this will make only one harbour name appear per
  # cluster of three points
  dat1$va <- va
  # plot solution
  x <- fit$points[,1]
  y <- fit$points[,2]
  plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
       main="Metric MDS", #type="n")
       #, pch=21, col="black", bg="red")
       pch=21, col="black", bg=dat1$hxcl2)
  #text(x, y, labels = row.names(mds01_df), cex=.7) 
  text(x, y, labels = dat1$va, cex=.7, pos=3) 
  legend("topright", title="salinity ppt",
         legend=unique(dat1$salin),
         pch=21,
         inset=c(-0.1, 0),
         pt.bg=unique(dat1$hxcl2),
         cex=0.6,
         text.width = 0.6)
  
  # Nonmetric MDS
  # N rows (objects) x p columns (variables)
  # each row identified by a unique row name
  
  library(MASS)
  d <- dist(mds01_df) # euclidean distances between the rows
  fit <- isoMDS(d, k=2) # k is the number of dim
  #fit # view results
  
  # plot solution
  x <- fit$points[,1]
  y <- fit$points[,2]
  plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
       main="Nonmetric MDS", #type="n")
       #, pch=21, col="black", bg="blue")
       pch=21, col="black", bg=dat1$hxcl2)
  #text(x, y, labels = row.names(mds01_df), cex=.7) 
  text(x, y, labels = dat1$va, cex=.7, pos=3) 
  legend("topright", title="salinity ppt",
         legend=unique(dat1$salin),
         pch=21,
         inset=c(-0.1, 0),
         pt.bg=unique(dat1$hxcl2),
         cex=0.6,
         text.width = 0.6)
  # end pdf file
  # end pdf file
  # end file to save as file
  dev.off()
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #  make MultiDimensional Scaling plots of harbours   tryout 03  - end
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    
    
    
    #make a name for the files to be saved
    pdffn = paste(wd00,wd10,"/suppmatr_10.15h_App_K_eDNAlog10_per_L_MDS_03.pdf", sep="")
  pdf(pdffn 
      ,width=6, height=8
      #,width=(1.6*8.2677),height=(1.6*2.9232)
  )
  par(mfrow=c(2,1) # c(2,1) will give 2 rows and 1 column
      , mar=c(4,4,2,2)
      , oma=c(2,2,2,2)
      #https://stackoverflow.com/questions/3932038/plot-a-legend-outside-of-the-plotting-area-in-base-graphics
      , xpd=TRUE
      
  )
  #https://www.statmethods.net/advstats/mds.html
  mds01_df<- t(smpls02.11)
  mds01_df<- smpls02.11
  #use strpsplit to split by point
  dat1 <- data.frame(do.call(rbind, strsplit(as.vector(smpls02.11$Harbour.season.id), split = "[.]")))
  #use strsplit to split the column with names
  names(dat1) <- c(unlist(strsplit(colnames(smpls02.11[1]),"[.]")))
  #head(dat1)
  #Classical MDS
  # N rows (objects) x p columns (variables)
  # each row identified by a unique row name
  d <- dist(mds01_df) # euclidean distances between the rows
  fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
  #replace for harbours that will end up being equal to others
  dat1$Harbour[(dat1$Harbour=="KalundborgStatiolHavn")] <- "KAS"
  dat1$Harbour[(dat1$Harbour=="Aalborgportland")] <- "ALP"
  dat1$Harbour[(dat1$Harbour=="Fredericia")] <- "FRC"
  dat1$Harbour[(dat1$Harbour=="Frederikshavn")] <- "FRH"
  dat1$Harbour[(dat1$Harbour=="Kalundborg")] <- "KAB"
  dat1$Harbour[(dat1$Harbour=="Koebenhavn")] <- "KOB"
  dat1$Harbour[(dat1$Harbour=="Koege")] <- "KOG"
  dat1$Harbour[(dat1$Harbour=="AalborgHavn")] <- "ALH"
  #convert to uppercase
  dat1$Harbour <- toupper(substr(dat1$Harbour,1,3))
  # get number of colors for harbours
  nch <- length(unique(dat1$season))
  colvc <- c("lightgreen","tan3")
  colvc <- rev(colvc)
  colfhrb <- (grDevices::colorRampPalette(colvc))(nch)
  # length(colfhrb)
  df_cfh01 <- as.data.frame(cbind(unique(dat1$season),colfhrb))
  colnames(df_cfh01) <- c("harb.abbr2","hxcol")
  dat1$hxcl2 <- df_cfh01$hxcol[match(dat1$season,
                                     df_cfh01$harb.abbr2)]
  #replace Danish seasons to months
  dat1$season <- gsub("foraar","Jun_Jul",dat1$season)
  dat1$season <- gsub("efteraar","Sep_Nov",dat1$season)
  #get column as a vector
  va <- dat1$Harbour
  #replace every third element
  va[seq(1, length(va), 3)] <- ""
  va[seq(2, length(va), 3)] <- ""
  #replace column - this will make only one harbour name appear per
  # cluster of three points
  dat1$va <- va
  # plot solution
  x <- fit$points[,1]
  y <- fit$points[,2]
  plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
       main="Metric MDS", #type="n")
       #, pch=21, col="black", bg="red")
       pch=21, col="black", bg=c(dat1$season))
  #text(x, y, labels = row.names(mds01_df), cex=.7) 
  text(x, y, labels = dat1$va, cex=.7, pos=3) 
  legend("topright", legend=unique(dat1$season),
         pch=21,
         pt.bg=c(unique(dat1$hxcl2)),
         cex=0.8)
  # Nonmetric MDS
  # N rows (objects) x p columns (variables)
  # each row identified by a unique row name
  library(MASS)
  d <- dist(mds01_df) # euclidean distances between the rows
  fit <- isoMDS(d, k=2) # k is the number of dim
  # plot solution
  x <- fit$points[,1]
  y <- fit$points[,2]
  plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
       main="Nonmetric MDS", #type="n")
       #, pch=21, col="black", bg="blue")
       pch=21, col="black", bg=c(dat1$season))
  #text(x, y, labels = row.names(mds01_df), cex=.7) 
  text(x, y, labels = dat1$va, cex=0.7, pos=3) 
  legend("topright", legend=unique(dat1$season),
         pch=21,
         inset=c(-0.1, 0),
         pt.bg=unique(dat1$hxcl2),
         cex=0.6,
         text.width = 0.6)
  
  # end pdf file
  # end file to save as file
  dev.off()
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #  make MultiDimensional Scaling plots of harbours   tryout 02  - end
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #  make MultiDimensional Scaling plots of harbours   tryout 04 - start
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #make a name for the files to be saved
  pdffn = paste(wd00,wd10,"/suppmatr_10.15h_App_K_plot_eDNAlog10_per_L_MDS_04.pdf", sep="")
  pdf(pdffn 
      ,width=6, height=8
      #,width=(1.6*8.2677),height=(1.6*2.9232)
  )
  par(mfrow=c(2,1) # c(2,1) will give 2 rows and 1 column
      , mar=c(4,4,2,2)
      , oma=c(2,2,1,1)
      #https://stackoverflow.com/questions/3932038/plot-a-legend-outside-of-the-plotting-area-in-base-graphics
      , xpd=TRUE
  )
  #https://www.statmethods.net/advstats/mds.html
  mds01_df<- t(smpls02.11)
  mds01_df<- smpls02.11
  #use strpsplit to split by point
  dat1 <- data.frame(do.call(rbind, strsplit(as.vector(smpls02.11$Harbour.season.id), split = "[.]")))
  #use strsplit to split the column with names
  names(dat1) <- c(unlist(strsplit(colnames(smpls02.11[1]),"[.]")))
  #use the salinity from earlier
  #dat1$Harbour <- as.character(dat1$Harbour)
  #match 
  dat1$salin <- harb03$salinity[match(dat1$Harbour,harb03$Harbour)]
  dat1$salin <- as.numeric(as.character(dat1$salin))
  # reorder the data frame
  dat1 <- dat1[order(dat1$salin),]
  #head(dat1,3)
  #get color palette
  #https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
  if(!require(viridis)){
    install.packages("viridis")
  }
  library(viridis)
  #Get unique salinity values and assign colors to them 
  un.sal <- unique(salin.f.harbour)
  l.un.sal <- length(un.sal)
  vcol.sal <- viridis(l.un.sal, option = "D")
  vir.col_df <- as.data.frame(cbind(un.sal,vcol.sal))
  vir.col2 <- vir.col_df$vcol.sal[match(salin.f.harbour,vir.col_df$un.sal)]
  
  # Classical MDS
  # N rows (objects) x p columns (variables)
  # each row identified by a unique row name
  d <- dist(mds01_df) # euclidean distances between the rows
  fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
  #replace for harbours that will end up being equal to others
  dat1$Harbour[(dat1$Harbour=="KalundborgStatiolHavn")] <- "KAS"
  dat1$Harbour[(dat1$Harbour=="Aalborgportland")] <- "ALP"
  dat1$Harbour[(dat1$Harbour=="Fredericia")] <- "FRC"
  dat1$Harbour[(dat1$Harbour=="Frederikshavn")] <- "FRH"
  dat1$Harbour[(dat1$Harbour=="Kalundborg")] <- "KAB"
  dat1$Harbour[(dat1$Harbour=="Koebenhavn")] <- "KOB"
  dat1$Harbour[(dat1$Harbour=="Koege")] <- "KOG"
  dat1$Harbour[(dat1$Harbour=="AalborgHavn")] <- "ALH"
  #convert to uppercase
  dat1$Harbour <- toupper(substr(dat1$Harbour,1,3))
  # get number of colors for harbours
  nch <- length(unique(dat1$salin))
  colvc <- c("white","yellow",
             "darkcyan","purple")
  #reverse the order of the vector
  #colvc <-  rev(colvc)
  colfhrb <- (grDevices::colorRampPalette(colvc))(nch)
  df_cfh01 <- as.data.frame(cbind(unique(dat1$salin),colfhrb))
  colnames(df_cfh01) <- c("salin2","hxcol")
  dat1$hxcl2 <- df_cfh01$hxcol[match(dat1$salin,
                                     df_cfh01$salin2)]
  #https://stackoverflow.com/questions/28461326/convert-hex-color-code-to-color-name
  library(plotrix)
  sapply(unique(dat1$hxcl2), color.id)
  
  #get column as a vector
  va <- dat1$Harbour
  #replace every third element
  va[seq(1, length(va), 3)] <- ""
  va[seq(2, length(va), 3)] <- ""
  #replace column - this will make only one harbour name appear per
  # cluster of three points
  dat1$va <- va
  # plot solution
  x <- fit$points[,1]
  y <- fit$points[,2]
  plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
       main="Metric MDS", #type="n")
       #, pch=21, col="black", bg="red")
       pch=21, col="black", bg=dat1$hxcl2)
  #text(x, y, labels = row.names(mds01_df), cex=.7) 
  text(x, y, labels = dat1$va, cex=0.7, pos=3) 
  legend("topright", title="salinity ppt",
         legend=unique(dat1$salin),
         pch=21,
         inset=c(-0.1, 0),
         pt.bg=unique(dat1$hxcl2),
         cex=0.6,
         text.width = 0.6)
  # Nonmetric MDS
  # N rows (objects) x p columns (variables)
  # each row identified by a unique row name
  library(MASS)
  d <- dist(mds01_df) # euclidean distances between the rows
  fit <- isoMDS(d, k=2) # k is the number of dim
  # plot solution
  x <- fit$points[,1]
  y <- fit$points[,2]
  plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
       main="Nonmetric MDS", #type="n")
       #, pch=21, col="black", bg="blue")
       pch=21, col="black", bg=dat1$hxcl2)
  #text(x, y, labels = row.names(mds01_df), cex=.7) 
  text(x, y, labels = dat1$va, cex=0.7, pos=3) 
  legend("topright", title="salinity ppt", 
         legend=unique(dat1$salin),
         pch=21,
         inset=c(-0.1, 0),
         pt.bg=unique(dat1$hxcl2),
         cex=0.6,
         text.width = 0.6)
  # end pdf file
  # end file to save as file
  dev.off()
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #  make MultiDimensional Scaling plots of harbours   tryout 03  - end
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #
  
  
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #  make MultiDimensional Scaling plots of harbours   tryout 04 - start
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #make a name for the files to be saved
  pdffn = paste(wd00,wd10,"/suppmatr_10.15g_App_K_eDNAlog10_per_L_MDS_05.pdf", sep="")
  pdf(pdffn 
      ,width=6, height=8
      #,width=(1.6*8.2677),height=(1.6*2.9232)
  )
  par(mfrow=c(2,1) # c(2,1) will give 2 rows and 1 column
      , mar=c(4,4,2,2)
      , oma=c(2,2,2,2)
      #https://stackoverflow.com/questions/3932038/plot-a-legend-outside-of-the-plotting-area-in-base-graphics
      , xpd=TRUE
  )
  #https://www.statmethods.net/advstats/mds.html
  mds01_df<- t(smpls02.11)
  mds01_df<- smpls02.11
  #use strpsplit to split by point
  dat1 <- data.frame(do.call(rbind, strsplit(as.vector(smpls02.11$Harbour.season.id), split = "[.]")))
  #use strsplit to split the column with names
  names(dat1) <- c(unlist(strsplit(colnames(smpls02.11[1]),"[.]")))
  #use the salinity from earlier
  #dat1$Harbour <- as.character(dat1$Harbour)
  salin.f.harbour <- harb03$salinity[match(dat1$Harbour,harb03$Harbour)]
  #match 
  dat1$salin <- harb03$salinity[match(dat1$Harbour,harb03$Harbour)]
  dat1$salin <- as.numeric(as.character(dat1$salin))
  # reorder the data frame
  dat1 <- dat1[order(dat1$salin),]
  #get color palette
  #https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
  if(!require(viridis)){
    install.packages("viridis")
  }
  library(viridis)
  #Get unique salinity values and assign colors to them 
  un.sal <- unique(salin.f.harbour)
  l.un.sal <- length(un.sal)
  vcol.sal <- viridis(l.un.sal, option = "D")
  
  vir.col_df <- as.data.frame(cbind(un.sal,vcol.sal))
  vir.col2 <- vir.col_df$vcol.sal[match(salin.f.harbour,vir.col_df$un.sal)]
  # Classical MDS
  # N rows (objects) x p columns (variables)
  # each row identified by a unique row name
  
  d <- dist(mds01_df) # euclidean distances between the rows
  fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
  #fit # view results
  #replace for harbours that will end up being equal to others
  dat1$Harbour[(dat1$Harbour=="KalundborgStatiolHavn")] <- "KAS"
  dat1$Harbour[(dat1$Harbour=="Aalborgportland")] <- "ALP"
  dat1$Harbour[(dat1$Harbour=="Fredericia")] <- "FRC"
  dat1$Harbour[(dat1$Harbour=="Frederikshavn")] <- "FRH"
  dat1$Harbour[(dat1$Harbour=="Kalundborg")] <- "KAB"
  dat1$Harbour[(dat1$Harbour=="Koebenhavn")] <- "KOB"
  dat1$Harbour[(dat1$Harbour=="Koege")] <- "KOG"
  dat1$Harbour[(dat1$Harbour=="AalborgHavn")] <- "ALH"
  #convert to uppercase
  dat1$Harbour <- toupper(substr(dat1$Harbour,1,3))
  # get number of colors for harbours
  nch <- length(unique(dat1$salin))
  colvc <- c("white","yellow",
             "darkcyan","purple")
  #reverse the order of the vector
  #colvc <-  rev(colvc)
  colfhrb <- (grDevices::colorRampPalette(colvc))(nch)
  df_cfh01 <- as.data.frame(cbind(unique(dat1$salin),colfhrb))
  colnames(df_cfh01) <- c("salin2","hxcol")
  dat1$hxcl2 <- df_cfh01$hxcol[match(dat1$salin,
                                     df_cfh01$salin2)]
  #https://stackoverflow.com/questions/28461326/convert-hex-color-code-to-color-name
  library(plotrix)
  #sapply(unique(dat1$hxcl2), color.id)
  # get number of colors for harbours
  nch <- length(sort(unique(dat1$Harbour)))
  colvc <- c("white","yellow","orange","green","steelblue2",
             "darkcyan","purple","black")
  colfhrb <- (grDevices::colorRampPalette(colvc))(nch)
  # length(colfhrb)
  df_cfh02 <- as.data.frame(cbind(unique(dat1$Harbour),colfhrb))
  colnames(df_cfh02) <- c("harb.abbr3","hxcol")
  dat1$hxcl3 <- df_cfh02$hxcol[match(dat1$Harbour,
                                     df_cfh02$harb.abbr3)]
  
  #get column as a vector
  va <- dat1$Harbour
  #replace every third element
  va[seq(1, length(va), 3)] <- ""
  va[seq(2, length(va), 3)] <- ""
  #replace column - this will make only one harbour name appear per
  # cluster of three points
  dat1$va <- va
  # plot solution
  x <- fit$points[,1]
  y <- fit$points[,2]
  plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
       main="Metric MDS", #type="n")
       #, pch=21, col="black", bg="red")
       pch=21, col="black", bg=dat1$hxcl3, cex=2.2)
  points(x,y,
         pch=21, col="black",  bg=dat1$hxcl2, cex=1)
  #text(x, y, labels = row.names(mds01_df), cex=.7) 
  text(x, y, labels = dat1$va, cex=.7, pos=3) 
  legend("topright", title="salinity ppt",
         legend=unique(dat1$salin),
         pch=21,
         inset=c(-0.1, 0),
         pt.bg=unique(dat1$hxcl2),
         cex=0.6,
         text.width = 0.6)
  
  # Nonmetric MDS
  # N rows (objects) x p columns (variables)
  # each row identified by a unique row name
  
  library(MASS)
  d <- dist(mds01_df) # euclidean distances between the rows
  fit <- isoMDS(d, k=2) # k is the number of dim
  #fit # view results
  
  # plot solution
  x <- fit$points[,1]
  y <- fit$points[,2]
  plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
       main="Nonmetric MDS", #type="n")
       #, pch=21, col="black", bg="blue")
       pch=21, col="black", bg=dat1$hxcl3, cex=2.2)
  points(x,y,
         pch=21, col="black",  bg=dat1$hxcl2, cex=1)
  
  #text(x, y, labels = row.names(mds01_df), cex=.7) 
  text(x, y, labels = dat1$va, cex=.7, pos=3) 
  legend("topright", title="salinity ppt",
         legend=unique(dat1$salin),
         pch=21,
         inset=c(-0.1, 0),
         pt.bg=unique(dat1$hxcl2),
         cex=0.6,
         text.width = 0.6)
  # end pdf file
  # end pdf file
  # end file to save as file
  dev.off()
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #  make MultiDimensional Scaling plots of harbours   tryout 03  - end
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  
  
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #  make MultiDimensional Scaling plots of harbours   tryout 04 - start
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #make a name for the files to be saved
  pdffn = paste(wd00,wd10,"/suppmatr_10.15g_App_K_eDNAlog10_per_L_MDS_06.pdf", sep="")
  pdf(pdffn 
      ,width=6, height=8
      #,width=(1.6*8.2677),height=(1.6*2.9232)
  )
  par(mfrow=c(2,1) # c(2,1) will give 2 rows and 1 column
      , mar=c(4,4,2,2)
      , oma=c(2,2,2,2)
      #https://stackoverflow.com/questions/3932038/plot-a-legend-outside-of-the-plotting-area-in-base-graphics
      , xpd=TRUE
  )
  # Exclude species that are unaffected by salinity levels
  # Karenia_mikimotoi cannot transverse low salinities
  #https://www.jstage.jst.go.jp/article/pbr/9/1/9_42/_article
  
  df_g01<- smpls02.11[!grepl("Mnemi",colnames(smpls02.11)),]
  df_g01<- smpls02.11[!grepl("Mya",colnames(df_g01)),]
  df_g01<- smpls02.11[!grepl("Crasso",colnames(df_g01)),]
  df_g01<- smpls02.11[!grepl("Colp",colnames(df_g01)),]
  smpls02.12 <- df_g01
  #https://www.statmethods.net/advstats/mds.html
  #mds01_df<- t(smpls02.11)
  mds01_df<- smpls02.12
  #use strpsplit to split by point
  dat1 <- data.frame(do.call(rbind, strsplit(as.vector(smpls02.11$Harbour.season.id), split = "[.]")))
  #use strsplit to split the column with names
  names(dat1) <- c(unlist(strsplit(colnames(smpls02.11[1]),"[.]")))
  #use the salinity from earlier
  #dat1$Harbour <- as.character(dat1$Harbour)
  salin.f.harbour <- harb03$salinity[match(dat1$Harbour,harb03$Harbour)]
  #match 
  dat1$salin <- harb03$salinity[match(dat1$Harbour,harb03$Harbour)]
  dat1$salin <- as.numeric(as.character(dat1$salin))
  # reorder the data frame
  dat1 <- dat1[order(dat1$salin),]
  #get color palette
  #https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
  if(!require(viridis)){
    install.packages("viridis")
  }
  library(viridis)
  #Get unique salinity values and assign colors to them 
  un.sal <- unique(salin.f.harbour)
  l.un.sal <- length(un.sal)
  vcol.sal <- viridis(l.un.sal, option = "D")
  
  vir.col_df <- as.data.frame(cbind(un.sal,vcol.sal))
  vir.col2 <- vir.col_df$vcol.sal[match(salin.f.harbour,vir.col_df$un.sal)]
  # Classical MDS
  # N rows (objects) x p columns (variables)
  # each row identified by a unique row name
  
  d <- dist(mds01_df) # euclidean distances between the rows
  fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
  #fit # view results
  #replace for harbours that will end up being equal to others
  dat1$Harbour[(dat1$Harbour=="KalundborgStatiolHavn")] <- "KAS"
  dat1$Harbour[(dat1$Harbour=="Aalborgportland")] <- "ALP"
  dat1$Harbour[(dat1$Harbour=="Fredericia")] <- "FRC"
  dat1$Harbour[(dat1$Harbour=="Frederikshavn")] <- "FRH"
  dat1$Harbour[(dat1$Harbour=="Kalundborg")] <- "KAB"
  dat1$Harbour[(dat1$Harbour=="Koebenhavn")] <- "KOB"
  dat1$Harbour[(dat1$Harbour=="Koege")] <- "KOG"
  dat1$Harbour[(dat1$Harbour=="AalborgHavn")] <- "ALH"
  #convert to uppercase
  dat1$Harbour <- toupper(substr(dat1$Harbour,1,3))
  # get number of colors for harbours
  nch <- length(unique(dat1$salin))
  colvc <- c("white","yellow",
             "darkcyan","purple")
  #reverse the order of the vector
  #colvc <-  rev(colvc)
  colfhrb <- (grDevices::colorRampPalette(colvc))(nch)
  df_cfh01 <- as.data.frame(cbind(unique(dat1$salin),colfhrb))
  colnames(df_cfh01) <- c("salin2","hxcol")
  dat1$hxcl2 <- df_cfh01$hxcol[match(dat1$salin,
                                     df_cfh01$salin2)]
  #https://stackoverflow.com/questions/28461326/convert-hex-color-code-to-color-name
  library(plotrix)
  #sapply(unique(dat1$hxcl2), color.id)
  # get number of colors for harbours
  nch <- length(sort(unique(dat1$Harbour)))
  colvc <- c("white","yellow","orange","green","steelblue2",
             "darkcyan","purple","black")
  colfhrb <- (grDevices::colorRampPalette(colvc))(nch)
  # length(colfhrb)
  df_cfh02 <- as.data.frame(cbind(unique(dat1$Harbour),colfhrb))
  colnames(df_cfh02) <- c("harb.abbr3","hxcol")
  dat1$hxcl3 <- df_cfh02$hxcol[match(dat1$Harbour,
                                     df_cfh02$harb.abbr3)]
  
  #get column as a vector
  va <- dat1$Harbour
  #replace every third element
  va[seq(1, length(va), 3)] <- ""
  va[seq(2, length(va), 3)] <- ""
  #replace column - this will make only one harbour name appear per
  # cluster of three points
  dat1$va <- va
  # plot solution
  x <- fit$points[,1]
  y <- fit$points[,2]
  plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
       main="Metric MDS", #type="n")
       #, pch=21, col="black", bg="red")
       pch=21, col="black", bg=dat1$hxcl3, cex=2.2)
  points(x,y,
         pch=21, col="black",  bg=dat1$hxcl2, cex=1)
  #text(x, y, labels = row.names(mds01_df), cex=.7) 
  text(x, y, labels = dat1$va, cex=.7, pos=3) 
  legend("topright", title="salinity ppt",
         legend=unique(dat1$salin),
         pch=21,
         inset=c(-0.1, 0),
         pt.bg=unique(dat1$hxcl2),
         cex=0.6,
         text.width = 0.6)
  
  # Nonmetric MDS
  # N rows (objects) x p columns (variables)
  # each row identified by a unique row name
  
  library(MASS)
  d <- dist(mds01_df) # euclidean distances between the rows
  fit <- isoMDS(d, k=2) # k is the number of dim
  #fit # view results
  
  # plot solution
  x <- fit$points[,1]
  y <- fit$points[,2]
  plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
       main="Nonmetric MDS", #type="n")
       #, pch=21, col="black", bg="blue")
       pch=21, col="black", bg=dat1$hxcl3, cex=2.2)
  points(x,y,
         pch=21, col="black",  bg=dat1$hxcl2, cex=1)
  
  #text(x, y, labels = row.names(mds01_df), cex=.7) 
  text(x, y, labels = dat1$va, cex=.7, pos=3) 
  legend("topright", title="salinity ppt",
         legend=unique(dat1$salin),
         pch=21,
         inset=c(-0.1, 0),
         pt.bg=unique(dat1$hxcl2),
         cex=0.6,
         text.width = 0.6)
  # end pdf file
  # end pdf file
  # end file to save as file
  dev.off()
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #  make MultiDimensional Scaling plots of harbours   tryout 03  - end
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  
  
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #  make MultiDimensional Scaling plots of harbours   tryout 04 - start
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #make a name for the files to be saved
  pdffn = paste(wd00,wd10,"/suppmatr_10.15g_App_K_eDNAlog10_per_L_MDS_07.pdf", sep="")
  pdf(pdffn 
      ,width=6, height=8
      #,width=(1.6*8.2677),height=(1.6*2.9232)
  )
  par(mfrow=c(2,1) # c(2,1) will give 2 rows and 1 column
      , mar=c(4,4,2,2)
      , oma=c(2,2,2,2)
      #https://stackoverflow.com/questions/3932038/plot-a-legend-outside-of-the-plotting-area-in-base-graphics
      , xpd=TRUE
  )
  # Exclude species that are unaffected by salinity levels
  # Karenia_mikimotoi cannot transverse low salinities
  #https://www.jstage.jst.go.jp/article/pbr/9/1/9_42/_article
  
  df_g01<- smpls02.11[!grepl("Mnemi",colnames(smpls02.11)),]
  df_g01<- smpls02.11[!grepl("Mya",colnames(df_g01)),]
  df_g01<- smpls02.11[!grepl("Crasso",colnames(df_g01)),]
  df_g01<- smpls02.11[!grepl("Colp",colnames(df_g01)),]
  smpls02.12 <- df_g01
  #https://www.statmethods.net/advstats/mds.html
  #mds01_df<- t(smpls02.11)
  mds01_df<- smpls02.12
  #use strpsplit to split by point
  dat1 <- data.frame(do.call(rbind, strsplit(as.vector(smpls02.11$Harbour.season.id), split = "[.]")))
  #use strsplit to split the column with names
  names(dat1) <- c(unlist(strsplit(colnames(smpls02.11[1]),"[.]")))
  #use the salinity from earlier
  #dat1$Harbour <- as.character(dat1$Harbour)
  salin.f.harbour <- harb03$salinity[match(dat1$Harbour,harb03$Harbour)]
  #match 
  dat1$salin <- harb03$salinity[match(dat1$Harbour,harb03$Harbour)]
  dat1$salin <- as.numeric(as.character(dat1$salin))
  # reorder the data frame
  dat1 <- dat1[order(dat1$salin),]
  #get color palette
  #https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
  if(!require(viridis)){
    install.packages("viridis")
  }
  library(viridis)
  #Get unique salinity values and assign colors to them 
  un.sal <- unique(salin.f.harbour)
  l.un.sal <- length(un.sal)
  vcol.sal <- viridis(l.un.sal, option = "D")
  
  vir.col_df <- as.data.frame(cbind(un.sal,vcol.sal))
  vir.col2 <- vir.col_df$vcol.sal[match(salin.f.harbour,vir.col_df$un.sal)]
  # Classical MDS
  # N rows (objects) x p columns (variables)
  # each row identified by a unique row name
  
  d <- dist(mds01_df) # euclidean distances between the rows
  fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
  #fit # view results
  #replace for harbours that will end up being equal to others
  dat1$Harbour[(dat1$Harbour=="KalundborgStatiolHavn")] <- "KAS"
  dat1$Harbour[(dat1$Harbour=="Aalborgportland")] <- "ALP"
  dat1$Harbour[(dat1$Harbour=="Fredericia")] <- "FRC"
  dat1$Harbour[(dat1$Harbour=="Frederikshavn")] <- "FRH"
  dat1$Harbour[(dat1$Harbour=="Kalundborg")] <- "KAB"
  dat1$Harbour[(dat1$Harbour=="Koebenhavn")] <- "KOB"
  dat1$Harbour[(dat1$Harbour=="Koege")] <- "KOG"
  dat1$Harbour[(dat1$Harbour=="AalborgHavn")] <- "ALH"
  #convert to uppercase
  dat1$Harbour <- toupper(substr(dat1$Harbour,1,3))
  # get number of colors for harbours
  nch <- length(unique(dat1$salin))
  colvc <- c("white","yellow",
             "darkcyan","purple")
  #reverse the order of the vector
  #colvc <-  rev(colvc)
  colfhrb <- (grDevices::colorRampPalette(colvc))(nch)
  df_cfh01 <- as.data.frame(cbind(unique(dat1$salin),colfhrb))
  colnames(df_cfh01) <- c("salin2","hxcol")
  dat1$hxcl2 <- df_cfh01$hxcol[match(dat1$salin,
                                     df_cfh01$salin2)]
  #https://stackoverflow.com/questions/28461326/convert-hex-color-code-to-color-name
  library(plotrix)
  #sapply(unique(dat1$hxcl2), color.id)
  # get number of colors for harbours
  nch <- length(sort(unique(dat1$Harbour)))
  colvc <- c("white","yellow","orange","green","steelblue2",
             "darkcyan","purple","black")
  colfhrb <- (grDevices::colorRampPalette(colvc))(nch)
  df_cfh02 <- as.data.frame(cbind(unique(dat1$Harbour),colfhrb))
  colnames(df_cfh02) <- c("harb.abbr3","hxcol")
  dat1$hxcl3 <- df_cfh02$hxcol[match(dat1$Harbour,
                                     df_cfh02$harb.abbr3)]
  #count unique elements in column and use for making pch symbols to
  # macth back to data frame
  nchs <- length(unique(dat1$salin))
  pchv <- seq(21,20+nchs,by=1)
  df_cfh03 <- as.data.frame(cbind(unique(dat1$salin),pchv))
  colnames(df_cfh03) <- c("salin3","pchsymb")
  dat1$pchsymb <- df_cfh03$pchsymb[match(dat1$salin,
                                         df_cfh03$salin3)]
  
  #count unique elements in column and use for making pch symbols to
  # macth back to data frame
  nchs <- length(unique(dat1$salin))
  pchv <- seq(1,nchs,by=1)
  df_cfh03 <- as.data.frame(cbind(unique(dat1$salin),pchv))
  colnames(df_cfh03) <- c("salin3","pchsymb")
  dat1$pchsymb <- df_cfh03$pchsymb[match(dat1$salin,
                                         df_cfh03$salin3)]
  #count unique elements in column and use for making pch symbols to
  # macth back to data frame
  nchs <- length(unique(dat1$season))
  pchv <- seq(1,nchs,by=1)
  pchv <- c("lightgreen","orange")
  df_cfh03 <- as.data.frame(cbind(unique(dat1$season),pchv))
  colnames(df_cfh03) <- c("season3","colfs")
  dat1$colfs <- df_cfh03$colfs[match(dat1$season,
                                     df_cfh03$season3)]
  #substitute in season names
  dat1$season <- gsub("foraar","Jun-Jul",dat1$season) 
  dat1$season <- gsub("efteraar","Sep-Nov",dat1$season) 
  #get column as a vector
  va <- dat1$Harbour
  #replace every third element
  va[seq(1, length(va), 3)] <- ""
  va[seq(2, length(va), 3)] <- ""
  #replace column - this will make only one harbour name appear per
  # cluster of three points
  dat1$va <- va
  # plot solution
  x <- fit$points[,1]
  y <- fit$points[,2]
  plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
       main="Metric MDS", #type="n")
       #, pch=21, col="black", bg="red")
       #pch=21, col="black", bg=dat1$hxcl3, cex=2.2)
       pch=21, col="black", bg=dat1$colfs, cex=2.4)
  # points(x,y,
  #        pch=21, col="black",  bg=dat1$hxcl2, cex=1)
  #text(x, y, labels = row.names(mds01_df), cex=.7) 
  text(x, y, labels = dat1$Harbour, cex=.4)#, pos=3) 
  legend("topright", title="harbour",
         legend=sort(unique(dat1$Harbour)),
         pch=21,
         inset=c(-0.1, 0),
         #pt.bg=unique(dat1$hxcl3),
         cex=0.6,
         text.width = 0.6)
  legend("topleft", title="season",
         legend=unique(dat1$season),
         #pch=unique(dat1$pchsymb),
         pch=21,
         inset=c(-0.1, 0),
         #pt.bg="black",
         pt.bg=unique(dat1$colfs),
         cex=0.6,
         text.width = 0.6)
  # Nonmetric MDS
  # N rows (objects) x p columns (variables)
  # each row identified by a unique row name
  
  library(MASS)
  d <- dist(mds01_df) # euclidean distances between the rows
  fit <- isoMDS(d, k=2) # k is the number of dim
  #fit # view results
  
  # plot solution
  x <- fit$points[,1]
  y <- fit$points[,2]
  #dev.off()
  plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
       main="Nonmetric MDS", #type="n")
       pch=21, col="black", bg=dat1$colfs, cex=2.4)
  #pch=dat1$pchsymb, col="black", bg=dat1$hxcl3, cex=1.8)
  # points(x,y,
  #        pch=21, col="black",  bg=dat1$hxcl2, cex=1)
  #text(x, y, labels = row.names(mds01_df), cex=.7) 
  text(x, y, labels = dat1$Harbour, cex=.4)#, pos=3) 
  legend("topright", title="harbour",
         legend=sort(unique(dat1$Harbour)),
         pch=21,
         inset=c(-0.1, 0),
         #pt.bg=unique(dat1$hxcl2),
         cex=0.6,
         text.width = 0.6)
  legend("topleft", title="season",
         legend=unique(dat1$season),
         #pch=unique(dat1$pchsymb),
         pch=21,
         inset=c(-0.1, 0),
         #pt.bg="black",
         pt.bg=unique(dat1$colfs),
         cex=0.6,
         text.width = 0.6)
  # end pdf file
  # end pdf file
  # end file to save as file
  dev.off()
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #  make MultiDimensional Scaling plots of harbours   tryout 03  - end
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #  make MultiDimensional Scaling plots of harbours   tryout 04 - start
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #make a name for the files to be saved
  pdffn = paste(wd00,wd10,"/suppmatr_10.15g_App_K_eDNAlog10_per_L_MDS_08.pdf", sep="")
  pdf(pdffn 
      ,width=6, height=8
      #,width=(1.6*8.2677),height=(1.6*2.9232)
  )
  par(mfrow=c(2,1) # c(2,1) will give 2 rows and 1 column
      , mar=c(4,4,2,2)
      , oma=c(2,2,2,2)
      #https://stackoverflow.com/questions/3932038/plot-a-legend-outside-of-the-plotting-area-in-base-graphics
      , xpd=TRUE
  )
  # Exclude species that are unaffected by salinity levels
  # Karenia_mikimotoi cannot transverse low salinities
  #https://www.jstage.jst.go.jp/article/pbr/9/1/9_42/_article
  
  df_g01<- smpls02.11[!grepl("Mnemi",colnames(smpls02.11)),]
  df_g01<- smpls02.11[!grepl("Mya",colnames(df_g01)),]
  df_g01<- smpls02.11[!grepl("Crasso",colnames(df_g01)),]
  df_g01<- smpls02.11[!grepl("Colp",colnames(df_g01)),]
  smpls02.12 <- df_g01
  #https://www.statmethods.net/advstats/mds.html
  #mds01_df<- t(smpls02.11)
  mds01_df<- smpls02.12
  #use strpsplit to split by point
  dat1 <- data.frame(do.call(rbind, strsplit(as.vector(smpls02.11$Harbour.season.id), split = "[.]")))
  #use strsplit to split the column with names
  names(dat1) <- c(unlist(strsplit(colnames(smpls02.11[1]),"[.]")))
  #use the salinity from earlier
  #dat1$Harbour <- as.character(dat1$Harbour)
  salin.f.harbour <- harb03$salinity[match(dat1$Harbour,harb03$Harbour)]
  #match 
  dat1$salin <- harb03$salinity[match(dat1$Harbour,harb03$Harbour)]
  dat1$salin <- as.numeric(as.character(dat1$salin))
  # reorder the data frame
  dat1 <- dat1[order(dat1$salin),]
  #get color palette
  #https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
  if(!require(viridis)){
    install.packages("viridis")
  }
  library(viridis)
  #Get unique salinity values and assign colors to them 
  un.sal <- unique(salin.f.harbour)
  l.un.sal <- length(un.sal)
  vcol.sal <- viridis(l.un.sal, option = "D")
  
  vir.col_df <- as.data.frame(cbind(un.sal,vcol.sal))
  vir.col2 <- vir.col_df$vcol.sal[match(salin.f.harbour,vir.col_df$un.sal)]
  # Classical MDS
  # N rows (objects) x p columns (variables)
  # each row identified by a unique row name
  
  d <- dist(mds01_df) # euclidean distances between the rows
  fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
  #fit # view results
  #replace for harbours that will end up being equal to others
  dat1$Harbour[(dat1$Harbour=="KalundborgStatiolHavn")] <- "KAS"
  dat1$Harbour[(dat1$Harbour=="Aalborgportland")] <- "ALP"
  dat1$Harbour[(dat1$Harbour=="Fredericia")] <- "FRC"
  dat1$Harbour[(dat1$Harbour=="Frederikshavn")] <- "FRH"
  dat1$Harbour[(dat1$Harbour=="Kalundborg")] <- "KAB"
  dat1$Harbour[(dat1$Harbour=="Koebenhavn")] <- "KOB"
  dat1$Harbour[(dat1$Harbour=="Koege")] <- "KOG"
  dat1$Harbour[(dat1$Harbour=="AalborgHavn")] <- "ALH"
  #convert to uppercase
  dat1$Harbour <- toupper(substr(dat1$Harbour,1,3))
  # get number of colors for harbours
  nch <- length(unique(dat1$salin))
  colvc <- c("white","yellow",
             "darkcyan","purple")
  #reverse the order of the vector
  #colvc <-  rev(colvc)
  colfhrb <- (grDevices::colorRampPalette(colvc))(nch)
  df_cfh01 <- as.data.frame(cbind(unique(dat1$salin),colfhrb))
  colnames(df_cfh01) <- c("salin2","hxcol")
  dat1$hxcl2 <- df_cfh01$hxcol[match(dat1$salin,
                                     df_cfh01$salin2)]
  #https://stackoverflow.com/questions/28461326/convert-hex-color-code-to-color-name
  library(plotrix)
  #sapply(unique(dat1$hxcl2), color.id)
  # get number of colors for harbours
  nch <- length(sort(unique(dat1$Harbour)))
  colvc <- c("white","yellow","orange","green","steelblue2",
             "darkcyan","purple","black")
  colfhrb <- (grDevices::colorRampPalette(colvc))(nch)
  df_cfh02 <- as.data.frame(cbind(unique(dat1$Harbour),colfhrb))
  colnames(df_cfh02) <- c("harb.abbr3","hxcol")
  dat1$hxcl3 <- df_cfh02$hxcol[match(dat1$Harbour,
                                     df_cfh02$harb.abbr3)]
  #count unique elements in column and use for making pch symbols to
  # macth back to data frame
  nchs <- length(unique(dat1$salin))
  pchv <- seq(21,20+nchs,by=1)
  df_cfh03 <- as.data.frame(cbind(unique(dat1$salin),pchv))
  colnames(df_cfh03) <- c("salin3","pchsymb")
  dat1$pchsymb <- df_cfh03$pchsymb[match(dat1$salin,
                                         df_cfh03$salin3)]
  
  #count unique elements in column and use for making pch symbols to
  # macth back to data frame
  nchs <- length(unique(dat1$salin))
  pchv <- seq(1,nchs,by=1)
  df_cfh03 <- as.data.frame(cbind(unique(dat1$salin),pchv))
  colnames(df_cfh03) <- c("salin3","pchsymb")
  dat1$pchsymb <- df_cfh03$pchsymb[match(dat1$salin,
                                         df_cfh03$salin3)]
  #count unique elements in column and use for making pch symbols to
  # macth back to data frame
  nchs <- length(unique(dat1$season))
  pchv <- seq(1,nchs,by=1)
  pchv <- c("lightgreen","orange")
  df_cfh03 <- as.data.frame(cbind(unique(dat1$season),pchv))
  colnames(df_cfh03) <- c("season3","colfs")
  dat1$colfs <- df_cfh03$colfs[match(dat1$season,
                                     df_cfh03$season3)]
  #substitute in season names
  dat1$season <- gsub("foraar","Jun-Jul",dat1$season) 
  dat1$season <- gsub("efteraar","Sep-Nov",dat1$season) 
  #get column as a vector
  va <- dat1$Harbour
  #replace every third element
  va[seq(1, length(va), 3)] <- ""
  va[seq(2, length(va), 3)] <- ""
  #replace column - this will make only one harbour name appear per
  # cluster of three points
  dat1$va <- va
  # plot solution
  x <- fit$points[,1]
  y <- fit$points[,2]
  plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
       main="Metric MDS", #type="n")
       #, pch=21, col="black", bg="red")
       #pch=21, col="black", bg=dat1$hxcl3, cex=2.2)
       pch=21, col="black", bg=dat1$hxcl2, cex=2.4)
  # points(x,y,
  #        pch=21, col="black",  bg=dat1$hxcl2, cex=1)
  #text(x, y, labels = row.names(mds01_df), cex=.7) 
  text(x, y, labels = dat1$Harbour, cex=.4)#, pos=3) 
  legend("topright", title="harbour",
         legend=sort(unique(dat1$Harbour)),
         pch=21,
         inset=c(-0.1, 0),
         #pt.bg=unique(dat1$hxcl3),
         cex=0.6,
         text.width = 0.6)
  legend("topleft", title="salinity",
         legend=unique(dat1$salin),
         #pch=unique(dat1$pchsymb),
         pch=21,
         inset=c(-0.1, 0),
         #pt.bg="black",
         pt.bg=unique(dat1$hxcl2),
         cex=0.6,
         text.width = 0.6)
  # Nonmetric MDS
  # N rows (objects) x p columns (variables)
  # each row identified by a unique row name
  
  library(MASS)
  d <- dist(mds01_df) # euclidean distances between the rows
  fit <- isoMDS(d, k=2) # k is the number of dim
  #fit # view results
  
  # plot solution
  x <- fit$points[,1]
  y <- fit$points[,2]
  #dev.off()
  plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
       main="Nonmetric MDS", #type="n")
       pch=21, col="black", bg=dat1$hxcl2, cex=2.4)
  #pch=dat1$pchsymb, col="black", bg=dat1$hxcl3, cex=1.8)
  # points(x,y,
  #        pch=21, col="black",  bg=dat1$hxcl2, cex=1)
  #text(x, y, labels = row.names(mds01_df), cex=.7) 
  text(x, y, labels = dat1$Harbour, cex=.4)#, pos=3) 
  legend("topright", title="harbour",
         legend=sort(unique(dat1$Harbour)),
         pch=21,
         inset=c(-0.1, 0),
         #pt.bg=unique(dat1$hxcl2),
         cex=0.6,
         text.width = 0.6)
  legend("topleft", title="salinity",
         legend=unique(dat1$salin),
         #pch=unique(dat1$pchsymb),
         pch=21,
         inset=c(-0.1, 0),
         #pt.bg="black",
         pt.bg=unique(dat1$hxcl2),
         cex=0.6,
         text.width = 0.6)
  # end pdf file
  # end pdf file
  # end file to save as file
  dev.off()
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #  make MultiDimensional Scaling plots of harbours   tryout 03  - end
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #  make MultiDimensional Scaling plots of harbours   tryout 04 - start
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #make a name for the files to be saved
  pdffn = paste(wd00,wd10,"/suppmatr_10.15g_App_K_eDNAlog10_per_L_MDS_12.pdf", sep="")
  pdf(pdffn 
      ,width=6, height=8
      #,width=(1.6*8.2677),height=(1.6*2.9232)
  )
  par(mfrow=c(2,1) # c(2,1) will give 2 rows and 1 column
      , mar=c(4,4,2,2)
      , oma=c(2,2,2,2)
      #https://stackoverflow.com/questions/3932038/plot-a-legend-outside-of-the-plotting-area-in-base-graphics
      , xpd=TRUE
  )
  # Exclude species that are unaffected by salinity levels
  # Karenia_mikimotoi cannot transverse low salinities
  #https://www.jstage.jst.go.jp/article/pbr/9/1/9_42/_article
  
  df_g01<- smpls02.11[!grepl("Mnemi",colnames(smpls02.11)),]
  df_g01<- smpls02.11[!grepl("Mya",colnames(df_g01)),]
  df_g01<- smpls02.11[!grepl("Crasso",colnames(df_g01)),]
  df_g01<- smpls02.11[!grepl("Colp",colnames(df_g01)),]
  smpls02.12 <- df_g01
  #https://www.statmethods.net/advstats/mds.html
  #mds01_df<- t(smpls02.11)
  mds01_df<- smpls02.12
  #use strpsplit to split by point
  dat1 <- data.frame(do.call(rbind, strsplit(as.vector(smpls02.11$Harbour.season.id), split = "[.]")))
  #use strsplit to split the column with names
  names(dat1) <- c(unlist(strsplit(colnames(smpls02.11[1]),"[.]")))
  #use the salinity from earlier
  #dat1$Harbour <- as.character(dat1$Harbour)
  salin.f.harbour <- harb03$salinity[match(dat1$Harbour,harb03$Harbour)]
  #match 
  dat1$salin <- harb03$salinity[match(dat1$Harbour,harb03$Harbour)]
  dat1$salin <- as.numeric(as.character(dat1$salin))
  # reorder the data frame
  dat1 <- dat1[order(dat1$salin),]
  #get color palette
  #https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
  if(!require(viridis)){
    install.packages("viridis")
  }
  library(viridis)
  #Get unique salinity values and assign colors to them 
  un.sal <- unique(salin.f.harbour)
  l.un.sal <- length(un.sal)
  vcol.sal <- viridis(l.un.sal, option = "D")
  
  vir.col_df <- as.data.frame(cbind(un.sal,vcol.sal))
  vir.col2 <- vir.col_df$vcol.sal[match(salin.f.harbour,vir.col_df$un.sal)]
  # Classical MDS
  # N rows (objects) x p columns (variables)
  # each row identified by a unique row name
  
  d <- dist(mds01_df) # euclidean distances between the rows
  fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
  #fit # view results
  #replace for harbours that will end up being equal to others
  dat1$Harbour[(dat1$Harbour=="KalundborgStatiolHavn")] <- "KAS"
  dat1$Harbour[(dat1$Harbour=="Aalborgportland")] <- "ALP"
  dat1$Harbour[(dat1$Harbour=="Fredericia")] <- "FRC"
  dat1$Harbour[(dat1$Harbour=="Frederikshavn")] <- "FRH"
  dat1$Harbour[(dat1$Harbour=="Kalundborg")] <- "KAB"
  dat1$Harbour[(dat1$Harbour=="Koebenhavn")] <- "KOB"
  dat1$Harbour[(dat1$Harbour=="Koege")] <- "KOG"
  dat1$Harbour[(dat1$Harbour=="AalborgHavn")] <- "ALH"
  #convert to uppercase
  dat1$Harbour <- toupper(substr(dat1$Harbour,1,3))
  # get number of colors for harbours
  nch <- length(unique(dat1$salin))
  colvc <- c("white","yellow",
             "darkcyan","purple")
  #reverse the order of the vector
  #colvc <-  rev(colvc)
  colfhrb <- (grDevices::colorRampPalette(colvc))(nch)
  df_cfh01 <- as.data.frame(cbind(unique(dat1$salin),colfhrb))
  colnames(df_cfh01) <- c("salin2","hxcol")
  dat1$hxcl2 <- df_cfh01$hxcol[match(dat1$salin,
                                     df_cfh01$salin2)]
  #https://stackoverflow.com/questions/28461326/convert-hex-color-code-to-color-name
  library(plotrix)
  #sapply(unique(dat1$hxcl2), color.id)
  # get number of colors for harbours
  nch <- length(sort(unique(dat1$Harbour)))
  colvc <- c("white","red","pink","orange","yellow",
             "green","cyan",
             "steelblue2",
             "darkcyan","blue","plum3","purple","black")
  colfhrb <- (grDevices::colorRampPalette(colvc))(nch)
  df_cfh02 <- as.data.frame(cbind(unique(dat1$Harbour),colfhrb))
  colnames(df_cfh02) <- c("harb.abbr3","hxcol")
  dat1$hxcl3 <- df_cfh02$hxcol[match(dat1$Harbour,
                                     df_cfh02$harb.abbr3)]
  #count unique elements in column and use for making pch symbols to
  # macth back to data frame
  nchs <- length(unique(dat1$salin))
  pchv <- seq(21,20+nchs,by=1)
  df_cfh03 <- as.data.frame(cbind(unique(dat1$salin),pchv))
  colnames(df_cfh03) <- c("salin3","pchsymb")
  dat1$pchsymb <- df_cfh03$pchsymb[match(dat1$salin,
                                         df_cfh03$salin3)]
  #replace Danish seasons to months
  dat1$season <- gsub("foraar","Jun_Jul",dat1$season)
  dat1$season <- gsub("efteraar","Sep_Nov",dat1$season)
  #count unique elements in column and use for making pch symbols to
  # macth back to data frame
  nchs <- length(unique(dat1$season))
  pchv <- seq(21,20+nchs,by=1)
  df_cfh03 <- as.data.frame(cbind(unique(dat1$season),pchv))
  colnames(df_cfh03) <- c("season3","pchsymb2")
  dat1$pchsymb2 <- df_cfh03$pchsymb2[match(dat1$season,
                                           df_cfh03$season3)]
  dat1$pchsymb2 <- as.numeric(dat1$pchsymb2)
  #count unique elements in column and use for making pch symbols to
  # macth back to data frame
  nchs <- length(unique(dat1$salin))
  pchv <- seq(21,20+nchs,by=1)
  df_cfh03 <- as.data.frame(cbind(unique(dat1$salin),pchv))
  colnames(df_cfh03) <- c("salin3","pchsymb")
  dat1$pchsymb <- df_cfh03$pchsymb[match(dat1$salin,
                                         df_cfh03$salin3)]
  dat1$pchsymb <- as.numeric(dat1$pchsymb)
  #count unique elements in column and use for making pch symbols to
  # macth back to data frame
  nchs <- length(unique(dat1$season))
  pchv <- seq(1,nchs,by=1)
  pchv <- c("lightgreen","orange")
  df_cfh03 <- as.data.frame(cbind(unique(dat1$season),pchv))
  colnames(df_cfh03) <- c("season3","colfs")
  dat1$colfs <- df_cfh03$colfs[match(dat1$season,
                                     df_cfh03$season3)]
  #substitute in season names
  dat1$season <- gsub("foraar","Jun-Jul",dat1$season) 
  dat1$season <- gsub("efteraar","Sep-Nov",dat1$season) 
  #get column as a vector
  va <- dat1$Harbour
  #replace every third element
  va[seq(1, length(va), 3)] <- ""
  va[seq(2, length(va), 3)] <- ""
  #replace column - this will make only one harbour name appear per
  # cluster of three points
  dat1$va <- va
  # plot solution
  x <- fit$points[,1]
  y <- fit$points[,2]
  plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
       main="Metric MDS", #type="n")
       #, pch=21, col="black", bg="red")
       pch=dat1$pchsymb2, col="black", bg=dat1$hxcl3, cex=1.2)
  #pch=21, col="black", bg=dat1$hxcl2, cex=2.4)
  # points(x,y,
  #        pch=21, col="black",  bg=dat1$hxcl2, cex=1)
  #text(x, y, labels = row.names(mds01_df), cex=.7) 
  #text(x, y, labels = dat1$va, cex=.4, pos=3) 
  legend("topright", title="harbour",
         legend=sort(unique(dat1$Harbour)),
         pch=21,
         inset=c(-0.1, 0),
         pt.bg=unique(dat1$hxcl3),
         cex=0.6,
         text.width = 0.6)
  legend("topleft", title="season",
         legend=unique(dat1$season),
         pch=unique(dat1$pchsymb2),
         #pch=21,
         inset=c(-0.1, 0),
         pt.bg="black",
         #pt.bg=unique(dat1$hxcl2),
         cex=0.6,
         text.width = 0.6)
  # Nonmetric MDS
  # N rows (objects) x p columns (variables)
  # each row identified by a unique row name
  
  library(MASS)
  d <- dist(mds01_df) # euclidean distances between the rows
  fit <- isoMDS(d, k=2) # k is the number of dim
  #fit # view results
  
  # plot solution
  x <- fit$points[,1]
  y <- fit$points[,2]
  #dev.off()
  plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
       main="Nonmetric MDS", #type="n")
       #pch=21, col="black", bg=dat1$hxcl2, cex=2.4)
       pch=dat1$pchsymb2, col="black", bg=dat1$hxcl3, cex=1.2)
  # points(x,y,
  #        pch=21, col="black",  bg=dat1$hxcl2, cex=1)
  #text(x, y, labels = row.names(mds01_df), cex=.7) 
  #text(x, y, labels = dat1$va, cex=.4, pos=3) 
  legend("topright", title="harbour",
         legend=sort(unique(dat1$Harbour)),
         pch=21,
         inset=c(-0.1, 0),
         pt.bg=unique(dat1$hxcl3),
         cex=0.6,
         text.width = 0.6)
  legend("topleft", title="season",
         legend=unique(dat1$season),
         pch=unique(dat1$pchsymb2),
         #pch=21,
         inset=c(-0.1, 0),
         pt.bg="black",
         #pt.bg=unique(dat1$hxcl2),
         cex=0.6,
         text.width = 0.6)
  # end pdf file
  # end pdf file
  # end file to save as file
  dev.off()
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #  make MultiDimensional Scaling plots of harbours   tryout 03  - end
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #  make MultiDimensional Scaling plots of harbours   tryout 04 - start
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #make a name for the files to be saved
  pdffn = paste(wd00,wd10,"/suppmatr_10.15g_App_K_eDNAlog10_per_L_MDS_09.pdf", sep="")
  pdf(pdffn 
      ,width=6, height=8
      #,width=(1.6*8.2677),height=(1.6*2.9232)
  )
  par(mfrow=c(2,1) # c(2,1) will give 2 rows and 1 column
      , mar=c(4,4,4,4)
      , oma=c(1,1,1,1)
      #https://stackoverflow.com/questions/3932038/plot-a-legend-outside-of-the-plotting-area-in-base-graphics
      , xpd=TRUE
  )
  # Exclude species that are unaffected by salinity levels
  # Karenia_mikimotoi cannot transverse low salinities
  #https://www.jstage.jst.go.jp/article/pbr/9/1/9_42/_article
  
  df_g01<- smpls02.11[!grepl("Mnemi",colnames(smpls02.11)),]
  df_g01<- smpls02.11[!grepl("Mya",colnames(df_g01)),]
  df_g01<- smpls02.11[!grepl("Crasso",colnames(df_g01)),]
  df_g01<- smpls02.11[!grepl("Colp",colnames(df_g01)),]
  smpls02.12 <- df_g01
  #https://www.statmethods.net/advstats/mds.html
  #mds01_df<- t(smpls02.11)
  mds01_df<- smpls02.12
  #use strpsplit to split by point
  dat1 <- data.frame(do.call(rbind, strsplit(as.vector(smpls02.11$Harbour.season.id), split = "[.]")))
  #use strsplit to split the column with names
  names(dat1) <- c(unlist(strsplit(colnames(smpls02.11[1]),"[.]")))
  #use the salinity from earlier
  #dat1$Harbour <- as.character(dat1$Harbour)
  salin.f.harbour <- harb03$salinity[match(dat1$Harbour,harb03$Harbour)]
  #match 
  dat1$salin <- harb03$salinity[match(dat1$Harbour,harb03$Harbour)]
  dat1$salin <- as.numeric(as.character(dat1$salin))
  # reorder the data frame
  dat1 <- dat1[order(dat1$salin),]
  #get color palette
  #https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
  if(!require(viridis)){
    install.packages("viridis")
  }
  library(viridis)
  #Get unique salinity values and assign colors to them 
  un.sal <- unique(salin.f.harbour)
  l.un.sal <- length(un.sal)
  vcol.sal <- viridis(l.un.sal, option = "D")
  
  vir.col_df <- as.data.frame(cbind(un.sal,vcol.sal))
  vir.col2 <- vir.col_df$vcol.sal[match(salin.f.harbour,vir.col_df$un.sal)]
  # Classical MDS
  # N rows (objects) x p columns (variables)
  # each row identified by a unique row name
  
  d <- dist(mds01_df) # euclidean distances between the rows
  fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
  #fit # view results
  #replace for harbours that will end up being equal to others
  dat1$Harbour[(dat1$Harbour=="KalundborgStatiolHavn")] <- "KAS"
  dat1$Harbour[(dat1$Harbour=="Aalborgportland")] <- "ALP"
  dat1$Harbour[(dat1$Harbour=="Fredericia")] <- "FRC"
  dat1$Harbour[(dat1$Harbour=="Frederikshavn")] <- "FRH"
  dat1$Harbour[(dat1$Harbour=="Kalundborg")] <- "KAB"
  dat1$Harbour[(dat1$Harbour=="Koebenhavn")] <- "KOB"
  dat1$Harbour[(dat1$Harbour=="Koege")] <- "KOG"
  dat1$Harbour[(dat1$Harbour=="AalborgHavn")] <- "ALH"
  #convert to uppercase
  dat1$Harbour <- toupper(substr(dat1$Harbour,1,3))
  # get number of colors for harbours
  nch <- length(unique(dat1$salin))
  colvc <- c("white","yellow",
             "darkcyan","purple")
  #reverse the order of the vector
  #colvc <-  rev(colvc)
  colfhrb <- (grDevices::colorRampPalette(colvc))(nch)
  df_cfh01 <- as.data.frame(cbind(unique(dat1$salin),colfhrb))
  colnames(df_cfh01) <- c("salin2","hxcol")
  dat1$hxcl2 <- df_cfh01$hxcol[match(dat1$salin,
                                     df_cfh01$salin2)]
  #https://stackoverflow.com/questions/28461326/convert-hex-color-code-to-color-name
  library(plotrix)
  #sapply(unique(dat1$hxcl2), color.id)
  # get number of colors for harbours
  nch <- length(sort(unique(dat1$Harbour)))
  colvc <- c("white","yellow","orange","green","steelblue2",
             "darkcyan","purple","black")
  colfhrb <- (grDevices::colorRampPalette(colvc))(nch)
  df_cfh02 <- as.data.frame(cbind(unique(dat1$Harbour),colfhrb))
  colnames(df_cfh02) <- c("harb.abbr3","hxcol")
  dat1$hxcl3 <- df_cfh02$hxcol[match(dat1$Harbour,
                                     df_cfh02$harb.abbr3)]
  #count unique elements in column and use for making pch symbols to
  # macth back to data frame
  nchs <- length(unique(dat1$salin))
  pchv <- seq(21,20+nchs,by=1)
  df_cfh03 <- as.data.frame(cbind(unique(dat1$salin),pchv))
  colnames(df_cfh03) <- c("salin3","pchsymb")
  dat1$pchsymb <- df_cfh03$pchsymb[match(dat1$salin,
                                         df_cfh03$salin3)]
  dat1$pchsymb <- as.numeric(dat1$pchsymb)
  #replace Danish seasons to months
  dat1$season <- gsub("foraar","Jun_Jul",dat1$season)
  dat1$season <- gsub("efteraar","Sep_Nov",dat1$season)
  #count unique elements in column and use for making pch symbols to
  # macth back to data frame
  nchs <- length(unique(dat1$season))
  pchv <- seq(21,20+nchs,by=1)
  df_cfh03 <- as.data.frame(cbind(unique(dat1$season),pchv))
  colnames(df_cfh03) <- c("season3","pchsymb2")
  dat1$pchsymb2 <- df_cfh03$pchsymb2[match(dat1$season,
                                           df_cfh03$season3)]
  dat1$pchsymb2 <- as.numeric(dat1$pchsymb2)
  #count unique elements in column and use for making pch symbols to
  # macth back to data frame
  nchs <- length(unique(dat1$salin))
  pchv <- seq(21,20+nchs,by=1)
  df_cfh03 <- as.data.frame(cbind(unique(dat1$salin),pchv))
  colnames(df_cfh03) <- c("salin3","pchsymb")
  dat1$pchsymb <- df_cfh03$pchsymb[match(dat1$salin,
                                         df_cfh03$salin3)]
  #count unique elements in column and use for making pch symbols to
  # macth back to data frame
  nchs <- length(unique(dat1$season))
  pchv <- seq(1,nchs,by=1)
  pchv <- c("lightgreen","orange")
  df_cfh03 <- as.data.frame(cbind(unique(dat1$season),pchv))
  colnames(df_cfh03) <- c("season3","colfs")
  dat1$colfs <- df_cfh03$colfs[match(dat1$season,
                                     df_cfh03$season3)]
  #substitute in season names
  dat1$season <- gsub("foraar","Jun-Jul",dat1$season) 
  dat1$season <- gsub("efteraar","Sep-Nov",dat1$season) 
  #get column as a vector
  va <- dat1$Harbour
  #replace every third element
  va[seq(1, length(va), 3)] <- ""
  va[seq(2, length(va), 3)] <- ""
  #replace column - this will make only one harbour name appear per
  # cluster of three points
  dat1$va <- va
  # plot solution
  x <- fit$points[,1]
  y <- fit$points[,2]
  plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
       main="Metric MDS", #type="n")
       #, pch=21, col="black", bg="red")
       pch=dat1$pchsymb, col="black", bg=dat1$hxcl3, cex=1.2)
  #pch=21, col="black", bg=dat1$hxcl2, cex=2.4)
  # points(x,y,
  #        pch=21, col="black",  bg=dat1$hxcl2, cex=1)
  #text(x, y, labels = row.names(mds01_df), cex=.7) 
  #text(x, y, labels = dat1$Harbour, cex=.4)#, pos=3) 
  legend("topright", title="harbour",
         legend=sort(unique(dat1$Harbour)),
         pch=21,
         inset=c(-0.1, 0),
         pt.bg=unique(dat1$hxcl3),
         cex=0.6,
         text.width = 0.6)
  legend("topleft", title="salinity",
         legend=unique(dat1$salin),
         pch=unique(dat1$pchsymb),
         #pch=21,
         inset=c(-0.1, 0),
         pt.bg="black",
         #pt.bg=unique(dat1$hxcl2),
         cex=0.6,
         text.width = 0.6)
  # Nonmetric MDS
  # N rows (objects) x p columns (variables)
  # each row identified by a unique row name
  
  library(MASS)
  d <- dist(mds01_df) # euclidean distances between the rows
  fit <- isoMDS(d, k=2) # k is the number of dim
  #fit # view results
  
  # plot solution
  x <- fit$points[,1]
  y <- fit$points[,2]
  #dev.off()
  plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
       main="Nonmetric MDS", #type="n")
       #pch=21, col="black", bg=dat1$hxcl2, cex=2.4)
       pch=dat1$pchsymb, col="black", bg=dat1$hxcl3, cex=1.2)
  # points(x,y,
  #        pch=21, col="black",  bg=dat1$hxcl2, cex=1)
  #text(x, y, labels = row.names(mds01_df), cex=.7) 
  #text(x, y, labels = dat1$Harbour, cex=.4)#, pos=3) 
  legend("topright", title="harbour",
         legend=sort(unique(dat1$Harbour)),
         pch=21,
         inset=c(-0.1, 0),
         pt.bg=unique(dat1$hxcl3),
         cex=0.6,
         text.width = 0.6)
  legend("topleft", title="salinity",
         legend=unique(dat1$salin),
         pch=unique(dat1$pchsymb),
         #pch=21,
         inset=c(-0.1, 0),
         pt.bg="black",
         #pt.bg=unique(dat1$hxcl2),
         cex=0.6,
         text.width = 0.6)
  # end pdf file
  # end pdf file
  # end file to save as file
  dev.off()
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #  make MultiDimensional Scaling plots of harbours   tryout 03  - end
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #  make MultiDimensional Scaling plots of harbours   tryout 04 - start
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #make a name for the files to be saved
  pdffn = paste(wd00,wd10,"/suppmatr_10.15g_App_K_eDNAlog10_per_L_MDS_10.pdf", sep="")
  pdf(pdffn 
      ,width=6, height=8
      #,width=(1.6*8.2677),height=(1.6*2.9232)
  )
  par(mfrow=c(2,1) # c(2,1) will give 2 rows and 1 column
      , mar=c(4,4,4,4)
      , oma=c(1,1,1,1)
      #https://stackoverflow.com/questions/3932038/plot-a-legend-outside-of-the-plotting-area-in-base-graphics
      , xpd=TRUE
  )
  # Exclude species that are unaffected by salinity levels
  # Karenia_mikimotoi cannot transverse low salinities
  #https://www.jstage.jst.go.jp/article/pbr/9/1/9_42/_article
  
  df_g01<- smpls02.11[!grepl("Mnemi",colnames(smpls02.11)),]
  df_g01<- smpls02.11[!grepl("Mya",colnames(df_g01)),]
  df_g01<- smpls02.11[!grepl("Crasso",colnames(df_g01)),]
  df_g01<- smpls02.11[!grepl("Colp",colnames(df_g01)),]
  smpls02.12 <- df_g01
  #https://www.statmethods.net/advstats/mds.html
  #mds01_df<- t(smpls02.11)
  mds01_df<- smpls02.12
  #use strpsplit to split by point
  dat1 <- data.frame(do.call(rbind, strsplit(as.vector(smpls02.11$Harbour.season.id), split = "[.]")))
  #use strsplit to split the column with names
  names(dat1) <- c(unlist(strsplit(colnames(smpls02.11[1]),"[.]")))
  #use the salinity from earlier
  #dat1$Harbour <- as.character(dat1$Harbour)
  salin.f.harbour <- harb03$salinity[match(dat1$Harbour,harb03$Harbour)]
  #match 
  dat1$salin <- harb03$salinity[match(dat1$Harbour,harb03$Harbour)]
  dat1$salin <- as.numeric(as.character(dat1$salin))
  # reorder the data frame
  dat1 <- dat1[order(dat1$salin),]
  #get color palette
  #https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
  if(!require(viridis)){
    install.packages("viridis")
  }
  library(viridis)
  #Get unique salinity values and assign colors to them 
  un.sal <- unique(salin.f.harbour)
  l.un.sal <- length(un.sal)
  vcol.sal <- viridis(l.un.sal, option = "D")
  
  vir.col_df <- as.data.frame(cbind(un.sal,vcol.sal))
  vir.col2 <- vir.col_df$vcol.sal[match(salin.f.harbour,vir.col_df$un.sal)]
  # Classical MDS
  # N rows (objects) x p columns (variables)
  # each row identified by a unique row name
  
  d <- dist(mds01_df) # euclidean distances between the rows
  fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
  #fit # view results
  #replace for harbours that will end up being equal to others
  dat1$Harbour[(dat1$Harbour=="KalundborgStatiolHavn")] <- "KAS"
  dat1$Harbour[(dat1$Harbour=="Aalborgportland")] <- "ALP"
  dat1$Harbour[(dat1$Harbour=="Fredericia")] <- "FRC"
  dat1$Harbour[(dat1$Harbour=="Frederikshavn")] <- "FRH"
  dat1$Harbour[(dat1$Harbour=="Kalundborg")] <- "KAB"
  dat1$Harbour[(dat1$Harbour=="Koebenhavn")] <- "KOB"
  dat1$Harbour[(dat1$Harbour=="Koege")] <- "KOG"
  dat1$Harbour[(dat1$Harbour=="AalborgHavn")] <- "ALH"
  #convert to uppercase
  dat1$Harbour <- toupper(substr(dat1$Harbour,1,3))
  # get number of colors for harbours
  nch <- length(unique(dat1$salin))
  colvc <- c("white","yellow",
             "darkcyan","purple")
  #reverse the order of the vector
  #colvc <-  rev(colvc)
  colfhrb <- (grDevices::colorRampPalette(colvc))(nch)
  df_cfh01 <- as.data.frame(cbind(unique(dat1$salin),colfhrb))
  colnames(df_cfh01) <- c("salin2","hxcol")
  dat1$hxcl2 <- df_cfh01$hxcol[match(dat1$salin,
                                     df_cfh01$salin2)]
  #https://stackoverflow.com/questions/28461326/convert-hex-color-code-to-color-name
  library(plotrix)
  #sapply(unique(dat1$hxcl2), color.id)
  # get number of colors for harbours
  nch <- length(sort(unique(dat1$Harbour)))
  colvc <- c("white","yellow","orange","green","steelblue2",
             "darkcyan","purple","black")
  colfhrb <- (grDevices::colorRampPalette(colvc))(nch)
  df_cfh02 <- as.data.frame(cbind(unique(dat1$Harbour),colfhrb))
  colnames(df_cfh02) <- c("harb.abbr3","hxcol")
  dat1$hxcl3 <- df_cfh02$hxcol[match(dat1$Harbour,
                                     df_cfh02$harb.abbr3)]
  #count unique elements in column and use for making pch symbols to
  # macth back to data frame
  nchs <- length(unique(dat1$salin))
  pchv <- seq(21,20+nchs,by=1)
  df_cfh03 <- as.data.frame(cbind(unique(dat1$salin),pchv))
  colnames(df_cfh03) <- c("salin3","pchsymb")
  dat1$pchsymb <- df_cfh03$pchsymb[match(dat1$salin,
                                         df_cfh03$salin3)]
  dat1$pchsymb <- as.numeric(dat1$pchsymb)
  #replace Danish seasons to months
  dat1$season <- gsub("foraar","Jun_Jul",dat1$season)
  dat1$season <- gsub("efteraar","Sep_Nov",dat1$season)
  #count unique elements in column and use for making pch symbols to
  # macth back to data frame
  nchs <- length(unique(dat1$season))
  pchv <- seq(21,20+nchs,by=1)
  df_cfh03 <- as.data.frame(cbind(unique(dat1$season),pchv))
  colnames(df_cfh03) <- c("season3","pchsymb2")
  dat1$pchsymb2 <- df_cfh03$pchsymb2[match(dat1$season,
                                           df_cfh03$season3)]
  dat1$pchsymb2 <- as.numeric(dat1$pchsymb2)
  #count unique elements in column and use for making pch symbols to
  # macth back to data frame
  nchs <- length(unique(dat1$salin))
  pchv <- seq(21,20+nchs,by=1)
  df_cfh03 <- as.data.frame(cbind(unique(dat1$salin),pchv))
  colnames(df_cfh03) <- c("salin3","pchsymb")
  dat1$pchsymb <- df_cfh03$pchsymb[match(dat1$salin,
                                         df_cfh03$salin3)]
  #count unique elements in column and use for making pch symbols to
  # macth back to data frame
  nchs <- length(unique(dat1$season))
  pchv <- seq(1,nchs,by=1)
  pchv <- c("lightgreen","orange")
  df_cfh03 <- as.data.frame(cbind(unique(dat1$season),pchv))
  colnames(df_cfh03) <- c("season3","colfs")
  dat1$colfs <- df_cfh03$colfs[match(dat1$season,
                                     df_cfh03$season3)]
  #substitute in season names
  dat1$season <- gsub("foraar","Jun-Jul",dat1$season) 
  dat1$season <- gsub("efteraar","Sep-Nov",dat1$season) 
  #get column as a vector
  va <- dat1$Harbour
  #replace every third element
  va[seq(1, length(va), 3)] <- ""
  va[seq(2, length(va), 3)] <- ""
  #replace column - this will make only one harbour name appear per
  # cluster of three points
  dat1$va <- va
  # plot solution
  x <- fit$points[,1]
  y <- fit$points[,2]
  plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
       main="Metric MDS", #type="n")
       #, pch=21, col="black", bg="red")
       pch=dat1$pchsymb, col="black", bg=dat1$hxcl3, cex=1.2)
  #pch=21, col="black", bg=dat1$hxcl2, cex=2.4)
  # points(x,y,
  #        pch=21, col="black",  bg=dat1$hxcl2, cex=1)
  #text(x, y, labels = row.names(mds01_df), cex=.7) 
  text(x, y, labels = dat1$va, cex=.4, pos=3) 
  legend("topright", title="harbour",
         legend=sort(unique(dat1$Harbour)),
         pch=21,
         inset=c(-0.1, 0),
         pt.bg=unique(dat1$hxcl3),
         cex=0.6,
         text.width = 0.6)
  legend("topleft", title="salinity",
         legend=unique(dat1$salin),
         pch=unique(dat1$pchsymb),
         #pch=21,
         inset=c(-0.1, 0),
         pt.bg="black",
         #pt.bg=unique(dat1$hxcl2),
         cex=0.6,
         text.width = 0.6)
  # Nonmetric MDS
  # N rows (objects) x p columns (variables)
  # each row identified by a unique row name
  
  library(MASS)
  d <- dist(mds01_df) # euclidean distances between the rows
  fit <- isoMDS(d, k=2) # k is the number of dim
  #fit # view results
  
  # plot solution
  x <- fit$points[,1]
  y <- fit$points[,2]
  #dev.off()
  plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
       main="Nonmetric MDS", #type="n")
       #pch=21, col="black", bg=dat1$hxcl2, cex=2.4)
       pch=dat1$pchsymb, col="black", bg=dat1$hxcl3, cex=1.2)
  # points(x,y,
  #        pch=21, col="black",  bg=dat1$hxcl2, cex=1)
  #text(x, y, labels = row.names(mds01_df), cex=.7) 
  text(x, y, labels = dat1$va, cex=.4, pos=3) 
  legend("topright", title="harbour",
         legend=sort(unique(dat1$Harbour)),
         pch=21,
         inset=c(-0.1, 0),
         pt.bg=unique(dat1$hxcl3),
         cex=0.6,
         text.width = 0.6)
  legend("topleft", title="salinity",
         legend=unique(dat1$salin),
         pch=unique(dat1$pchsymb),
         #pch=21,
         inset=c(-0.1, 0),
         pt.bg="black",
         #pt.bg=unique(dat1$hxcl2),
         cex=0.6,
         text.width = 0.6)
  # end pdf file
  # end pdf file
  # end file to save as file
  dev.off()
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #  make MultiDimensional Scaling plots of harbours   tryout 03  - end
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #  make MultiDimensional Scaling plots of harbours   tryout 04 - start
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #make a name for the files to be saved
  pdffn = paste(wd00,wd10,"/suppmatr_10.15g_App_K_eDNAlog10_per_L_MDS_11.pdf", sep="")
  pdf(pdffn 
      ,width=6, height=8
      #,width=(1.6*8.2677),height=(1.6*2.9232)
  )
  par(mfrow=c(2,1) # c(2,1) will give 2 rows and 1 column
      , mar=c(4,4,4,4)
      , oma=c(1,1,1,1)
      #https://stackoverflow.com/questions/3932038/plot-a-legend-outside-of-the-plotting-area-in-base-graphics
      , xpd=TRUE
  )
  # Exclude species that are unaffected by salinity levels
  # Karenia_mikimotoi cannot transverse low salinities
  #https://www.jstage.jst.go.jp/article/pbr/9/1/9_42/_article
  
  df_g01<- smpls02.11[!grepl("Mnemi",colnames(smpls02.11)),]
  df_g01<- smpls02.11[!grepl("Mya",colnames(df_g01)),]
  df_g01<- smpls02.11[!grepl("Crasso",colnames(df_g01)),]
  df_g01<- smpls02.11[!grepl("Colp",colnames(df_g01)),]
  smpls02.12 <- df_g01
  #https://www.statmethods.net/advstats/mds.html
  #mds01_df<- t(smpls02.11)
  mds01_df<- smpls02.12
  #replace for harbours that will end up being equal to others
  dat1$Harbour[(dat1$Harbour=="KalundborgStatiolHavn")] <- "KAS"
  dat1$Harbour[(dat1$Harbour=="Aalborgportland")] <- "ALP"
  dat1$Harbour[(dat1$Harbour=="Fredericia")] <- "FRC"
  dat1$Harbour[(dat1$Harbour=="Frederikshavn")] <- "FRH"
  dat1$Harbour[(dat1$Harbour=="Kalundborg")] <- "KAB"
  dat1$Harbour[(dat1$Harbour=="Koebenhavn")] <- "KOB"
  dat1$Harbour[(dat1$Harbour=="Koege")] <- "KOG"
  dat1$Harbour[(dat1$Harbour=="AalborgHavn")] <- "ALH"
  #convert to uppercase
  dat1$Harbour <- toupper(substr(dat1$Harbour,1,3))
  
  #use strpsplit to split by point
  dat1 <- data.frame(do.call(rbind, strsplit(as.vector(smpls02.11$Harbour.season.id), split = "[.]")))
  #use strsplit to split the column with names
  names(dat1) <- c(unlist(strsplit(colnames(smpls02.11[1]),"[.]")))
  #use the salinity from earlier
  #dat1$Harbour <- as.character(dat1$Harbour)
  salin.f.harbour <- harb03$salinity[match(dat1$Harbour,harb03$Harbour)]
  #match 
  dat1$salin <- harb03$salinity[match(dat1$Harbour,harb03$Harbour)]
  dat1$salin <- as.numeric(as.character(dat1$salin))
  # reorder the data frame
  dat1 <- dat1[order(dat1$salin),]
  #get color palette
  #https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
  if(!require(viridis)){
    install.packages("viridis")
  }
  library(viridis)
  #Get unique salinity values and assign colors to them 
  un.sal <- unique(salin.f.harbour)
  l.un.sal <- length(un.sal)
  vcol.sal <- viridis(l.un.sal, option = "D")
  
  vir.col_df <- as.data.frame(cbind(un.sal,vcol.sal))
  vir.col2 <- vir.col_df$vcol.sal[match(salin.f.harbour,vir.col_df$un.sal)]
  # Classical MDS
  # N rows (objects) x p columns (variables)
  # each row identified by a unique row name
  
  d <- dist(mds01_df) # euclidean distances between the rows
  fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
  #fit # view results
  #replace for harbours that will end up being equal to others
  dat1$Harbour[(dat1$Harbour=="KalundborgStatiolHavn")] <- "KAS"
  dat1$Harbour[(dat1$Harbour=="Aalborgportland")] <- "ALP"
  dat1$Harbour[(dat1$Harbour=="Fredericia")] <- "FRC"
  dat1$Harbour[(dat1$Harbour=="Frederikshavn")] <- "FRH"
  dat1$Harbour[(dat1$Harbour=="Kalundborg")] <- "KAB"
  dat1$Harbour[(dat1$Harbour=="Koebenhavn")] <- "KOB"
  dat1$Harbour[(dat1$Harbour=="Koege")] <- "KOG"
  dat1$Harbour[(dat1$Harbour=="AalborgHavn")] <- "ALH"
  #convert to uppercase
  dat1$Harbour <- toupper(substr(dat1$Harbour,1,3))
  # get number of colors for harbours
  nch <- length(unique(dat1$salin))
  colvc <- c("white","yellow",
             "darkcyan","purple")
  #reverse the order of the vector
  #colvc <-  rev(colvc)
  colfhrb <- (grDevices::colorRampPalette(colvc))(nch)
  df_cfh01 <- as.data.frame(cbind(unique(dat1$salin),colfhrb))
  colnames(df_cfh01) <- c("salin2","hxcol")
  dat1$hxcl2 <- df_cfh01$hxcol[match(dat1$salin,
                                     df_cfh01$salin2)]
  #https://stackoverflow.com/questions/28461326/convert-hex-color-code-to-color-name
  library(plotrix)
  #sapply(unique(dat1$hxcl2), color.id)
  # get number of colors for harbours
  nch <- length(sort(unique(dat1$Harbour)))
  colvc <- c("white","red","pink","orange","yellow",
             "green","cyan",
             "steelblue2",
             "darkcyan","blue","plum3","purple","black")
  colfhrb <- (grDevices::colorRampPalette(colvc))(nch)
  df_cfh02 <- as.data.frame(cbind(unique(dat1$Harbour),colfhrb))
  colnames(df_cfh02) <- c("harb.abbr3","hxcol")
  dat1$hxcl3 <- df_cfh02$hxcol[match(dat1$Harbour,
                                     df_cfh02$harb.abbr3)]
  #count unique elements in column and use for making pch symbols to
  # macth back to data frame
  nchs <- length(unique(dat1$salin))
  pchv <- seq(21,20+nchs,by=1)
  df_cfh03 <- as.data.frame(cbind(unique(dat1$salin),pchv))
  colnames(df_cfh03) <- c("salin3","pchsymb")
  dat1$pchsymb <- df_cfh03$pchsymb[match(dat1$salin,
                                         df_cfh03$salin3)]
  dat1$pchsymb <- as.numeric(dat1$pchsymb)
  #replace Danish seasons to months
  dat1$season <- gsub("foraar","Jun_Jul",dat1$season)
  dat1$season <- gsub("efteraar","Sep_Nov",dat1$season)
  #count unique elements in column and use for making pch symbols to
  # macth back to data frame
  nchs <- length(unique(dat1$season))
  pchv <- seq(21,20+nchs,by=1)
  df_cfh03 <- as.data.frame(cbind(unique(dat1$season),pchv))
  colnames(df_cfh03) <- c("season3","pchsymb2")
  dat1$pchsymb2 <- df_cfh03$pchsymb2[match(dat1$season,
                                           df_cfh03$season3)]
  dat1$pchsymb2 <- as.numeric(dat1$pchsymb2)
  #count unique elements in column and use for making pch symbols to
  # macth back to data frame
  nchs <- length(unique(dat1$salin))
  pchv <- seq(21,20+nchs,by=1)
  df_cfh03 <- as.data.frame(cbind(unique(dat1$salin),pchv))
  colnames(df_cfh03) <- c("salin3","pchsymb")
  dat1$pchsymb <- df_cfh03$pchsymb[match(dat1$salin,
                                         df_cfh03$salin3)]
  dat1$pchsymb <- as.numeric(dat1$pchsymb)
  #count unique elements in column and use for making pch symbols to
  # macth back to data frame
  nchs <- length(unique(dat1$season))
  pchv <- seq(1,nchs,by=1)
  pchv <- c("lightgreen","orange")
  df_cfh03 <- as.data.frame(cbind(unique(dat1$season),pchv))
  colnames(df_cfh03) <- c("season3","colfs")
  dat1$colfs <- df_cfh03$colfs[match(dat1$season,
                                     df_cfh03$season3)]
  #substitute in season names
  dat1$season <- gsub("foraar","Jun-Jul",dat1$season) 
  dat1$season <- gsub("efteraar","Sep-Nov",dat1$season) 
  #get column as a vector
  va <- dat1$Harbour
  #replace every third element
  va[seq(1, length(va), 3)] <- ""
  va[seq(2, length(va), 3)] <- ""
  #replace column - this will make only one harbour name appear per
  # cluster of three points
  dat1$va <- va
  # plot solution
  x <- fit$points[,1]
  y <- fit$points[,2]
  plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
       main="Metric MDS", #type="n")
       #, pch=21, col="black", bg="red")
       pch=dat1$pchsymb, col="black", bg=dat1$hxcl3, cex=1.2)
  #pch=21, col="black", bg=dat1$hxcl2, cex=2.4)
  # points(x,y,
  #        pch=21, col="black",  bg=dat1$hxcl2, cex=1)
  #text(x, y, labels = row.names(mds01_df), cex=.7) 
  #text(x, y, labels = dat1$va, cex=.4, pos=3) 
  legend("topright", title="harbour",
         legend=sort(unique(dat1$Harbour)),
         pch=21,
         inset=c(-0.1, 0),
         pt.bg=unique(dat1$hxcl3),
         cex=0.6,
         text.width = 0.6)
  legend("topleft", title="salinity",
         legend=unique(dat1$salin),
         pch=unique(dat1$pchsymb),
         #pch=21,
         inset=c(-0.1, 0),
         pt.bg="black",
         #pt.bg=unique(dat1$hxcl2),
         cex=0.6,
         text.width = 0.6)
  # Nonmetric MDS
  # N rows (objects) x p columns (variables)
  # each row identified by a unique row name
  
  library(MASS)
  d <- dist(mds01_df) # euclidean distances between the rows
  fit <- isoMDS(d, k=2) # k is the number of dim
  #fit # view results
  
  # plot solution
  x <- fit$points[,1]
  y <- fit$points[,2]
  #dev.off()
  plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
       main="Nonmetric MDS", #type="n")
       #pch=21, col="black", bg=dat1$hxcl2, cex=2.4)
       pch=dat1$pchsymb, col="black", bg=dat1$hxcl3, cex=1.2)
  # points(x,y,
  #        pch=21, col="black",  bg=dat1$hxcl2, cex=1)
  #text(x, y, labels = row.names(mds01_df), cex=.7) 
  #text(x, y, labels = dat1$va, cex=.4, pos=3) 
  legend("topright", title="harbour",
         legend=sort(unique(dat1$Harbour)),
         pch=21,
         inset=c(-0.1, 0),
         pt.bg=unique(dat1$hxcl3),
         cex=0.6,
         text.width = 0.6)
  legend("topleft", title="salinity",
         legend=unique(dat1$salin),
         pch=unique(dat1$pchsymb),
         #pch=21,
         inset=c(-0.1, 0),
         pt.bg="black",
         #pt.bg=unique(dat1$hxcl2),
         cex=0.6,
         text.width = 0.6)
  # end pdf file
  # end pdf file
  # end file to save as file
  dev.off()
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #  make MultiDimensional Scaling plots of harbours   tryout 03  - end
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  library(tidyverse)
  #calculate principal components
  df_s01 <- smpls02.11[,-1]
  results2 <- prcomp(df_s01, scale = TRUE)
  #reverse the signs
  results2$rotation <- -1*results2$rotation
  #display principal components
  results2$rotation
  #reverse the signs of the scores
  results2$x <- -1*results2$x
  #display the first six scores
  #head(results2$x)
  biplot(results2, scale = 0)
  
  
  #__
  #____________________________________________________________________
  # https://www.statology.org/linear-discriminant-analysis-in-r/
  # Linear Discriminant Analysis in R# Step 1: Load Necessary Libraries
  library(MASS)
  library(ggplot2)
  # Step 2: Load the Data
  df_lda01 <-  smpls02.11
  # substitute in column
  df_lda01$Harbour.season <- gsub("(.*)\\.(.*)","\\1",df_lda01$Harbour.season.id)
  #delete column
  df_lda01$Harbour.season.id <- NULL
  # Step 3: Scale the Data
  # One of the key assumptions of linear discriminant analysis 
  # is that each of the predictor variables have the same variance. 
  # An easy way to assure that this assumption is met is to scale 
  #each variable such that it has a mean of 0 and a standard 
  # deviation of 1.
  # We can quickly do so in R by using the scale() function:
  #scale each predictor variable 
  ncdflda <- length(df_lda01)
  df_lda01[1:(ncdflda-1)] <- scale(df_lda01[1:(ncdflda-1)])
  # Next, we’ll split the dataset into a training set to train the model on and a testing set to test the model on:
  #   #make this example reproducible
  set.seed(1)
  #Use 70% of dataset as training set and remaining 30% as 
  #testing set
  sample2 <- sample(c(TRUE, FALSE), 
                    nrow(df_lda01), replace=TRUE, 
                    prob=c(0.7,0.3))
  train2 <- df_lda01[sample2, ]
  test2 <- df_lda01[!sample2, ] 
  # Step 5: Fit the LDA Model
  # Next, we’ll use the lda() function from the MASS package to fit the LDA model to our data:
  #   #fit LDA model
  model2 <- lda(Harbour.season~., data=train2)
  # Proportion of trace: These display the percentage separation achieved by each linear discriminant function.
  # Step 6: Use the Model to Make Predictions
  # Once we’ve fit the model using our training data, we can use it to make predictions on our test data:
  #use LDA model to make predictions on test data
  predicted2 <- predict(model2, test2)
  # posterior: The posterior probability that an observation belongs to each class
  # x: The linear discriminants
  # We can quickly view each of these results for the first six observations in our test dataset:
  #   #view predicted class for first six observations in test set
  #head(predicted2$class)
  #view linear discriminants for first six observations in test set
  #head(predicted2$x)
  # We can use the following code to see what percentage of observations the LDA model correctly predicted the Species for:
  #find accuracy of model
  mean(predicted2$class==test2$Harbour.season)
  # It turns out that the model correctly predicted the Species for 100% of the observations in our test dataset.
  # In the real-world an LDA model will rarely predict every 
  # class outcome correctly, but this iris2 dataset is simply built 
  # in a way that machine learning algorithms tend to perform very 
  # well on it.
  # Step 7: Visualize the Results
  #define data to plot
  lda_plot2 <- cbind(train2, predict(model2)$x)
  #head(lda_plot2)
  # get proportion of each trace
  #https://stackoverflow.com/questions/23163157/extract-lda-linear-discriminant-analysis-data-in-r
  lst_proptrcLd <-  prop.table(model2$svd^2)
  #lda_plot2$Harbour.season
  df_hs02 <- data.frame(do.call('rbind', strsplit(as.character(lda_plot2$Harbour.season),'.',fixed=TRUE)))
  colnames(df_hs02) <- c("harb","season")
  lda_plot2$Harbour <- df_hs02$harb
  lda_plot2$season <- df_hs02$season
  dev.off()
  #create plot
  ggplot(lda_plot2, aes(LD1, LD2)) +
    geom_point(aes(color = Harbour))
  
  ggplot(lda_plot2, aes(LD1, LD2)) +
    geom_point(aes(color = season))
  #____
  set.seed(1)
  LD1 <- lda_plot2$LD1
  LD2 <- lda_plot2$LD2
  LD3 <- lda_plot2$LD3
  #replace for harbours that will end up being equal to others
  lda_plot2$Harbour[(lda_plot2$Harbour=="KalundborgStatiolHavn")] <- "KAS"
  lda_plot2$Harbour[(lda_plot2$Harbour=="Aalborgportland")] <- "ALP"
  lda_plot2$Harbour[(lda_plot2$Harbour=="Fredericia")] <- "FRC"
  lda_plot2$Harbour[(lda_plot2$Harbour=="Frederikshavn")] <- "FRH"
  lda_plot2$Harbour[(lda_plot2$Harbour=="Kalundborg")] <- "KAB"
  lda_plot2$Harbour[(lda_plot2$Harbour=="Koebenhavn")] <- "KOB"
  lda_plot2$Harbour[(lda_plot2$Harbour=="Koege")] <- "KOG"
  lda_plot2$Harbour[(lda_plot2$Harbour=="AalborgHavn")] <- "ALH"
  #convert to uppercase
  lda_plot2$Harbour <- toupper(substr(lda_plot2$Harbour,1,3))
  #get proportion, round, and paste to get new axis labels
  ld1lab <- paste("LD1 (",round(100*(lst_proptrcLd[1]),digits = 1),"%)",sep="")
  ld2lab <- paste("LD2 (",round(100*(lst_proptrcLd[2]),digits = 1),"%)",sep="")
  ld3lab <- paste("LD3 (",round(100*(lst_proptrcLd[3]),digits = 1),"%)",sep="")
  # 3D Scatterplot
  require(scatterplot3d)
  library(scatterplot3d)
  #make a name for the files to be saved
  pdffn = paste(wd00,wd10,"/suppmatr_10.15g_App_K_eDNAlog10_per_L_LDA_01.pdf", sep="")
  pdf(pdffn 
      ,width=6, height=8
      #,width=(1.6*8.2677),height=(1.6*2.9232)
  )
  par(mfrow=c(2,1) # c(2,1) will give 2 rows and 1 column
      , mar=c(4,4,4,4)
      , oma=c(2,2,1,1)
      #https://stackoverflow.com/questions/3932038/plot-a-legend-outside-of-the-plotting-area-in-base-graphics
      , xpd=TRUE
  )
  # Change color order, changing levels order
  # Change group colors
  colors <- c("lightgreen","orange")
  
  s3d <- scatterplot3d(LD1,LD2,LD3, pch = 21, 
                       color = "black", 
                       angle=220,
                       cex.symbols = 2.4,
                       bg=colors[factor(lda_plot2$season, 
                                        levels = c(unique(lda_plot2$season)))],
                       main="LDA_season_smpl",
                       xlab=ld1lab, ylab=ld2lab, zlab=ld3lab)
  #add txt label to points
  # see : https://r.789695.n4.nabble.com/text-labels-in-3d-scatter-td795191.html
  text(s3d$xyz.convert(lda_plot2$LD1,
                       lda_plot2$LD2,
                       lda_plot2$LD3), 
       labels=lda_plot2$Harbour, #pos=1,
       cex=0.4)
  
  #dev.off()
  lda_plot2$salin <- as.numeric(dat1$salin[match(lda_plot2$Harbour,dat1$Harbour)])
  colors <- c("white","yellow","darkgreen","purple")
  library(RColorBrewer)
  s3d <- scatterplot3d(LD1,LD2,LD3, pch = 21, 
                       color = "black", 
                       angle=220,
                       cex.symbols = 2.4,
                       bg=colors[factor(lda_plot2$salin, 
                                        levels = c(unique(lda_plot2$salin)))],
                       main="LDA_salinity",
                       xlab=ld1lab, ylab=ld2lab, zlab=ld3lab)
  #add txt label to points
  # see : https://r.789695.n4.nabble.com/text-labels-in-3d-scatter-td795191.html
  text(s3d$xyz.convert(lda_plot2$LD1,
                       lda_plot2$LD2,
                       lda_plot2$LD3), 
       labels=lda_plot2$Harbour, #pos=1,
       cex=0.4)
  #close pdf
  dev.off()
  #____
  
  #__
  #____________________________________________________________________
  # https://www.statology.org/linear-discriminant-analysis-in-r/
  # Linear Discriminant Analysis in R# Step 1: Load Necessary Libraries
  library(MASS)
  library(ggplot2)
  # Step 2: Load the Data
  df_lda01 <-  smpls02.11
  # substitute in column
  df_lda01$Harbour.season <- gsub("(.*)\\.(.*)","\\1",df_lda01$Harbour.season.id)
  #delete column
  df_lda01$Harbour.season.id <- NULL
  # Step 3: Scale the Data
  # One of the key assumptions of linear discriminant analysis 
  # is that each of the predictor variables have the same variance. 
  # An easy way to assure that this assumption is met is to scale 
  #each variable such that it has a mean of 0 and a standard 
  # deviation of 1.
  # We can quickly do so in R by using the scale() function:
  #scale each predictor variable 
  ncdflda <- length(df_lda01)
  df_lda01[1:(ncdflda-1)] <- scale(df_lda01[1:(ncdflda-1)])
  # Next, we’ll split the dataset into a training set to train the model on and a testing set to test the model on:
  #   #make this example reproducible
  set.seed(1)
  #Use 70% of dataset as training set and remaining 30% as 
  #testing set
  sample2 <- sample(c(TRUE, FALSE), 
                    nrow(df_lda01), replace=TRUE, 
                    prob=c(0.7,0.3))
  train2 <- df_lda01[sample2, ]
  test2 <- df_lda01[!sample2, ] 
  # Step 5: Fit the LDA Model
  # Next, we’ll use the lda() function from the MASS package to fit the LDA model to our data:
  #   #fit LDA model
  model2 <- lda(Harbour.season~., data=train2)
  # Proportion of trace: These display the percentage separation achieved by each linear discriminant function.
  # Step 6: Use the Model to Make Predictions
  # Once we’ve fit the model using our training data, we can use it to make predictions on our test data:
  #use LDA model to make predictions on test data
  predicted2 <- predict(model2, test2)
  # posterior: The posterior probability that an observation belongs to each class
  # x: The linear discriminants
  # We can quickly view each of these results for the first six observations in our test dataset:
  #   #view predicted class for first six observations in test set
  #head(predicted2$class)
  #view linear discriminants for first six observations in test set
  #head(predicted2$x)
  # We can use the following code to see what percentage of observations the LDA model correctly predicted the Species for:
  #find accuracy of model
  mean(predicted2$class==test2$Harbour.season)
  # It turns out that the model correctly predicted the Species for 100% of the observations in our test dataset.
  # In the real-world an LDA model will rarely predict every 
  # class outcome correctly, but this iris2 dataset is simply built 
  # in a way that machine learning algorithms tend to perform very 
  # well on it.
  # Step 7: Visualize the Results
  #define data to plot
  lda_plot2 <- cbind(train2, predict(model2)$x)
  #head(lda_plot2)
  # get proportion of each trace
  #https://stackoverflow.com/questions/23163157/extract-lda-linear-discriminant-analysis-data-in-r
  lst_proptrcLd <-  prop.table(model2$svd^2)
  #lda_plot2$Harbour.season
  df_hs02 <- data.frame(do.call('rbind', strsplit(as.character(lda_plot2$Harbour.season),'.',fixed=TRUE)))
  colnames(df_hs02) <- c("harb","season")
  lda_plot2$Harbour <- df_hs02$harb
  lda_plot2$season <- df_hs02$season
  dev.off()
  #create plot
  ggplot(lda_plot2, aes(LD1, LD2)) +
    geom_point(aes(color = Harbour))
  
  ggplot(lda_plot2, aes(LD1, LD2)) +
    geom_point(aes(color = season))
  #____
  set.seed(1)
  LD1 <- lda_plot2$LD1
  LD2 <- lda_plot2$LD2
  LD3 <- lda_plot2$LD3
  #replace for harbours that will end up being equal to others
  lda_plot2$Harbour[(lda_plot2$Harbour=="KalundborgStatiolHavn")] <- "KAS"
  lda_plot2$Harbour[(lda_plot2$Harbour=="Aalborgportland")] <- "ALP"
  lda_plot2$Harbour[(lda_plot2$Harbour=="Fredericia")] <- "FRC"
  lda_plot2$Harbour[(lda_plot2$Harbour=="Frederikshavn")] <- "FRH"
  lda_plot2$Harbour[(lda_plot2$Harbour=="Kalundborg")] <- "KAB"
  lda_plot2$Harbour[(lda_plot2$Harbour=="Koebenhavn")] <- "KOB"
  lda_plot2$Harbour[(lda_plot2$Harbour=="Koege")] <- "KOG"
  lda_plot2$Harbour[(lda_plot2$Harbour=="AalborgHavn")] <- "ALH"
  #convert to uppercase
  lda_plot2$Harbour <- toupper(substr(lda_plot2$Harbour,1,3))
  #get proportion, round, and paste to get new axis labels
  ld1lab <- paste("LD1 (",round(100*(lst_proptrcLd[1]),digits = 1),"%)",sep="")
  ld2lab <- paste("LD2 (",round(100*(lst_proptrcLd[2]),digits = 1),"%)",sep="")
  ld3lab <- paste("LD3 (",round(100*(lst_proptrcLd[3]),digits = 1),"%)",sep="")
  # get number of colors for harbours
  nch <- length(sort(unique(lda_plot2$Harbour)))
  colvc <- c("white","red","pink","orange","yellow",
             "green","cyan",
             "steelblue2",
             "darkcyan","blue","plum3","purple","black")
  colfhrb <- (grDevices::colorRampPalette(colvc))(nch)
  df_cfh02 <- as.data.frame(cbind(unique(lda_plot2$Harbour),colfhrb))
  colnames(df_cfh02) <- c("harb.abbr3","hxcol")
  lda_plot2$hxcl3 <- df_cfh02$hxcol[match(lda_plot2$Harbour,
                                          df_cfh02$harb.abbr3)]
  #match salinities
  lda_plot2$salin <- dat1$salin[match(lda_plot2$Harbour,dat1$Harbour)]
  #count unique elements in column and use for making pch symbols to
  # macth back to data frame
  nchs <- length(unique(lda_plot2$salin))
  pchv <- seq(21,20+nchs,by=1)
  df_cfh03 <- as.data.frame(cbind(unique(lda_plot2$salin),pchv))
  colnames(df_cfh03) <- c("salin3","pchsymb")
  lda_plot2$pchsymb.sal <- df_cfh03$pchsymb[match(lda_plot2$salin,
                                                  df_cfh03$salin3)]
  lda_plot2$pchsymb.sal <- as.numeric(lda_plot2$pchsymb.sal)
  #replace Danish seasons to months
  dat1$season <- gsub("foraar","Jun_Jul",dat1$season)
  dat1$season <- gsub("efteraar","Sep_Nov",dat1$season)
  #count unique elements in column and use for making pch symbols to
  # macth back to data frame
  nchs <- length(unique(lda_plot2$season))
  pchv <- seq(21,20+nchs,by=1)
  df_cfh03 <- as.data.frame(cbind(unique(lda_plot2$season),pchv))
  colnames(df_cfh03) <- c("season3","pchsymb2")
  lda_plot2$pchsymb.se <- df_cfh03$pchsymb2[match(lda_plot2$season,
                                                  df_cfh03$season3)]
  lda_plot2$pchsymb.se <- as.numeric(lda_plot2$pchsymb.se)
  
  #replace Danish seasons to months
  lda_plot2$season <- gsub("foraar","Jun_Jul",lda_plot2$season)
  
  lda_plot2$season <- gsub("efteraar","Sep_Nov",lda_plot2$season)
  # 3D Scatterplot
  require(scatterplot3d)
  library(scatterplot3d)
  #make a name for the files to be saved
  pdffn = paste(wd00,wd10,"/suppmatr_10.15g_App_K_eDNAlog10_per_L_LDA_02.pdf", sep="")
  pdf(pdffn 
      ,width=6, height=8
      #,width=(1.6*8.2677),height=(1.6*2.9232)
  )
  par(mfrow=c(2,1) # c(2,1) will give 2 rows and 1 column
      , mar=c(1,4,1,4) #bottom , left ,top, right 
      , oma=c(1,4,1,4)
      #https://stackoverflow.com/questions/3932038/plot-a-legend-outside-of-the-plotting-area-in-base-graphics
      , xpd=TRUE
  )
  # Change color order, changing levels order
  # Change group colors
  colors <- c("lightgreen","orange")
  
  s3d <- scatterplot3d(LD1,LD2,LD3, pch = lda_plot2$pchsymb.se, 
                       color = "black", 
                       angle=220,
                       cex.symbols = 1.2,
                       cex.lab=0.9,
                       bg=lda_plot2$hxcl3,
                       main="LDA_season_smpl",
                       xlab=ld1lab, ylab=ld2lab, zlab=ld3lab)
  #add txt label to points
  # see : https://r.789695.n4.nabble.com/text-labels-in-3d-scatter-td795191.html
  # text(s3d$xyz.convert(lda_plot2$LD1,
  #                      lda_plot2$LD2,
  #                      lda_plot2$LD3), 
  #      labels=lda_plot2$Harbour, #pos=1,
  #      cex=0.4)
  legend("topright", title="harbour",
         legend=sort(unique(lda_plot2$Harbour)),
         pch=21,
         inset=c(-0.22, 0),
         pt.bg=unique(lda_plot2$hxcl3),
         cex=0.6,
         text.width = 0.6)
  legend("topleft", title="season",
         legend=unique(lda_plot2$season),
         pch=unique(lda_plot2$pchsymb.se),
         #pch=21,
         inset=c(-0.22, 0),
         pt.bg="black",
         #pt.bg=unique(dat1$hxcl2),
         cex=0.6,
         text.width = 0.6)
  
  #dev.off()
  
  library(RColorBrewer)
  s3d <- scatterplot3d(LD1,LD2,LD3, pch = lda_plot2$pchsymb.sal, 
                       color = "black", 
                       angle=220,
                       cex.symbols = 1.2,
                       cex.lab=0.9,
                       #label.tick.marks = T,
                       
                       bg=lda_plot2$hxcl3,
                       main="LDA_salinity",
                       xlab=ld1lab, ylab=ld2lab, zlab=ld3lab)
  #add txt label to points
  # see : https://r.789695.n4.nabble.com/text-labels-in-3d-scatter-td795191.html
  # text(s3d$xyz.convert(lda_plot2$LD1,
  #                      lda_plot2$LD2,
  #                      lda_plot2$LD3), 
  #      labels=lda_plot2$Harbour, #pos=1,
  #     cex=0.4)
  legend("topright", title="harbour",
         legend=sort(unique(lda_plot2$Harbour)),
         pch=21,
         inset=c(-0.22, 0),
         pt.bg=unique(lda_plot2$hxcl3),
         cex=0.6,
         text.width = 0.6)
  legend("topleft", title="salinity",
         legend=unique(lda_plot2$salin),
         pch=unique(lda_plot2$pchsymb.sal),
         #pch=21,
         inset=c(-0.22, 0),
         pt.bg="black",
         #pt.bg=unique(dat1$hxcl2),
         cex=0.6,
         text.width = 0.6)
  #close pdf
  dev.off()
  #____
  
  
