#-------------------------------------------------------------------------
# BM01BAM Final Assignment: Vaccine Effects on Pharmaceutical Company
#-------------------------------------------------------------------------
# Empty the memory
remove(list=ls())
cat("\f")

#-------------------------------------------------------------------------
# Load libraries
#-------------------------------------------------------------------------
library(plyr)

# Package stargazer for appropriately formatted tables
# install.packages("stargazer", dependencies = TRUE)
library(stargazer)

# Package plm for panel estimation
# install.packages("plm", dependencies = TRUE)
library(plm)

# Package ggplot2 for graphics
# install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)

library(sandwich)

library(tidyverse)

library(reshape)

library(ggforce)

library(reshape2)

library(gridExtra)

library(dplyr)
#-------------------------------------------------------------------------
# Read the data
#-------------------------------------------------------------------------
data <- read.csv("Group_project_20/Data/data.csv")
data$Market.Cap<-data$Market.Cap/1000000000
data = na.omit(data)
data
#-------------------------------------------------------------------------
# summarize the data
#-------------------------------------------------------------------------
stargazer(data, type = "text")
#-------------------------------------------------------------------------
# 1 Panel Data Modeling
#-------------------------------------------------------------------------
mdlA <- cum_change ~ phase_1 + phase_2 + phase_3 + authorized + approved + abandoned + US + Market.Cap

# Estimate pooled model ('pooling')
rsltPool <- plm(mdlA, data = data, model = "pooling")
summary(rsltPool)
rmsePool <- sqrt(sum(residuals(rsltPool)^2)/rsltPool$df.residual)
rmsePool

# Estimate fixed effect model ('within')
rsltFE <- plm(mdlA, data = data, index = ("stock_country") ,model = "within")
summary(rsltFE)
rmseFE <- sqrt(sum(residuals(rsltFE)^2)/rsltFE$df.residual)

# Estimate random effect model ('random')
rsltRE <- plm(mdlA, data = data, index=c("stock_country"),model = "random",random.method = "amemiya")
summary(rsltRE)
df.ehat  <- rsltRE$df.residual
rmseRE  <- sqrt(sum(residuals(rsltRE)^2)/df.ehat)

# Estimate first difference model
rsltFD<- 
  plm(mdlA, data  = data, 
      index = c("stock_country"), 
      model = "fd")
summary(rsltFD)
df.ehat  <- rsltFD$df.residual
rmseFD    <- sqrt(sum(residuals(rsltFD)^2)/df.ehat)

# Tabulate the results
stargazer(rsltPool,rsltFE,rsltRE,rsltFD,align=FALSE,no.space=TRUE,intercept.bottom = TRUE,add.lines = list(c("rmse",rmsePool,rmseFE,rmseRE,rmseFD)))


# Fixed variable coefficients model, separate estimation
# per group
rsltVCFE <- pvcm(mdlA, data = data, model = "within")


summary(rsltVCFE)
round(cbind(VCFE.avg =sapply(coefficients(rsltVCFE), mean, na.rm = TRUE),
            VCFE.std =sapply(coefficients(rsltVCFE), sd, na.rm = TRUE)),3)
nobs <- nrow(data)
df   <- rsltVCFE$df.residual
sse  <- sum(residuals(rsltVCFE)^2)
rmseVCFE <- sqrt(sse/df)
r2   <- summary(rsltVCFE)$rsqr
fobs <- (r2/(nobs-df))/((1-r2)/df)

round(cbind(nobs, df, r2, rmseVCFE, fobs), 3)

tmp <- coefficients(rsltVCFE)
ggplot(tmp, aes(x=Market.Cap)) +
  geom_histogram(fill="purple", colour="black") +
  theme(axis.title = element_text(size=rel(1.1)),
        axis.text = element_text(size=rel(1.1)))
#-------------------------------------------------------------------------
# 2 Panel Data Modeling Test
#-------------------------------------------------------------------------
pooltest(rsltPool, rsltVCFE)
pooltest(rsltFE, rsltVCFE)
pooltest(rsltFD,rsltVCFE)
pooltest(rsltRE,rsltVCFE)


phtest(rsltFE, rsltPool)
pFtest(rsltFE, rsltPool)
#-------------------------------------------------------------------------
# 3 Add columns for Difference in Difference Analysis
#-------------------------------------------------------------------------
data$compvac<-ifelse(data$company<21, "Yes", "No") #New variable if the company produced a vaccine in the end(=Yes) or not(=No)
data$compvacbin<-ifelse(data$company<21, "1", "0") #New binary variable if the company produced a vaccine in the end(=1) or not (=0)

#Creating new binary variables if during a certain month the time frame was that of a covid wave
data$covidwave1<-ifelse(data$month>3, "0", "1") #If it was the first wave
data$covidwave2<-ifelse(data$month>6, "0", "1") #If it was the second wave
data$covidwave3<-ifelse(data$month>11, "0", "1") #If it was the third wave

#-------------------------------------------------------------------------
# 4 Illustration of cumultive change_rate in different waves
#-------------------------------------------------------------------------
#For cumultive change_rate in different waves
#All of the cumulative change rate comparison


Overall_cum_change <-ggplot(data, aes(month, cum_change, color = compvac )) +
  stat_summary(geom = 'line') +
  theme_minimal()+
  ggtitle("Cumulative Change Rate")
Overall_cum_change

#First wave
#The comparison of cumulative change rates for the first wave of Covid
Cumulative_Change_Rate_1st_Wave <-ggplot(data, aes(month, cum_change, color = compvac )) +
  stat_summary(geom = 'line') +
  theme_minimal()+
  geom_vline(xintercept = 3) +
  geom_vline(xintercept = 5) +
  theme_minimal()+
  xlim(2, 5)+
  ggtitle("Cumulative_Change_Rate_1st_Wave")




#Second wave
#The comparison of cumulative change rates for the second wave of Covid
Cumulative_Change_Rate_2nd_Wave <-ggplot(data, aes(month, cum_change, color = compvac )) +
  stat_summary(geom = 'line') +
  theme_minimal()+
  geom_vline(xintercept = 6) +
  geom_vline(xintercept = 7) +
  theme_minimal()+
  xlim(5, 8)+
  ggtitle("Cumulative_Change_Rate_2nd_Wave")



#Third wave
#The comparison of cumulative change rates for the second wave of Covid
Cumulative_Change_Rate_3rd_Wave <-ggplot(data, aes(month, cum_change, color = compvac )) +
  stat_summary(geom = 'line') +
  theme_minimal()+
  geom_vline(xintercept = 9) +
  geom_vline(xintercept = 12) +
  theme_minimal()+
  xlim(8,13)+
  ggtitle("Cumulative_Change_Rate_3rd_Wave")


#After covid Waves

Cumulative_Change_Rate_After_Waves <-ggplot(data, aes(month, cum_change, color = compvac )) +
  stat_summary(geom = 'line') +
  theme_minimal()+
  theme_minimal()+
  xlim(14,22)+
  ggtitle("Cumulative_Change_Rate_After-Waves")


Cumulative_Change_Rates <- grid.arrange(Cumulative_Change_Rate_1st_Wave, Cumulative_Change_Rate_2nd_Wave, Cumulative_Change_Rate_3rd_Wave, Cumulative_Change_Rate_After_Waves, ncol = 2)



#-------------------------------------------------------------------------
# 5 Difference in Difference Analysis focus on US's company have vaccine or not under different waves
#-------------------------------------------------------------------------
###DiD for cumulative change for first wave

data$covidwave1 <- as.numeric(data$covidwave1)  #change variable to numeric one


AvgCumChange1<-ddply(data, .(covidwave1, compvacbin), summarise, AvgCumChange1 = mean(cum_change, na.rm =TRUE))

tmp1<- cast(AvgCumChange1,covidwave1 ~ compvacbin, value.var = "AvgCumChange1" )
tmp1<- rbind(tmp1, tmp1[1,] - tmp1[2,])
row.names(tmp1) <- c("After Covid wave","Before 1st Covid wave", "Difference Change Rate")  
colnames(tmp1) <- c("Covid_wave1", " No Vaccine", "Vaccine")  
tmp1[3, "Covid_wave1"] <- NA


###DiD for 2nd wave


data$covidwave2 <- as.numeric(data$covidwave2) #change variable to numeric one

AvgCumChange2<-ddply(data, .(covidwave2, compvacbin), summarise, AvgCumChange2 = mean(cum_change, na.rm =TRUE))

tmp2<- cast(AvgCumChange2,covidwave2 ~ compvacbin, value.var = "AvgCumChange2")
tmp2<- rbind(tmp2, tmp2[1,] - tmp2[2,])
row.names(tmp2) <- c("After Covid wave","Before 2nd  Covid wave", "Difference Change Rate")  
colnames(tmp2) <- c("Covid_wave2", " No Vaccine", "Vaccine")  
tmp2[3, "Covid_wave2"] <- NA



###DiD for 3rd wave
remove(tmp4)

data$covidwave3 <- as.numeric(data$covidwave3)

AvgCumChange3<-ddply(data, .(covidwave3, compvacbin), summarise, AvgCumChange3 = mean(cum_change, na.rm =TRUE))

tmp3<- cast(AvgCumChange3,covidwave3 ~ compvacbin, value.var = "AvgCumChange3")
tmp3<- rbind(tmp3, tmp3[1,] - tmp3[2,])
row.names(tmp3) <- c("After Covid wave","Before 3rd Covid wave", "Difference Change Rate")  
colnames(tmp3) <- c("Covid_wave3", " No Vaccine", "Vaccine")  
tmp3[3, "Covid_wave3"] <- NA

temp <- cbind(tmp1,tmp2,tmp3)
stargazer(temp, summary = FALSE)



#-------------------------------------------------------------------------
# 6 Illustration of change_rate with or without vaccine
#-------------------------------------------------------------------------


#DiD diagram for 1st wave

Did1 <- ggplot(data, aes(month, change_rate, color = compvac )) +
  theme_minimal()+
  ggtitle("First wave of Covid-19 in US") + 
  xlim(2,6) + 
  ylim(0,4) + 
  geom_segment(x=0, y=0.068, xend=3, yend=0.068, colour="red") +
  geom_segment(x=3, y=0.423, xend=6, yend=0.423, colour="red") +
  geom_segment(x=0, y=0.149, xend=3, yend=0.149, colour="blue") +
  geom_segment(x=3, y=3.05, xend=6, yend=3.05, colour="blue") +
  geom_vline(xintercept =  3, linetype = "dashed", color= "red")
#Blue lines are the vaccine producing
#Red lines are the non vaccine producing ones


#DiD diagram for 2nd wave


Did2 <- ggplot(data, aes(month, change_rate, color = compvac )) +
  theme_minimal()+
  ggtitle("Second wave of Covid-19 in US") + 
  xlim(4,9) + 
  ylim(0,4) + 
  geom_segment(x=4, y=0.234, xend=6, yend=0.234, colour="red") +
  geom_segment(x=6, y=0.427, xend=9, yend=0.427, colour="red") +
  geom_segment(x=4, y=0.743, xend=6, yend=0.743, colour="blue") +
  geom_segment(x=6, y=3.367, xend=9, yend=3.367, colour="blue") +
  geom_vline(xintercept =  6, linetype = "dashed", color= "red")
#Blue lines are the vaccine producing
#Red lines are the non vaccine producing ones



#DiD diagram for third wave

Did3 <- ggplot(data, aes(month, change_rate, color = compvac )) +
  theme_minimal()+
  ggtitle("Third wave of Covid-19 in US") + 
  xlim(9,16) + 
  ylim(0,4) + 
  geom_segment(x=9, y=0.293, xend=11, yend=0.293, colour="red") +
  geom_segment(x=11, y=0.456, xend=16, yend=0.456, colour="red") +
  geom_segment(x=9, y=1.505, xend=11, yend=1.505, colour="blue") +
  geom_segment(x=11, y=3.798, xend=16, yend=3.798, colour="blue") +
  geom_vline(xintercept =  11, linetype = "dashed", color= "red")





DiD_Diagrams <- grid.arrange(Did1, Did2, Did3, ncol = 2)

#-------------------------------------------------------------------------
# 7 Difference in Difference Analysis focus on cumulative change rate with vaccine in different phase
#-------------------------------------------------------------------------

avgCumChange1 <- ddply(data, 
                       .(phase_1), summarise,
                       avgChange1 = mean(cum_change ,na.rm=TRUE))


# Make table of the outcomes 
tmp1 <- rbind(avgCumChange1, 
              c(NA, avgCumChange1$avgChange1[2] - avgCumChange1$avgChange1[1]))

avgCumChange2 <- ddply(data, 
                       .(phase_2), summarise,
                       avgChange2 = mean(cum_change ,na.rm=TRUE))


# Make table of the outcomes 
tmp2 <- rbind(avgCumChange2, 
              c(NA, avgCumChange2$avgChange2[2] - avgCumChange2$avgChange2[1]))

avgCumChange3 <- ddply(data, 
                       .(phase_3), summarise,
                       avgChange3 = mean(cum_change ,na.rm=TRUE))


# Make table of the outcomes 
tmp3 <- rbind(avgCumChange3, 
              c(NA, avgCumChange3$avgChange3[2] - avgCumChange3$avgChange3[1]))

rownames(tmp3) <- c("Before", "After", "Difference")
tmp <- cbind(tmp1,tmp2,tmp3)
# Make a table with the results
stargazer(tmp, summary = FALSE, align=TRUE)
