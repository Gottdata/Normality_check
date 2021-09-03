#defining individual data sets

INT<-unlist(R_Data[,2])
LARGE<-unlist(R_Data[,3])
MIDM<-unlist(R_Data[,4])
PRIV<-unlist(R_Data[,5])
MASS<-unlist(R_Data[,6])
SME<-unlist(R_Data[,7])
EURIBOR <-unlist(R_Data[,8])  

#Install libraries

install.packages("ggpubr")
install.packages("ggplot2")
library(ggplot2)
library(ggpubr)
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")

# NORMALITY - Density and QQ Plot
# INT 

INT.ols <- lm (INT~EURIBOR, data = R_Data)
R_Data$resi <- INT.ols$residuals
ggdensity(R_Data$resi, fill = 'lightgray') + ggtitle("INT")
ggqqplot(R_Data$resi) + ggtitle("INT")

# LARGE 

LARGE.ols <- lm (LARGE~EURIBOR, data = R_Data)
R_Data$resi <- LARGE.ols$residuals
ggdensity(R_Data$resi, fill = 'lightgray') + ggtitle("LARGE")
ggqqplot(R_Data$resi) + ggtitle("LARGE")

# MIDM 

MIDM.ols <- lm (MIDM~EURIBOR, data = R_Data)
R_Data$resi <- MIDM.ols$residuals
ggdensity(R_Data$resi, fill = 'lightgray') + ggtitle("MIDM")
ggqqplot(R_Data$resi) + ggtitle("MIDM")

# PRIV 

PRIV.ols <- lm (PRIV~EURIBOR, data = R_Data)
R_Data$resi <- PRIV.ols$residuals
ggdensity(R_Data$resi, fill = 'lightgray') + ggtitle("PRIV")
ggqqplot(R_Data$resi) + ggtitle("PRIV")

# MASS 

MASS.ols <- lm (MASS~EURIBOR, data = R_Data)
R_Data$resi <- MASS.ols$residuals
ggdensity(R_Data$resi, fill = 'lightgray') + ggtitle("MASS")
ggqqplot(R_Data$resi) + ggtitle("MASS")

# SME 

SME.ols <- lm (SME~EURIBOR, data = R_Data)
R_Data$resi <- SME.ols$residuals
ggdensity(R_Data$resi, fill = 'lightgray') + ggtitle("SME")
ggqqplot(R_Data$resi) + ggtitle("SME")

# NORMALITY Shapiro Test

install.packages("dplyr")
library("dplyr")

# INT 

INT.ols <- lm (INT~EURIBOR, data = R_Data)
R_Data$resi <- INT.ols$residuals
shapiro.test(R_Data$resi)

# LARGE 

LARGE.ols <- lm (LARGE~EURIBOR, data = R_Data)
R_Data$resi <- LARGE.ols$residuals
shapiro.test(R_Data$resi)

# MIDM 

MIDM.ols <- lm (MIDM~EURIBOR, data = R_Data)
R_Data$resi <- MIDM.ols$residuals
shapiro.test(R_Data$resi)

# PRIV 

PRIV.ols <- lm (PRIV~EURIBOR, data = R_Data)
R_Data$resi <- PRIV.ols$residuals
shapiro.test(R_Data$resi)

# MASS 

MASS.ols <- lm (MASS~EURIBOR, data = R_Data)
R_Data$resi <- MASS.ols$residuals
shapiro.test(R_Data$resi)

# SME 

SME.ols <- lm (SME~EURIBOR, data = R_Data)
R_Data$resi <- SME.ols$residuals
shapiro.test(R_Data$resi)
