install.packages("plyr")
library(plyr)
install.packages("sandwich")
library(sandwich)
library(lmtest)
#entre = read.csv("C:/Users/dilli/OneDrive/Documents/R/Econ_425_Project/startup_data_2024.csv")
entre = read.csv("C:/Users/dilli.DESKTOP-P60JFM2/Downloads/Data Science/Portfolio/Econometrics project/startup_data_2024.csv")


entre$status = ifelse(entre$status=="closed",0,1)

plot(table(entre$funding_total_usd),xlab = "USD",ylab = "# Firms",main = "firms vs. Total Funding")

entre = entre[-c(365),]

fund = entre$funding_total_usd

Sys.sleep(5)

##################################################

plot(table(entre$funding_total_usd),xlab = "USD",ylab = "# Firms",main = "firms vs. Total Funding")



Sys.sleep(5)

#####################################################

plot(table(entre$relationships),xlab = "relationships", ylab="# of Firms",main = "#firms vs Relationships")
sd(fund, na.rm=TRUE)

stargazer(entre)

L0 = lm(status ~ funding_total_usd, data = entre)


Sys.sleep(5)

####################################################

plot(entre$funding_total_usd, entre$status,xlim=c(0,150000000),main = "Linear Probability Model",xlab = "$USD",ylab="Probability of Success")
abline(L0)

#heterosked.? 

bptest(L0)

#yes

coeftest(L0, vcov = vcovHC(L0, "HC1"))



L1 = lm(status ~ funding_total_usd*relationships+is_CA+is_MA+is_TX,data = entre)

bptest(L1)
#There is a good chance of heterosk.

#let's correct for it

coeftest(L1, vcov = vcovHC(L1, "HC1"))


L2 = lm(status ~ milestones+is_ecommerce+is_web+is_software,data = entre)

bptest(L2)

coeftest(L2, vcov = vcovHC(L2, "HC1"))

#Model 3 is a failure

L3 = lm(status ~ milestones*relationships+is_MA+is_ecommerce,data = entre)

bptest(L3)

coeftest(L3, vcov = vcovHC(L3, "HC1"))



#model 4 seems to consist of the most important variables and the interaction
#term has a very small p value. 
	
