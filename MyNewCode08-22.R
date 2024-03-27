#To start off we can clear all the variables from the current environment and close all the plots.

rm(list = ls())
graphics.off()

#We will need to make use of the vars and tsm packages
library(tsm)
library(vars)
library(mFilter)
library (readr)


#To load the data and store the time series objects
data<-read_csv("https://raw.githubusercontent.com/Asad6414/Code_v2/main/data08-22.csv", show_col_types = FALSE)


################### LEVELS #######################
gdp <- ts(log(data$gdp), start= c(2008, 1), freq = 4) # Correct way to construct the data
real_gdp <- ts(log(data$real_gdp), start= c(2008, 1), freq = 4)
cpi <- ts(log(data$cpi), start= c(2008, 1), freq = 4)
core_cpi <- ts(log(data$core_cpi), start= c(2008, 1), freq = 4)
prate <- ts(data$fedfunds, start= c(2008, 1), freq = 4) # Not necessesary to take log
empration <- ts(data$emratio, start= c(2008, 1), freq = 4) # Not necessesary to take log
wti_oil <- ts(log(data$wti_oil), start= c(2008, 1), freq = 4) # Not necessesary to take log
gscpi <- ts(data$gscpi, start= c(2008, 1), freq = 4) # A NEW VARIABLE DATA - Not necessesary to take log
divisia <- ts(log(data$divisia_m3), start= c(2008, 1), freq = 4) 
gov_total <- ts(log(data$gov_total), start= c(2008, 1), freq = 4)
gov_current <- ts(log(data$gov_current), start= c(2008, 1), freq = 4)
total_par <- ts(log(data$total_par), start= c(2008, 1), freq = 4)
total_market <- ts(log(data$total_market), start= c(2008, 1), freq = 4) 
private_par <- ts(log(data$private_par), start= c(2008, 1), freq = 4)
private_market <- ts(log(data$private_market), start= c(2008, 1), freq = 4)

#Plot the data
plot(cbind(gdp, cpi, prate, empration, wti_oil,gscpi, divisia, gov_total, total_market))


##################################### 1. GDP ########################################
# To graph the log of the variables 
plot(gdp)

#To consider the degree of persistence in the data we make use of the autocorrelation function.
gdp.acf <- ac(gdp, main = "GDP")

#To check for a unit root
adf.gdp <- ur.df(gdp,lags = 12, type = "trend",  selectlags = "AIC")
summary(adf.gdp)

##################################### 2. REAL GDP ########################################
# To graph the log of the variables 
plot(real_gdp)

#To consider the degree of persistence in the data we make use of the autocorrelation function.
real_gdp.acf <- ac(real_gdp, main = "REAL GDP")

#To check for a unit root
adf.real_gdp <- ur.df(real_gdp,lags = 12, type = "trend",  selectlags = "AIC")
summary(adf.real_gdp)

##################################### 3. CPI ########################################
# To graph the log of the variables 
plot(cpi)

#To consider the degree of persistence in the data we make use of the autocorrelation function.
cpi.acf <- ac(cpi, main = "CPI")

#To check for a unit root
adf.cpi <- ur.df(cpi,lags = 12, type = "trend",  selectlags = "AIC")
summary(adf.cpi)


##################################### 4. CORE CPI ########################################
# To graph the log of the variables 
plot(core_cpi)

#To consider the degree of persistence in the data we make use of the autocorrelation function.
core_cpi.acf <- ac(core_cpi, main = "CORE CPI")

#To check for a unit root
adf.core_cpi <- ur.df(core_cpi, type = "trend", selectlags = "AIC")
summary(adf.core_cpi)


##################################### 5. POLICY RATE ########################################
# To graph the log of the variables 
plot(prate)

#To consider the degree of persistence in the data we make use of the autocorrelation function.
prate.acf <- ac(prate, main = "POLICY RATE")

#To check for a unit root
adf.prate <- ur.df(prate, type = "trend", selectlags = "AIC")
summary(adf.prate)

##################################### 6. EM RATIO ########################################
# To graph the log of the variables 
plot(empration)

#To consider the degree of persistence in the data we make use of the autocorrelation function.
empration.acf <- ac(empration, main = "EM RATIO")

#To check for a unit root
adf.empration <- ur.df(empration, type = "trend", selectlags = "AIC")
summary(adf.empration)


##################################### 7. WTI OIL ########################################
# To graph the log of the variables 
plot(wti_oil)

#To consider the degree of persistence in the data we make use of the autocorrelation function.
wti_oil.acf <- ac(wti_oil, main = "WTI OIL")

#To check for a unit root
adf.wti_oil <- ur.df(wti_oil, type = "trend", selectlags = "AIC")
summary(adf.wti_oil)

##################################### 8. GSCPI ########################################
# To graph the log of the variables 
plot(gscpi)

#To consider the degree of persistence in the data we make use of the autocorrelation function.
gscpi.acf <- ac(gscpi, main = "GSCPI")

#To check for a unit root
adf.gscpi <- ur.df(gscpi, type = "trend", selectlags = "AIC")
summary(adf.gscpi)


##################################### 9. DIVISIA ########################################
# To graph the log of the variables 
plot(divisia)

#To consider the degree of persistence in the data we make use of the autocorrelation function.
divisia.acf <- ac(divisia, main = "DIVISIA")

#To check for a unit root
adf.divisia <- ur.df(divisia, type = "trend", selectlags = "AIC")
summary(adf.divisia)

##################################### 10. GOV TOTAL ########################################
# To graph the log of the variables 
plot(gov_total)

#To consider the degree of persistence in the data we make use of the autocorrelation function.
gov_total.acf <- ac(gov_total, main = "GOV TOTAL")

#To check for a unit root
adf.gov_total <- ur.df(gov_total, type = "trend", selectlags = "AIC")
summary(adf.gov_total)

##################################### 11. GOV CURRENT ########################################
# To graph the log of the variables 
plot(gov_current)

#To consider the degree of persistence in the data we make use of the autocorrelation function.
gov_current.acf <- ac(gov_current, main = "GOV CURRENT")

#To check for a unit root
adf.gov_current <- ur.df(gov_current, type = "trend", selectlags = "AIC")
summary(adf.gov_current)

##################################### 12. TORAL PAR ########################################
# To graph the log of the variables 
plot(total_par)

#To consider the degree of persistence in the data we make use of the autocorrelation function.
total_par.acf <- ac(total_par, main = "TORAL PAR")

#To check for a unit root
adf.total_par <- ur.df(total_par, type = "trend", selectlags = "AIC")
summary(adf.total_par)

##################################### 13. TORAL MARKET ########################################
# To graph the log of the variables 
plot(total_market)

#To consider the degree of persistence in the data we make use of the autocorrelation function.
total_market.acf <- ac(total_market, main = "TORAL MARKET")

#To check for a unit root
adf.total_market <- ur.df(total_market, type = "trend", selectlags = "AIC")
summary(adf.total_market)

##################################### 14. PRIVATE PAR ########################################
# To graph the log of the variables 
plot(private_par)

#To consider the degree of persistence in the data we make use of the autocorrelation function.
private_par.acf <- ac(private_par, main = "PRIVATE PAR")

#To check for a unit root
adf.private_par <- ur.df(private_par, type = "trend", selectlags = "AIC")
summary(adf.private_par)

##################################### 15. PRIVATE MARKET ########################################
# To graph the log of the variables 
plot(private_market)

#To consider the degree of persistence in the data we make use of the autocorrelation function.
private_market.acf <- ac(private_market, main = "PRIVATE MARKET")


#To check for a unit root
adf.private_market <- ur.df(private_market, type = "trend", selectlags = "AIC")
summary(adf.private_market)


# MODEL SELECTION AND ESTIMATION - ######## LEVELS ########
#Addition of the t and t2 as per suggestion
t<-1:60
t2 = t^2

data.bv <- cbind(gdp, cpi, prate, empration, wti_oil,gscpi, divisia, gov_total, total_market)
data.bv

colnames(data.bv) <- c("gdp", "cpi", "prate", "empration", "wti_oil", "gscpi", "divisia", "gov_total", "total_market")

info.bv <- VARselect(data.bv, lag.max = 12, type = "both")
info.bv$selection

bv.est <- VAR(data.bv, p = 6, type = "both", season = NULL, exog = NULL)
summary(bv.est)

#To test for serial correlation we can apply a Portmanteau-test
bv.serial <- serial.test(bv.est, lags.pt = 12, type = "PT.asymptotic")
bv.serial

#Modified version: This version that includes t1 and t2 and excludes "private_market" variable.
data.pz <- cbind(gdp, cpi, prate,empration, wti_oil, divisia, gov_total,total_market)
data.pz

colnames(data.pz) <- c("gdp", "cpi", "prate", "empration", "wti_oil", "divisia", "gov_total", "total_market")

info.pz <- VARselect(data.pz, lag.max = 12, type = "const", exogen = cbind(t, t2))
info.pz$selection

pz.est <- VAR(data.pz, p = 4, type = "const", season = NULL, exog = cbind(t, t2))
summary(pz.est)

#To test for serial correlation we can apply a Portmanteau-test - ES (Empirical Saddlepoint Approximation)
pz.serial <- serial.test(pz.est, lags.pt = 12, type = "ES")
pz.serial


plot(bv.serial, names = "gdp")

plot(bv.serial, names = "cpi")

plot(bv.serial, names = "prate")

plot(bv.serial, names = "empration")

plot(bv.serial, names = "wti_oil")

plot(bv.serial, names = "divisia")

plot(bv.serial, names = "gov_total")

plot(bv.serial, names = "total_market")

plot(bv.serial, names = "private_market")

#Modified version:
plot(pz.serial, names = "gdp")

plot(pz.serial, names = "cpi")

plot(pz.serial, names = "prate")

plot(pz.serial, names = "empration")

plot(pz.serial, names = "wti_oil")

plot(pz.serial, names = "divisia")

plot(pz.serial, names = "gov_total")

plot(pz.serial, names = "total_market")

plot(pz.serial, names = "private_market")


#To test for heteroscedasticity in the residuals
bv.arch <- arch.test(bv.est, lags.multi = 12, multivariate.only = TRUE)
bv.arch

#To consider the distribution of the residuals, we could apply a normality test
bv.norm <- normality.test(bv.est, multivariate.only = TRUE)
bv.norm

#To test for the structural break in the residuals we can apply a CUSUM test.
bv.cusum <- stability(bv.est, type = "OLS-CUSUM")
plot(bv.cusum)

#Modified version:To test for heteroscedasticity in the residuals
pz.arch <- arch.test(pz.est, lags.multi = 12, multivariate.only = TRUE)
pz.arch

#Modified version:To consider the distribution of the residuals, we could apply a normality test
pz.norm <- normality.test(pz.est, multivariate.only = TRUE)
pz.norm

#Modified version:To test for the structural break in the residuals we can apply a CUSUM test.
pz.cusum <- stability(pz.est, type = "OLS-CUSUM")
plot(pz.cusum)

######################################## Transform Data - Difference ###########################################
#To start off we can clear all the variables from the current environment and close all the plots.
rm(list = ls())
graphics.off()

#To load the data and store the time series objects
data<-read_csv("https://raw.githubusercontent.com/Asad6414/Code_v2/main/data_tr08-22.csv", show_col_types = FALSE)
# data<-read_csv("https://raw.githubusercontent.com/Asad6414/Code_v2/main/data08-22.csv", show_col_types = FALSE)

#This allows us to load the data and store the time series objects as gdp and une.
# gdp_tr <- ts(diff(log(data$gdp)), start= c(1967, 1), freq = 4) # Correct way to construct the data
# real_gdp_tr <- ts(diff(log(data$real_gdp)), start= c(1967, 1), freq = 4)
# cpi_tr <- ts(diff(log(data$cpi)), start= c(1967, 1), freq = 4)
# core_cpi_tr <- ts(diff(log(data$core_cpi)), start= c(1967, 1), freq = 4)
# prate_tr <- ts(data$fedfunds, start= c(1967, 1), freq = 4) # Not necessesary to take to take log and diff
# empration_tr <- ts(data$emratio, start= c(1967, 1), freq = 4) # Not necessesary to take log and diff
# wti_oil_tr <- ts(diff(log(data$wti_oil)), start= c(1967, 1), freq = 4)
# divisia_tr <- ts(diff(log(data$divisia_m3)), start= c(1967, 1), freq = 4)
# gscpi <- ts(data$gscpi, start= c(1998, 1), freq = 4) # Not necessesary to take log and diff
# gov_total_tr <- ts(diff(log(data$gov_total)), start= c(1967, 1), freq = 4)
# gov_current_tr <- ts(diff(log(data$gov_current)), start= c(1967, 1), freq = 4)
# total_par_tr <- ts(diff(log(data$total_par)), start= c(1967, 1), freq = 4)
# total_market_tr <- ts(diff(log(data$total_market)), start= c(1967, 1), freq = 4) 
# private_par_tr <- ts(diff(log(data$private_par)), start= c(1967, 1), freq = 4)
# private_market_tr <- ts(diff(log(data$private_market)), start= c(1967, 1), freq = 4)

gdp_tr <- ts(data$gdp, start= c(2008, 1), freq = 4) # Correct way to construct the data
real_gdp_tr <- ts(data$real_gdp, start= c(2008, 1), freq = 4) #already transformed in the csv data
cpi_tr <- ts(data$cpi, start= c(2008, 1), freq = 4) #already transformed in the csv data
core_cpi_tr <- ts(data$core_cpi, start= c(2008, 1), freq = 4) #already transformed in the csv data
prate_tr <- ts(data$fedfunds, start= c(2008, 1), freq = 4) # Not necessesary to take to take log and diff
empration_tr <- ts(data$emratio, start= c(2008, 1), freq = 4) # Not necessesary to take log and diff
wti_oil_tr <- ts(data$wti_oil, start= c(2008, 1), freq = 4) #already transformed in the csv data
gscpi <- ts(data$gscpi, start= c(2008, 1), freq = 4) # A NEW VARIABLE DATA - Not necessesary to take log
divisia_tr <- ts(data$divisia_m3, start= c(2008, 1), freq = 4) #already transformed in the csv data
gov_total_tr <- ts(data$gov_total, start= c(2008, 1), freq = 4) #already transformed in the csv data
gov_current_tr <- ts(data$gov_current, start= c(2008, 1), freq = 4) #already transformed in the csv data
total_par_tr <- ts(data$total_par, start= c(2008, 1), freq = 4) #already transformed in the csv data
total_market_tr <- ts(data$total_market, start= c(2008, 1), freq = 4) #already transformed in the csv data
private_par_tr <- ts(data$private_par, start= c(2008, 1), freq = 4) #already transformed in the csv data
private_market_tr <- ts(data$private_market, start= c(2008, 1), freq = 4) #already transformed in the csv data

#Plot the data
plot(cbind(gdp_tr, cpi_tr, prate_tr, empration_tr, wti_oil_tr, divisia_tr, gov_total_tr, total_market_tr))


##################################### 1. GDP TR ########################################
# To graph the log of the variables 
plot(gdp_tr)

#To consider the degree of persistence in the data we make use of the autocorrelation function.
gdp_tr.acf <- ac(gdp_tr, main = "GDP TR")

#To check for a unit root
adf.gdp_tr <- ur.df(gdp_tr,lags = 12, type = "trend",  selectlags = "AIC")
summary(adf.gdp_tr)

##################################### 2. REAL GDP TR ########################################
# To graph the log of the variables 
plot(real_gdp_tr)

#To consider the degree of persistence in the data we make use of the autocorrelation function.
real_gdp_tr.acf <- ac(real_gdp_tr, main = "REAL GDP TR")

#To check for a unit root
adf.real_gdp_tr <- ur.df(real_gdp_tr,lags = 12, type = "trend",  selectlags = "AIC")
summary(adf.real_gdp_tr)

##################################### 3. CPI TR ########################################
# To graph the log of the variables 
plot(cpi_tr)

#To consider the degree of persistence in the data we make use of the autocorrelation function.
cpi_tr.acf <- ac(cpi_tr, main = "CPI TR")

#To check for a unit root
adf.cpi_tr <- ur.df(cpi_tr,lags = 12, type = "trend",  selectlags = "AIC")
summary(adf.cpi_tr)


##################################### 4. CORE CPI ########################################
# To graph the log of the variables 
plot(core_cpi_tr)

#To consider the degree of persistence in the data we make use of the autocorrelation function.
core_cpi_tr.acf <- ac(core_cpi_tr, main = "CORE CPI TR")

#To check for a unit root
adf.core_cpi_tr <- ur.df(core_cpi_tr, type = "trend", selectlags = "AIC")
summary(adf.core_cpi_tr)


##################################### 5. POLICY RATE TR ########################################
# To graph the log of the variables 
plot(prate_tr)

#To consider the degree of persistence in the data we make use of the autocorrelation function.
prate_tr.acf <- ac(prate_tr, main = "POLICY RATE TR")

#To check for a unit root
adf.prate_tr <- ur.df(prate_tr, type = "trend", selectlags = "AIC")
summary(adf.prate_tr)

##################################### 6. EM RATIO TR ########################################
# To graph the log of the variables 
plot(empration_tr)

#To consider the degree of persistence in the data we make use of the autocorrelation function.
empration_tr.acf <- ac(empration_tr, main = "EM RATIO TR")

#To check for a unit root
adf.empration_tr <- ur.df(empration_tr, type = "trend", selectlags = "AIC")
summary(adf.empration_tr)


##################################### 7. WTI OIL TR ########################################
# To graph the log of the variables 
plot(wti_oil_tr)

#To consider the degree of persistence in the data we make use of the autocorrelation function.
wti_oil_tr.acf <- ac(wti_oil_tr, main = "WTI OIL TR")

#To check for a unit root
adf.wti_oil_tr <- ur.df(wti_oil_tr, type = "trend", selectlags = "AIC")
summary(adf.wti_oil_tr)


##################################### 8. DIVISIA TR ########################################
# To graph the log of the variables 
plot(divisia_tr)

#To consider the degree of persistence in the data we make use of the autocorrelation function.
divisia_tr.acf <- ac(divisia_tr, main = "DIVISIA TR")

#To check for a unit root
adf.divisia_tr <- ur.df(divisia_tr, type = "trend", selectlags = "AIC")
summary(adf.divisia_tr)

##################################### 9. GOV TOTAL TR ########################################
# To graph the log of the variables 
plot(gov_total_tr)

#To consider the degree of persistence in the data we make use of the autocorrelation function.
gov_total_tr.acf <- ac(gov_total_tr, main = "GOV TOTAL TR")

#To check for a unit root
adf.gov_total_tr <- ur.df(gov_total_tr, type = "trend", selectlags = "AIC")
summary(adf.gov_total_tr)

##################################### 10. GOV CURRENT TR ########################################
# To graph the log of the variables 
plot(gov_current_tr)

#To consider the degree of persistence in the data we make use of the autocorrelation function.
gov_current_tr.acf <- ac(gov_current_tr, main = "GOV CURRENT TR")

#To check for a unit root
adf.gov_current_tr <- ur.df(gov_current_tr, type = "trend", selectlags = "AIC")
summary(adf.gov_current_tr)

##################################### 11. TORAL PAR TR ########################################
# To graph the log of the variables 
plot(total_par_tr)

#To consider the degree of persistence in the data we make use of the autocorrelation function.
total_par_tr.acf <- ac(total_par_tr, main = "TORAL PAR TR")

#To check for a unit root
adf.total_par_tr <- ur.df(total_par_tr, type = "trend", selectlags = "AIC")
summary(adf.total_par_tr)

##################################### 12. TORAL MARKET TR ########################################
# To graph the log of the variables 
plot(total_market_tr)

#To consider the degree of persistence in the data we make use of the autocorrelation function.
total_market_tr.acf <- ac(total_market_tr, main = "TORAL MARKET TR")

#To check for a unit root
adf.total_market_tr <- ur.df(total_market_tr, type = "trend", selectlags = "AIC")
summary(adf.total_market_tr)

##################################### 13. PRIVATE PAR TR ########################################
# To graph the log of the variables 
plot(private_par_tr)

#To consider the degree of persistence in the data we make use of the autocorrelation function.
private_par_tr.acf <- ac(private_par_tr, main = "PRIVATE PAR TR")

#To check for a unit root
adf.private_par_tr <- ur.df(private_par_tr, type = "trend", selectlags = "AIC")
summary(adf.private_par_tr)

##################################### 14. PRIVATE MARKET TR ########################################
# To graph the log of the variables 
plot(private_market_tr)

#To consider the degree of persistence in the data we make use of the autocorrelation function.
private_market_tr.acf <- ac(private_market_tr, main = "PRIVATE MARKET TR")

#To check for a unit root
adf.private_market_tr <- ur.df(private_market_tr, type = "trend", selectlags = "AIC")
summary(adf.private_market_tr)



# MODEL SELECTION AND ESTIMATION - ######## DIFFERENCE ########
data_tr.bv <- cbind(gdp_tr, cpi_tr, prate_tr, empration_tr, wti_oil_tr, divisia_tr, gov_total_tr, total_market_tr)
data_tr.bv

colnames(data_tr.bv) <- c("gdp tr", "cpi tr", "prate tr", "empration tr", "wti_oil tr", "divisia tr", "gov_total tr", "total_market tr")


info_tr.bv <- VARselect(data_tr.bv, lag.max = 12, type = "both")
info_tr.bv$selection

bv_tr.est <- VAR(data_tr.bv, p = 6, type = "both", season = NULL, exog = NULL)
summary(bv_tr.est)

#To test for serial correlation we can apply a Portmanteau-test
bv_tr.serial <- serial.test(bv_tr.est, lags.pt = 12, type = "PT.asymptotic")
bv_tr.serial

#Modified version: This version that includes t1 and t2 and excludes "private_market" variable.

#Addition of the t and t2 as per suggestion
t<-1:60
t2 = t^2

data_tr.pz <- cbind(gdp_tr, cpi_tr, prate_tr, empration_tr, wti_oil_tr, divisia_tr, gov_total_tr, total_market_tr)
data_tr.pz

colnames(data_tr.pz) <- c("gdp tr", "cpi tr", "prate tr", "empration tr", "wti_oil tr", "divisia tr", "gov_total tr", "total_market tr")

info_tr.pz <- VARselect(data_tr.pz, lag.max = 8, type = "const", exogen = cbind(t, t2))
info_tr.pz$selection

pz_tr.est <- VAR(data_tr.pz, p = 6, type = "const", season = NULL, exog = cbind(t, t2))
summary(pz_tr.est)

#To test for serial correlation we can apply a Portmanteau-test - ES (Empirical Saddlepoint Approximation)
pz_tr.serial <- serial.test(pz_tr.est, lags.pt = 12, type = "ES")
pz_tr.serial

plot(bv_tr.serial, names = "gdp tr")

plot(bv_tr.serial, names = "cpi tr")

plot(bv_tr.serial, names = "prate tr")

plot(bv_tr.serial, names = "empration tr")

plot(bv_tr.serial, names = "wti_oil tr")

plot(bv_tr.serial, names = "divisia tr")

plot(bv_tr.serial, names = "gov_total tr")

plot(bv_tr.serial, names = "total_market tr")

plot(bv_tr.serial, names = "private_market tr")


#Modified version:
plot(pz_tr.serial, names = "gdp tr")

plot(pz_tr.serial, names = "cpi tr")

plot(pz_tr.serial, names = "prate tr")

plot(pz_tr.serial, names = "empration tr")

plot(pz_tr.serial, names = "wti_oil tr")

plot(pz_tr.serial, names = "divisia tr")

plot(pz_tr.serial, names = "gov_total tr")

plot(pz_tr.serial, names = "total_market tr")

plot(pz_tr.serial, names = "private_market tr")


#To test for heteroscedasticity in the residuals
bv_tr.arch <- arch.test(bv_tr.est, lags.multi = 12, multivariate.only = TRUE)
bv_tr.arch

#To consider the distribution of the residuals, we could apply a normality test
bv_tr.norm <- normality.test(bv_tr.est, multivariate.only = TRUE)
bv_tr.norm

#To test for the structural break in the residuals we can apply a CUSUM test.
bv_tr.cusum <- stability(bv_tr.est, type = "OLS-CUSUM")
plot(bv_tr.cusum)

#Modified version:To test for heteroscedasticity in the residuals
pz_tr.arch <- arch.test(pz_tr.est, lags.multi = 12, multivariate.only = TRUE)
pz_tr.arch

#Modified version:To consider the distribution of the residuals, we could apply a normality test
pz_tr.norm <- normality.test(pz_tr.est, multivariate.only = TRUE)
pz_tr.norm

#Modified version:To test for the structural break in the residuals we can apply a CUSUM test.
pz_tr.cusum <- stability(pz_tr.est, type = "OLS-CUSUM")
plot(pz_tr.cusum)


