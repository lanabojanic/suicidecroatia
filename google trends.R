#Google search Trends Hrvatska
#02/12/2019
#Lana Bojanic

library(dplyr)
library(tidyr)

#read 
library(readr)
samoub <- read.csv(file = "samoubojstvo.csv", header = TRUE)
samoub_n <- read.csv(file = "samoubojstvo navodnici.csv", header = TRUE)
suicid <- read.csv(file = "suicid.csv", header = TRUE)
suicid_n <- read.csv(file = "suicid navodnici.csv", header = TRUE)
kako <- read.csv(file = "kako se ubiti.csv", header = TRUE)
kako_n <- read.csv(file = "kako se ubiti navodnici.csv", header = TRUE)

#change date format
library(readr)

samoub$Tjedan <- as.Date(parse_date_time(samoub$Tjedan, "dmy"))
samoub_n$Tjedan <- as.Date(parse_date_time(samoub_n$Tjedan, "dmy"))
suicid$Tjedan <- as.Date(parse_date_time(suicid$Tjedan, "dmy"))
suicid_n$Tjedan <- as.Date(parse_date_time(suicid_n$Tjedan, "dmy"))
kako$Tjedan <- as.Date(parse_date_time(kako$Tjedan, "dmy"))
kako_n$Tjedan <- as.Date(parse_date_time(kako_n$Tjedan, "dmy"))

#summarise
library(lubridate)
samoub1 <- samoub %>% group_by(month=floor_date(Tjedan, "month")) %>% 
  summarize(amount=sum(samoubojstvo...Hrvatska.))
samoub1_n <- samoub_n %>% group_by(month=floor_date(Tjedan, "month")) %>% 
  summarize(amount=sum(X.samoubojstvo....Hrvatska.))

suicid1 <- suicid %>% group_by(month=floor_date(Tjedan, "month")) %>% 
  summarize(amount=sum(suicid...Hrvatska.))
suicid1_n <- suicid_n %>% group_by(month=floor_date(Tjedan, "month")) %>% 
  summarize(amount=sum(X.suicid....Hrvatska.))

kako1 <- kako %>% group_by(month=floor_date(Tjedan, "month")) %>% 
  summarize(amount=sum(kako.se.ubiti...Hrvatska.))
kako1_n <- kako_n %>% group_by(month=floor_date(Tjedan, "month")) %>% 
  summarize(amount=sum(X.kako.se.ubiti....Hrvatska.))

#missing values

samoub1[samoub1==0] <- NA
samoub1_n[samoub1_n==0] <- NA
suicid1[suicid1==0] <- NA
suicid1_n[suicid1_n==0] <- NA
kako1[kako1==0] <- NA
kako1_n[kako1_n==0] <- NA

#how many nas
sum(is.na(samoub1))
sum(is.na(samoub1_n))
sum(is.na(suicid1))
sum(is.na(suicid1_n))
sum(is.na(kako1))
sum(is.na(kako1_n))

#there are NAs in suicid, kako se ubiti and "kako se ubiti"

#linear interpolation for kako se ubiti, kako se ubiti navodnici i suicid
m <- lm(amount ~ month, data = kako1)
kako1$pred_value <- predict(m, newdata = kako1)
kako1$interp_value = ifelse(is.na(kako1$amount), kako1$pred_value, kako1$amount)

m2 <- lm(amount ~ month, data = suicid1)
suicid1$pred_value <- predict(m2, newdata = suicid1)
suicid1$interp_value = ifelse(is.na(suicid1$amount), suicid1$pred_value, suicid1$amount)

m3 <- lm(amount ~ amount, data=kako1_n)
kako1_n$pred_value <- predict(m3, newdata=kako1_n)
kako1_n$interp_value <- ifelse(is.na(kako1_n$amount), kako1_n$pred_value, kako1_n$amount)

suicid1 <- suicid1 %>% select(month, interp_value)
kako1 <- kako1 %>% select(month, interp_value)
kako1_n <- kako1_n %>% select(month, interp_value)

#Make into time series

samo <- ts(samoub1$amount, start = c(2014, 1, 1), end = c(2018, 12, 1), frequency = 12)
samo_n <- ts(samoub1_n$amount, start = c(2014, 1, 1), end = c(2018, 12, 1), frequency = 12)
sui <- ts(suicid1$interp_value, start = c(2014, 1, 1), end = c(2018, 12, 1), frequency = 12)
sui_n <- ts(suicid1_n$amount, start = c(2014, 1, 1), end = c(2018, 12, 1), frequency = 12)
kak <- ts(kako1$interp_value, start = c(2014, 1, 1), end = c(2018, 12, 1), frequency = 12)
kak_n <- ts(kako1_n$interp_value, start= c(2014, 1, 1), end = c(2018, 12, 1), frequency = 12)

#read actual data

library(readxl)
suicidi_Croatia <- read_excel("E:/Misc/hrvatski/google hrvatski/data/suicidi-Croatia-mjeseci.xlsx")

sc_real <- ts(suicidi_Croatia$Amount, start = c(2014, 1, 1), end= c(2018, 12, 1), frequency=12)

#outliers
library(forecast)
library(tsoutliers)
#The package detects 5 different types of outliers iteratively in time series data: Additive Outlier (AO), Innovation Outlier (IO),
#Level Shift (LS), Temporary change (TC), Seasonal Level Shift (SLS)

res <- tso(samo)
out_samo <- outliers.effects(res$outliers, length(samo))
#AO outlier May 2015

tso(samo_n)
#no outliers

res1 <- tso(sui)
out_sui <- outliers.effects(res1$outliers, length(sui))
#TC outlier July 2017

res2 <- tso(sui_n)
out_sui_n <- outliers.effects(res2$outliers, length(sui_n))
#AO outliers March 2015, July 2016

res3 <- tso(kak)
out_kak <- outliers.effects(res3$outliers, length(kak))
#AO outlier May 2014 & TC outlier April 2017

tso(kak_n)
#no outliers

#STATIONARITY

library(tseries)

adf.test(samo)
kpss.test(samo)
#stationary

adf.test(samo_n)
kpss.test(samo_n)
#not stationary, difference

adf.test(sui)
kpss.test(sui)
#not stationary, trend

adf.test(sui_n)
kpss.test(sui_n)
#not stationary, trend

adf.test(kak)
kpss.test(kak)
#stationary

adf.test(kak_n)
kpss.test(kak_n)
#stationary

#if adf significant, ts is stationary
#if kpss significant, ts is not stationary
#Case 1: Both tests conclude not stationary -> series is not stationary
#Case 2: Both tests conclude stationary -> series is stationary
#Case 3: KPSS = stationary and ADF = not stationary  -> trend stationary, 
#remove the trend to make series strict stationary
#Case 4: KPSS = not stationary and ADF = stationary -> difference stationary, 
#use differencing to make series stationary

#Modelling, SAMOUBOJSTVO

fit1 <- auto.arima(samo, D=1, xreg = out_samo)
fit1

fit_rv1 <- Arima(samo, order = c(1,0,0), seasonal=list(order=c(0,1,1)), 
                 include.drift=FALSE, xreg = out_samo)
fit_rv1

library(ggplot2)
autoplot(fit_rv1$fitted) + autolayer(samo) + ggtitle("ARIMA(1,0,0)(0,1,1)[12]")

Box.test(fit_rv1$residuals, lag = 20, type="Ljung-Box")
checkresiduals(fit_rv1)
#In general, the Box-Ljung test is defined as: H0:	The model does not exhibit lack of fit.

library(TSA)
fitwhite0 <- fitted(Arima(samo, model=fit_rv1, xreg = out_samo))
fitwhite02 <- fitted(Arima(sc_real, model=fit_rv1, xreg = out_samo))
print(Ccf(fitwhite0, fitwhite02))

#plotting samoubojstvo
tiff("samo.tiff", units="in", width=5, height=4, res=300)

autoplot(fitwhite0, series = "Actual number") + 
  forecast::autolayer(fitwhite02, series="Google search") +
  ggtitle("Samoubojstvo") +
  xlab("Year") + ylab("Number") +
  guides(colour=guide_legend(title="Modelled time-series")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "gray52"),
        legend.position = "top")
dev.off()

#Modelling, SAMOUBOJSTVO NAVODNICI

fit2 <- auto.arima(diff(samo_n), D=1)
fit2

fit_rv2 <- Arima(samo_n, order = c(2,1,0), seasonal=list(order=c(0,1,1)), include.drift=FALSE)
fit_rv2

library(ggplot2)
autoplot(fit_rv2$fitted) + autolayer(samo_n) + ggtitle("ARIMA(2,1,0)(0,1,1)[12]")


Box.test(fit_rv2$residuals, lag = 20, type="Ljung-Box")
checkresiduals(fit_rv2)

library(TSA)

fitwhite <- fitted(Arima(samo_n, model=fit_rv2))
fitwhite2 <- fitted(Arima(sc_real, model=fit_rv2))
print(Ccf(fitwhite, fitwhite2))

#plotting samo_n
tiff("samo_n.tiff", units="in", width=5, height=4, res=300)

autoplot(fitwhite, series = "Actual number") + 
  forecast::autolayer(fitwhite2, series="Google search") +
  ggtitle("Samoubojstvo-quotation marks") +
  xlab("Year") + ylab("Number") +
  guides(colour=guide_legend(title="Modelled time-series")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "gray52"),
        legend.position = "top")
dev.off()

#Modelling, SUICID

fit3 <- auto.arima(sui, D=1, include.mean=TRUE, xreg = out_sui)
fit3

fit_rv3 <- Arima(sui, order = c(0,0,0), seasonal=list(order=c(1,1,0)), 
                 include.drift=FALSE, include.mean = TRUE, xreg = out_sui)
fit_rv3

library(ggplot2)
autoplot(fit_rv3$fitted) + autolayer(sui) + ggtitle("ARIMA(0,0,0)(1,1,0)[12]")


Box.test(fit_rv3$residuals, lag = 20, type="Ljung-Box")
checkresiduals(fit_rv3)

fitwhite3 <- fitted(Arima(sui, model=fit_rv3, xreg = out_sui))
fitwhite32 <- fitted(Arima(sc_real, model=fit_rv3, xreg = out_sui))
print(Ccf(fitwhite3, fitwhite32))

#plotting sui
tiff("sui.tiff", units="in", width=5, height=4, res=300)

autoplot(fitwhite3, series = "Actual number") + 
  forecast::autolayer(fitwhite32, series="Google search") +
  ggtitle("Suicid") +
  xlab("Year") + ylab("Number") +
  guides(colour=guide_legend(title="Modelled time-series")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "gray52"),
        legend.position = "top")
dev.off()

#Modelling, SUICID NAVODNICI

fit3i <- auto.arima(sui_n, D=1, include.mean=TRUE, xreg = out_sui_n)
fit3i

fit_rv3i <- Arima(sui_n, order = c(0,0,0), seasonal=list(order=c(0,1,1)), include.drift=FALSE,
                 include.mean = TRUE, xreg = out_sui_n)
fit_rv3i

library(ggplot2)
autoplot(fit_rv3i$fitted) + autolayer(sui_n) + ggtitle("ARIMA(0,0,0)(0,1,1)[12]")

Box.test(fit_rv3i$residuals, lag = 20, type="Ljung-Box")
checkresiduals(fit_rv3i)

fitwhite4 <- fitted(Arima(sui_n, model=fit_rv3i, xreg = out_sui_n))
fitwhite42 <- fitted(Arima(sc_real, model=fit_rv3i, xreg = out_sui_n))
print(Ccf(fitwhite4, fitwhite42))

#plotting sui_n
tiff("sui_n.tiff", units="in", width=5, height=4, res=300)

autoplot(fitwhite4, series = "Actual number") + 
  forecast::autolayer(fitwhite42, series="Google search") +
  ggtitle("Suicid-quotation marks") +
  xlab("Year") + ylab("Number") +
  guides(colour=guide_legend(title="Modelled time-series")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "gray52"),
        legend.position = "top")
dev.off()

#Modelling, KAKO SE UBITI

fit4 <- auto.arima(kak, D=1, xreg = out_kak)
fit4

fit_rv4 <- Arima(kak, order = c(0,0,2), seasonal=list(order=c(1,1,0)), include.drift=FALSE,
                 xreg= out_kak)
fit_rv4

library(ggplot2)
autoplot(fit_rv4$fitted) + autolayer(kak) + ggtitle("ARIMA(0,0,2)(1,1,0)[12]")

Box.test(fit_rv4$residuals, lag = 20, type="Ljung-Box")
checkresiduals(fit_rv4)
#In general, the Box-Ljung test is defined as: H0:	The model does not exhibit lack of fit.

fitwhite5 <- fitted(Arima(kak, model=fit_rv4, xreg=out_kak))
fitwhite52 <- fitted(Arima(sc_real, model=fit_rv4, xreg = out_kak))
print(Ccf(fitwhite5, fitwhite52))

#plot and export kak
tiff("kak.tiff", units="in", width=5, height=4, res=300)

autoplot(fitwhite5, series = "Actual number") + 
  forecast::autolayer(fitwhite52, series="Google search") +
  ggtitle("Kako se ubiti") +
  xlab("Year") + ylab("Number") +
  guides(colour=guide_legend(title="Modelled time-series")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "gray52"),
        legend.position = "top")
dev.off()

#Modelling, KAKO SE UBITI NAVODNICI

fit5 <- auto.arima(kak_n, D=1)
fit5

fit_rv5 <- Arima(kak_n, order = c(0,0,1), seasonal=list(order=c(0,1,1)), include.drift=FALSE)
fit_rv5

library(ggplot2)
autoplot(fit_rv5$fitted) + autolayer(kak_n) + ggtitle("ARIMA(0,0,1)(0,1,1)[12]")

Box.test(fit_rv5$residuals, lag = 20, type="Ljung-Box")
checkresiduals(fit_rv5)

fitwhite6 <- fitted(Arima(kak_n, model=fit_rv5))
fitwhite62 <- fitted(Arima(sc_real, model=fit_rv5))
print(Ccf(fitwhite6, fitwhite62))

#plot and export kak_n
tiff("kak_n.tiff", units="in", width=5, height=4, res=300)

autoplot(fitted(Arima(kak_n, model=fit_rv5)), series = "Actual number") + 
  forecast::autolayer(fitted(Arima(sc_real, model=fit_rv5)), series="Google search") +
  ggtitle("Kako se ubiti-quotation marks") +
  xlab("Year") + ylab("Number") +
  guides(colour=guide_legend(title="Modelled time-series")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "gray52"),
          legend.position = "top")
dev.off()

#B O N U S
#model fit Cro time series

tso(sc_real)
#no outliers

adf.test(sc_real)
kpss.test(sc_real)
#stationary

fit_cro <- auto.arima(sc_real, D=1)
fit_rv_cro <- Arima(sc_real, order = c(0,0,0), seasonal=list(order=c(0,1,1)), include.drift=FALSE)
fit_rv_cro

library(ggplot2)
autoplot(fit_rv_cro$fitted) + autolayer(sc_real) + ggtitle("ARIMA(0,0,0)(0,1,1)[12]")

Box.test(fit_rv_cro$residuals, lag = 20, type="Ljung-Box")
checkresiduals(fit_rv_cro)
