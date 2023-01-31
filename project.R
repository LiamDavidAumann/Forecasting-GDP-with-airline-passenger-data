library(tstools)
library(vars)
data <- read.csv("projectdata.csv")
gdpfull <- read.csv("GDP.csv")
#This is the GDP file from FRED from Q1 1947 to Q3 2022


#Times Series varaibles
GDPts <- ts(data$GDP, start=c(2002,4),frequency=4)
DOMts <- ts(data$DOMESTIC, start=c(2002,4),frequency=4)
INTts <- ts(data$INTERNATIONAL, start=c(2002,4),frequency=4)

gdpfts <- ts(gdpfull$GDP, start = c(1947,1),frequency = 4)

#Plots
plot(GDPts, main="United States GDP Over Time (billions of USD)",
     sub="Source: FRED (2022)",
     ylab="", xlab="",
     las=1)

plot(DOMts, main="Domestic Airline Passengers (in millions)",
     sub="Source: Bureau of Transportation Statistics (2022)",
     ylab="", xlab="",
     yaxt="n",
     las=1)
axis(2, at = c(50000000,100000000,150000000,200000000),
     labels = c(50,100,150,200), las=1)

plot(INTts, main="International Arrivals in the USA (in millions)",
     sub="Source: Bureau of Transportation Statistics (2022)",
     ylab="", xlab="",
     yaxt="n",
     las=1)
axis(2, at = c(0,20000000,40000000,60000000),
     labels = c(0,20,40,60), las=1)

#Unit Root Testing

#GDP:
#Random Walk with drift
u_fit_gdp1 <- tsreg(GDPts, lags(GDPts,1))
summary(u_fit_gdp1)
#Crit value = -2.89
(1.01528-1)/.01112
#1.3741 > -2.98
#IS UNIT ROOT

#Pure random walk
u_fit_gdp2 <- tsreg(GDPts, lags(GDPts,1),intercept=FALSE)
summary(u_fit_gdp2)
#Crit value = -1.95
(1.011017-1)/.002204
#5>-1.95 UNIT ROOT EXISTS

#Random Walk With Drift amd trend
u_gdp_trend <- make.trend(GDPts)
u_fit_gdp3 <- tsreg(GDPts, lags(GDPts, 1) %~% u_gdp_trend)
summary(u_fit_gdp3)
(.96362-1)/.06161
#-.6 > -3.45 UNIT ROOT EXISTS

#DOMESTIC:
#Random walk with drift
u_fit_dom1 <- tsreg(DOMts, lags(DOMts,1))
summary(u_fit_dom1)
(.695-1)/.0835
#Does not exhibit UNIT root behavior
#Is expected as it is clearly a seasonal variable

#INTERNATIONAL:
#Random walk with drift
u_fit_int1 <- tsreg(INTts, lags(INTts,1))
summary(u_fit_int1)
(.8281-1)/.06344

#Pure
u_fit_int2 <- tsreg(INTts, lags(INTts,1),intercept=FALSE)
summary(u_fit_int2)
(.99155-1)/.01898

#drift and trend int
u_int_trend <- make.trend(INTts)
u_fit_int3 <- tsreg(INTts, lags(INTts,1) %~% u_int_trend)
summary(u_fit_int3)
(.8205-1)/.06554

#Seeing if covid effected it
int_window <- window(INTts, start = c(2002,4), end = c(2020,1))

#Pure random walk
u_fit_intw1 <- tsreg(int_window, lags(int_window,1),intercept = FALSE)
summary(u_fit_intw1)
(.99764-1)/.01364

#walk with drift
u_fit_intw2 <- tsreg(int_window, lags(int_window,1))
summary(u_fit_intw2)
(.8424-1)/.06153

#walk and trend
trend_intw <- make.trend(int_window)
u_fit_intw3 <- tsreg(int_window, lags(int_window,1) %~% trend_intw)
summary(u_fit_intw3)
(.2907-1)/.1301

#DOM
dumdom <- quarter.dummy(DOMts, omit=1)
fitseasdom <- tsreg(DOMts, dumdom)
rhsdom <- ts.combine(make.trend(DOMts), dumdom)
#Detrended and Deseasonalized
fitdesdom <- tsreg(DOMts, rhsdom)
dom.dt <- fitdesdom$resids

#INT
dumint <- quarter.dummy(INTts, 1)
fitseasint <- tsreg(INTts, dumint)
rhsint <- ts.combine(make.trend(INTts), dumint)
fitdesint <- tsreg(INTts, rhsint)
#Detrended and Deseasonalized
int.dt <- fitdesint$resids

#Deseasonalized plots:
plot(dom.dt, main="Detrended/Deseasonalized Domestic (in millions)",
     ylab="", xlab="",
     yaxt="n",
     las=1,
     lwd=1.5)
axis(2, at = c(-70000000,-35000000,0,35000000),
     labels = c(-70,-50,0,35), las=1)


plot(int.dt, main="Detrended/Deseasonalized Inter. (in millions)",
     ylab="", xlab="",
     yaxt="n",
     las=1,
     lwd=1.5)
axis(2, at = c(-30000000,-15000000,0,15000000),
     labels = c(-30,-15,0,15), las=1)

#LAG SELECTION
lag.finder.gdpf <- function(p) {
        fit <- tsreg(gdpfts, lags(gdpfts, 1:p))
        return(AIC(fit))
}
lapply(1:5, lag.finder.gdpf)

#USED IN PAPER
armamain3 <- arima(gdpfts, order = c(5,0,5), method = "ML")
armamain3$coef
predict(armamain3, n.ahead=12)$pred

forecastplot <- predict(armamain3, n.ahead=12)$pred
plot(forecastplot, main="12-Step GDP Forecast",
     ylab="", xlab="",
     xaxt="n",
     las=1,
     lwd=1.2)
axis(2, las=1)
axis(1, at = c(2023.0, 2024.0,2025.0), labels = c(2023,2024,2025))


#VAR:
#1 lag
varforecast <- ts.combine(GDPts, dom.dt, int.dt)
varfit1 <- VAR(varforecast, p=1)

gdpvar1 <- var.fcst(varfit1, "GDPts", 1:12)
gdpvar1

domvar1 <- var.fcst(varfit1, "dom.dt", 1:12)
domvar1

intvar1 <- var.fcst(varfit1, "int.dt", 1:12)
intvar1

#5 lag
varforecast5 <- ts.combine(GDPts, dom.dt, int.dt)
varfit5 <- VAR(varforecast, p=5)

gdpvar5 <- var.fcst(varfit5, "GDPts", 1:12)
gdpvar5

domvar5 <- var.fcst(varfit5, "dom.dt", 1:12)
domvar5

intvar5 <- var.fcst(varfit5, "int.dt", 1:12)
intvar5

#SCENARIO FORECASTS
rhsscen <- lags(GDPts %~% dom.dt, 1)
scen.gdp <- tsreg(GDPts, rhsscen)
scen.dom <- tsreg(dom.dt/1000000000, rhsscen)
scen.gdp
scen.dom

dom.dt
plot(dom.dt)

last(GDPts)
last(dom.dt)

#Conditional that dom.dt drops by 8% compared to last obs
# dom.dt = 23708704.06

#First step:
-.0102 + 1.017*(25248.48) + (-0.000005439*(25770330.5))
# = 25537.53

#Second step:
-.0102 + 1.017*(25537.53) + (-0.000005439*(23708704.06))
# = 25842.71

#Third step:
-.0102 + 1.017*(25842.71) + (-0.000005439*(23708704.06))
# = 26153.07

#Conditional that dom.dt will be = to its last observation
#dom.dt = 25770330.5

#step 1: = 25537.53

#step 2:
-.0102 + 1.017*(25537.53) + (-0.000005439*(25770330.5))
# = 25831.49

#step 3:
-.0102 + 1.017*(25831.49) + (-0.000005439*(25770330.5))
# = 26130.45

#Conditional that dom.dt increases 8%: 27831956.94

#Step 1: 25537.53

#Step 2: 
-.0102 + 1.017*(25537.53) + (-0.000005439*(27831956.94))
# = 25820.28

#Step 3:
-.0102 + 1.017*(25820.28) + (-0.000005439*(27831956.94))
# = 26107.84

#Scenario Plot
fsct8i <- ts(c(last(GDPts), 25537.53, 25820.28, 26107.84), start=c(2022,2), frequency=4)

fsct8d <- ts(c(last(GDPts), 25537.53, 25842.71, 26153.07), start=c(2022,2), frequency=4)

fsct0 <- ts(c(last(GDPts), 25537.53, 25831.49, 26130.45), start=c(2022,2), frequency=4)

scenplot <- ts(c(GDPts, fsct8i, fsct8d,fsct0), start = c(2021,1),frequency=4)
plot(scenplot)

combined <- ts.union(GDPts, fsct8i, fsct8d,fsct0)

plot(combined, lty=c(1,2,3,4),plot.type="single", 
     main="Scenario Analysis Plot (%)",
     ylab="", xlab="",
     xaxt="n",
     las=1,
     lwd=1.2)
axis(2, las=1)
axis(1, at = c(2022.25,2022.5,2022.75), 
     labels = c("2022 Q1","2022 Q2", "2022 Q3"))

