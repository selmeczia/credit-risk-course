########################################################################
# ALMDemo script
#
# script file of the Asset and Liability Management chapter
#
# Authors: Daniel Havran and Istvan Margitai
# 2014 October
#
########################################################################




#initializing script
########################################################################
#Import datafile
portfolio <- read.csv("week1/portfolio.csv")
market <- read.csv("week1/market.csv")

#format date values
portfolio$issue    <-as.Date(portfolio$issue,    format = "%m/%d/%Y")
portfolio$maturity <-as.Date(portfolio$maturity, format = "%m/%d/%Y")
market$date        <-as.Date(market$date,        format = "%m/%d/%Y")
#########################################################################


#check data
#########################################################################
head(portfolio)
levels(portfolio$account_name)
head(market)
#########################################################################


#loading source file
#########################################################################
source.filename<-"week1/bankALM.R"
source(source.filename)
#########################################################################


#check basic functions
#########################################################################
NOW<-as.Date("09/30/2014",    format = "%m/%d/%Y")

cf(0.10,3,100,type="BULLET")
test.date<-seq(from= as.Date("09/30/2015", format= "%m/%d/%Y"),
               to= as.Date("09/30/2035", format= "%m/%d/%Y"), by="1 year")               
get.yieldcurve.spot(market, test.date, type="EUR01", now=NOW, showplot=TRUE)

test.reprice.date<-test.date[seq(from=1, to=20, by=2)]
test.forward<-get.yieldcurve.forward(market, test.reprice.date, 
                                     type="EUR01", now=NOW)
test.floating<-get.floating(market, test.date, test.reprice.date, 
                                       type="EUR01", now=NOW, showplot=TRUE)
#plot(test.forward, type="l", col="green")
#lines(test.floating, type="s", col="red")


rm(test.floating, test.forward, test.date, test.reprice.date)
#########################################################################


#cash-flow generation
#########################################################################

#get cashflows
cashflow.table<-do.call(rbind, lapply(1:NROW(portfolio), 
                        function(i) cf.table(portfolio, market, now = NOW, id = i)))

head(cashflow.table)
#write.csv(cashflow.table,file="~/Dropbox/ALM_R/R_code/cashflowtable.csv")
#########################################################################


#get present values
#########################################################################
presentvalue.table <- do.call(rbind, lapply(1:NROW(portfolio), 
          function (i) pv.table(cashflow.table[cashflow.table$id==portfolio$id[i],], market, now=NOW)))

head(presentvalue.table)
sum(presentvalue.table$presentvalue)
#write.csv(presentvalue.table,file="~/Dropbox/ALM_R/R_code/presentvaluetable.csv")
#########################################################################


#some basic transformation
#########################################################################
#cash-flow for a given date
head(aggregate(. ~ date, FUN=sum, 
               data=subset(cashflow.table,select=-c(id, account))))

#Market Value of Equity
pvt1<-aggregate(. ~ account, FUN=sum, 
                data=subset(presentvalue.table,select=-c(id,date)))
head(pvt1)
sum(pvt1[,2])
#########################################################################


#IRR measurement
#########################################################################
#get re-pricing table
(repgap<-repricing.gap.table(portfolio, now=NOW))
#
barplot(repgap, col="gray", xlab="Months", ylab="EUR")
title(main="Repricing gap table", cex=0.8, 
      sub =paste("Actual date: ",as.character(as.Date(NOW))))


#net interest income
nii     <- nii.table(cashflow.table, now=NOW)

#plot
plot.new()
par.backup<-par()
par(oma = c(1, 1, 1, 6), new=TRUE)

barplot(nii, density=5*(1:(NROW(nii)-1)), xlab="Maturity", cex.names=0.8, ylab="EUR", 
        cex.axis=0.8, args.legend = list(x = "right"))

title(main="Net interest income table", cex=0.8, 
      sub=paste("Actual date: ",as.character(as.Date(NOW))) )
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0),mar = c(0, 0, 0, 0),  new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right", legend = row.names(nii[1:(NROW(nii)-1),]), 
       density=5*(1:(NROW(nii)-1)), bty = "n", cex=1)
par(par.backup)

#########################################################################


#liquidity measurement
#########################################################################
#get liquidity tables
lq      <- lq.table(cashflow.table, now=NOW)
#

#plot
plot.new()
par.backup<-par()
par(oma = c(1, 1, 1, 6), new=TRUE)

lq.bar<-barplot(lq[1:(NROW(lq)-1),], density=5*(1:(NROW(lq)-1)),
                xlab="Maturity", cex.names=0.8,
                ylab="EUR", cex.axis=0.8,
                args.legend = list(x = "right")) 
title(main="Liquidity gap table", cex=0.8, 
      sub=paste("Actual date: ",as.character(as.Date(NOW))))
lines(x=lq.bar,y=lq[NROW(lq),],lwd=4, col="darkgray", lty=5, type="b", pch=0 )
lines(x=lq.bar,y=cumsum(lq[NROW(lq),]),lwd=4, col="black", type="b" )

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0),mar = c(0, 0, 0, 0),  new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right", legend = row.names(lq[1:(NROW(lq)-1),]), 
       density=5*(1:(NROW(lq)-1)), bty = "n", cex=1)
par(par.backup)

#########################################################################


#non-maturing deposits
#########################################################################
#Data import
nmd <- read.csv("ecb_nmd_data.csv")
nmd$date <- as.Date(nmd$date, format = "%m/%d/%Y")

#Plotting deposit coupon and 1 month Euribor
library(car)
plot(nmd$eur1m ~ nmd$date, type="l", xlab="Time", ylab="Interest rate")
lines(nmd$cpn~ nmd$date, type="l", lty=2)
title(main="Deposit coupon vs 1-month Euribor", cex=0.8 )
legend("topright", legend = c("Coupon","EUR 1M"), 
        bty = "n", cex=1,lty=c(2,1))

library(urca)
attach(nmd)

#Unit root test (ADF)
cpn.ur <- ur.df(cpn, type="none", lags=2)
dcpn.ur <- ur.df(diff(cpn), type="none", lags=1)
eur1m.ur <- ur.df(eur1m, type="none", lags=2)
deur1m.ur <- ur.df(diff(eur1m), type="none", lags=1)
sumtbl <- matrix(cbind(cpn.ur@teststat, cpn.ur@cval,
                       dcpn.ur@teststat, dcpn.ur@cval,
                       eur1m.ur@teststat, eur1m.ur@cval,
                       deur1m.ur@teststat, deur1m.ur@cval), nrow=4)
colnames(sumtbl) <- c("cpn", "diff(cpn)", "eur1m", "diff(eur1m)")
rownames(sumtbl) <- c("Test stat", "1pct CV", "5pct CV", "10pct CV")

#Stationarty test (KPSS)
cpn.kpss <- ur.kpss(cpn, type="mu")
eur1m.kpss <- ur.kpss(eur1m, type="mu")

sumtbl <- matrix(cbind( cpn.kpss@teststat, cpn.kpss@cval,
                        eur1m.kpss@teststat, eur1m.kpss@cval), nrow=5)
colnames(sumtbl) <- c("cpn", "eur1m")
rownames(sumtbl) <- c("Test stat", "10pct CV", "5pct CV", "2.5pct CV", "1pct CV")

print(cpn.ur@test.name)
print(sumtbl)
print(cpn.kpss@test.name)
print(sumtbl)

#Residual test of cointegrating equation
lr <- lm(cpn ~ eur1m)
res <- resid(lr)
lr$coefficients
res.ur <- ur.df(res, type="none", lags=1)
summary(res.ur)

library(dynlm)
res <- resid(lr)[2:length(cpn)]
dy <- diff(cpn)
dx <- diff(eur1m)
detach(nmd)
ecmdata <- c(dy, dx, res)
ecm <- dynlm(dy ~ L(dx, 1) + L(res, 1), data = ecmdata)
summary(ecm)

#ECB yield curve data import
ecb.yc <- read.csv("ecb_yc_data.csv")
ecb.yc$date <- as.Date(ecb.yc$date, format = "%d/%m/%Y")

matplot(ecb.yc$date, ecb.yc[,2:6], type="l", lty=(1:5), lwd=2, 
        col=1, xlab="Time", ylab="Yield", ylim=c(0,6), xaxt="n")
legend("topright", cex=0.8, bty = "n", lty=c(1:5), lwd=2,
       legend = colnames(ecb.yc[,2:6]))       
title(main="ECB yield curve", cex=0.8 )
axis.Date(1,ecb.yc$date)

#Solving linear optimization problem with constraints
library(quadprog)
b <- nmd$cpn[21:135]
A <- cbind(ecb.yc$EUR1M, ecb.yc$EUR3M, ecb.yc$EUR1Y, ecb.yc$EUR5Y, ecb.yc$EUR10Y)
m <- c(1, 3, 12, 60, 120)
l <- 60
stat.opt <- solve.QP( t(A) %*% A, t(b) %*% A, 
                      cbind( matrix(1, nr = 5, nc = 1),
                             matrix(m, nr = 5, nc = 1),
                             diag(5)),
                      c(1, l, 0,0,0,0,0),
                      meq=2 )
sumtbl <- matrix(round(stat.opt$solution*100, digits = 1), nr=1)
colnames(sumtbl) <- c("1M", "3M", "1Y", "5Y", "10Y")
cat("Portfolio weights in %")
print(sumtbl)

mrg <- nmd$cpn[21:135] - stat.opt$solution[2]*ecb.yc$EUR3M + stat.opt$solution[5]*ecb.yc$EUR10Y
plot(mrg ~ ecb.yc$date, type = "l", col="black", xlab="Time", ylab="%")
title(main="Margin of static replication", cex=0.8 )

#########################################################################

