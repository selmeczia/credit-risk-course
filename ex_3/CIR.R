library(data.table)
library(sde)

####################################

# config
sig <- 0.1
k <- 0.75
theta <-0.05
lamdbanull <-0.1
d <- expression(k*(theta-x))
s <- expression(sig*sqrt(x)) 

lambda_CIR <- sde.sim(X0=lamdbanull,drift=d, sigma=s, M=10,N=10)

matplot(lambda_CIR, main="CIR-process estimates", type="l", xlab="Time",ylab="Lambda")

hist(lambda_CIR)

current <-matrix(lamdbanull)
current_all <- matrix()
for (i in 1:10) {
  current <- cbind(current, lambda_CIR[i+10]*exp(-lambda_CIR[i+10]*i))
}

cum_current <- cumsum(current[1:10])
matplot(cum_current, type="l",
        main="Default probability from t = 0",
        xlab="Time",
        ylab="Probability of default")


default_dt <- data.table(rev(cum_current), 
           c(rev(cum_current)[2:10],rep(0,1)),
           c(rev(cum_current)[3:10],rep(0,2)),
           c(rev(cum_current)[4:10],rep(0,3)),
           c(rev(cum_current)[5:10],rep(0,4)),
           c(rev(cum_current)[6:10],rep(0,5)),
           c(rev(cum_current)[7:10],rep(0,6)),
           c(rev(cum_current)[8:10],rep(0,7)),
           c(rev(cum_current)[9:10],rep(0,8)),
           c(rev(cum_current)[10:10],rep(0,9)))

matplot(default_dt[seq(dim(default_dt)[1],1),], type="l",
        main="Probability of default from different years",xlab="Time",ylab="Probability of default")





