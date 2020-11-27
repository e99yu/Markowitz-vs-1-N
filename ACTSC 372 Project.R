#######################
# Name: Ewen Yu
# Student ID: 20765939
# ACTSC 372 COURSE PROJECT
########################



# DATA COLLECTION
library(quantmod)
# Selection of 10 stocks as listed below
dataset <- c('AAPL',   # Apple Inc
             'AMZN',   # Amazon.com, Inc.
             'CSCO',   # Cisco Systems, Inc.
             'FB',     # Facebook, Inc. Common Stock
             'GOOG',   # Alphabet Inc Class C
             'IBM',    # IBM Common Stock
             'INTC',   # Intel Corporation
             'MSFT',   # Microsoft Corporation
             'NFLX',   # Netflix Inc
             'TWTR')   # Twitter Inc

start_date <- '2018-10-31'

end_date <- '2020-10-31'

getSymbols(Symbols = dataset, src = "yahoo",from =start_date, to = end_date)


# ESTIMATION

# adjusted daily returns of the 10 stocks
returns <- data.frame (dailyReturn(Ad(AAPL)) , dailyReturn(Ad(AMZN)) ,
                       dailyReturn(Ad(CSCO)) , dailyReturn(Ad(FB)) ,
                       dailyReturn(Ad(GOOG)) , dailyReturn(Ad(IBM)) ,
                       dailyReturn(Ad(INTC)) , dailyReturn(Ad(MSFT)),
                       dailyReturn(Ad(NFLX)) , dailyReturn(Ad(TWTR)))

colnames(returns)<- dataset # dataframe of returns

# the training set was selected to be from 2018-10-31 to 2019-10-31

tr.returns <- returns [1:252, 1:10] # adjusted daily returns of the training set
t.returns <- returns [253:504, 1:10] # adjusted daily returns of the test set

cov.ret <- cov(tr.returns) # variance-covariance matrix of adjusted daily returns of training set
incov.matrix <- solve(cov.ret) # inverse of variance-covariance matrix


# expected returns for stocks in training set
mu.training <- c(mean(tr.returns [,1]) , mean(tr.returns [,2]), mean(tr.returns [,3]),
                 mean(tr.returns [,4]), mean(tr.returns [,5]), mean(tr.returns [,6]),
                 mean(tr.returns [,7]), mean(tr.returns [,8]), mean(tr.returns [,9]),
                 mean(tr.returns [,10]))


# CONSTRUCTION OF PORTFOLIOS

e <- c(rep(1, 10))


wm <- (incov.matrix %*% e) %*% (solve(t(e) %*% incov.matrix %*% e)) # min-risk portfolio

d<- drop((t(e) %*% incov.matrix %*% mu.training) %*%  (solve(t(e) %*% incov.matrix %*% e)))
wz <- ((incov.matrix %*% mu.training) - (d* (incov.matrix %*% e))) #zero-covariance portfolio

tau.grid <- c(0,	0.01,	0.02,	0.03,	0.04,	0.05,	0.1,	0.2,	0.3,	0.4,	0.5) # selected tau values

# optimal portfolios with different values of tau
w_opt1 <- (tau.grid[1]*wz) +wm  # tau=0
w_opt2 <- (tau.grid[2]*wz) +wm # tau=0.01
w_opt3 <- (tau.grid[3]*wz) +wm # tau=0.02
w_opt4 <- (tau.grid[4]*wz) +wm # tau=0.03
w_opt5 <- (tau.grid[5]*wz) +wm # tau=0.04
w_opt6 <- (tau.grid[6]*wz) +wm # tau=0.05
w_opt7 <- (tau.grid[7]*wz) +wm # tau= 0.1
w_opt8 <- (tau.grid[8]*wz) +wm # tau=0.2
w_opt9 <- (tau.grid[9]*wz) +wm # tau=0.3
w_opt10 <- (tau.grid[10]*wz) +wm # tau=0.4
w_opt11 <- (tau.grid[11]*wz) +wm # tau=0.5

# IN-SAMPLE PERFORMANCE EVALUATION

mu_opt <- c()
sigma_opt <- c()

for (i in 1:11) #calculation of portfolio mean and std. dev for different values of tau
{
  mu_opt [i] <- t(mu.training) %*% (wm + (tau.grid[i]*wz))
  sigma_opt [i] <- sqrt (t((wm + (tau.grid[i]*wz))) %*% cov.ret %*% ((wm + (tau.grid[i]*wz))))
}
plot(sigma_opt,mu_opt, xlim = c(0.008,0.03),ylim = c(0.0004,0.003), 
     type = "l", main= "Efficient Frontier",col = "green", lwd=2 ) # graph of efficient frontier

mu_M <- t(mu.training) %*% wm 
sigma_M <- sqrt (t(wm)%*%cov.ret%*%wm)
points(sigma_M,mu_M) # minimum risk portfolio


SPY <- c('SPY')  # SPDR S&P 500 ETF TRUST 
getSymbols(Symbols = SPY, src = "yahoo",from =start_date, to = end_date)
tr.SPY <- SPY[1:252,]
t.SPY <- SPY[253:504,]

mean.SPY <- mean(dailyReturn(tr.SPY)) # mean return of S&P500 on training set duration
sd.SPY <- sd(dailyReturn(tr.SPY))     # standard deviations of S&P500 on training set duration

points(sd.SPY,mean.SPY, col="red") # highlight of S&P500 (training set) on (sigma,mu)-plane

N.vector <- c(rep(0.1, 10)) # weighting of 1/N strategy
N.mean.return <- N.vector %*% mu.training # mean return of 1/N strategy
N.sd <- sqrt(t(N.vector) %*% cov.ret %*% N.vector) # standard deviations of 1/N strategy

points(N.sd, N.mean.return, col="blue") # highlight of 1/N strategy on (sigma,mu)-plane

#################################
#### (calculating risk-free rates)
data <- new.env()

# set dates
date.start <- "2018-10-31"
date.end <- "2020-10-31"

# set tickers
tickers <- c("DGS10")

library("quantmod")
getSymbols( tickers
            , src = "FRED" 
            , from = date.start 
            , to = date.end  
            , env = data
            , adjust = TRUE
)

# subset data to within time range
dtx <- data$DGS10
dtr<-dtx[paste("2018-10-31","2019-10-31",sep="/")]
dt<-dtx[paste("2019-10-31","2020-10-31",sep="/")]

sharpe.rf.tr <-mean(dtr,na.rm = TRUE) # risk-free rate for Sharpe ratio calculation on training set
sharpe.rf.t <-mean(dt,na.rm = TRUE)   # risk-free for Sharpe ratio calculation on test set

#####

# adjusted daily returns to S&P500 for training set
SPY.ret <- dailyReturn(Ad(AAPL)) [1:252] 
Sharpe.SPY <- (mean.SPY- (sharpe.rf.tr/100))%*% (solve(sd.SPY)) #Sharpe Ratio for training set S&P500

# daily returns on optimal portfolios for different values of tau for training set
dr.opt1 <- data.matrix(tr.returns) %*% w_opt1
dr.opt2 <- data.matrix(tr.returns) %*% w_opt2
dr.opt3 <- data.matrix(tr.returns) %*% w_opt3
dr.opt4 <- data.matrix(tr.returns) %*% w_opt4
dr.opt5 <- data.matrix(tr.returns) %*% w_opt5
dr.opt6 <- data.matrix(tr.returns) %*% w_opt6
dr.opt7 <- data.matrix(tr.returns)%*% w_opt7
dr.opt8 <- data.matrix(tr.returns) %*% w_opt8
dr.opt9 <- data.matrix(tr.returns) %*% w_opt9
dr.opt10 <- data.matrix(tr.returns) %*% w_opt10
dr.opt <- data.frame(dr.opt1,dr.opt2,dr.opt3,dr.opt4,dr.opt5,dr.opt6,dr.opt7,
                     dr.opt8,dr.opt9,dr.opt10)

# mean returns for daily returns of  optimal portfolios under different values of tau for training set
mean.dr.opt <- data.frame(mean(dr.opt1),mean(dr.opt2), mean(dr.opt3),mean(dr.opt4),
                          mean(dr.opt5), mean(dr.opt6), mean(dr.opt7),mean(dr.opt8),
                          mean(dr.opt9), mean(dr.opt10)) 

# sd for daily returns of optimal portfolios under different values of tau for training set
sd.dr.opt <- data.frame(sd(dr.opt1),sd(dr.opt2), sd(dr.opt3),sd(dr.opt4),
                        sd(dr.opt5), sd(dr.opt6), sd(dr.opt7),sd(dr.opt8),
                        sd(dr.opt9), sd(dr.opt10)) 

# Sharpe ratio for daily returns of optimal portfolios under different values of tau for training set
Sharpe.dr.opt <- (mean.dr.opt - (sharpe.rf.tr/100))/ (sd.dr.opt)

# daily returns on 1/N strategy portfolio for training set
N.daily.ret <-data.matrix(tr.returns) %*% N.vector
mean.N.daily.ret <- mean(N.daily.ret) # mean return on 1/N portfolio
sd.N.daily.ret <- sd(N.daily.ret) # standard deviations for 1/N portfolio

# Sharpe ratio for daily returns on 1/N strategy portfolio for training set
Sharpe.N.daily.ret <- (mean.N.daily.ret - (sharpe.rf.tr/100))%*% (solve(sd.N.daily.ret))


# OUT-SAMPLE PERFORMANCE EVALUATION


t.mean.SPY <- mean(dailyReturn(t.SPY)) # mean return of S&P500 on test set duration
t.sd.SPY <- sd(dailyReturn(t.SPY))     # standard deviations of S&P500 on test set duration

# adjusted daily returns to S&P500 for test set
t.SPY.ret <- dailyReturn(Ad(AAPL)) [253:504] 
t.Sharpe.SPY <- (t.mean.SPY- (sharpe.rf.t/100))%*% (solve(t.sd.SPY)) #Sharpe Ratio for test set S&P500

# daily returns on optimal portfolios for different values of tau for test set
t.dr.opt1 <- data.matrix(t.returns) %*% w_opt1
t.dr.opt2 <- data.matrix(t.returns) %*% w_opt2
t.dr.opt3 <- data.matrix(t.returns) %*% w_opt3
t.dr.opt4 <- data.matrix(t.returns) %*% w_opt4
t.dr.opt5 <- data.matrix(t.returns) %*% w_opt5
t.dr.opt6 <- data.matrix(t.returns) %*% w_opt6
t.dr.opt7 <- data.matrix(t.returns) %*% w_opt7
t.dr.opt8 <- data.matrix(t.returns) %*% w_opt8
t.dr.opt9 <- data.matrix(t.returns) %*% w_opt9
t.dr.opt10 <- data.matrix(t.returns) %*% w_opt10
t.dr.opt <- data.frame(t.dr.opt1, t.dr.opt2, t.dr.opt3, t.dr.opt4, t.dr.opt5, t.dr.opt6, t.dr.opt7,
                       t.dr.opt8, t.dr.opt9, t.dr.opt10)

# mean returns for daily returns of  optimal portfolios under different values of tau for test set
t.mean.dr.opt <- data.frame(mean(t.dr.opt1),mean(t.dr.opt2), mean(t.dr.opt3),mean(t.dr.opt4),
                            mean(t.dr.opt5), mean(t.dr.opt6), mean(t.dr.opt7),mean(t.dr.opt8),
                            mean(t.dr.opt9), mean(t.dr.opt10)) 

# sd for daily returns of optimal portfolios under different values of tau for test set
t.sd.dr.opt <- data.frame(sd(t.dr.opt1),sd(t.dr.opt2), sd(t.dr.opt3),sd(t.dr.opt4),
                          sd(t.dr.opt5), sd(t.dr.opt6), sd(t.dr.opt7),sd(t.dr.opt8),
                          sd(t.dr.opt9), sd(t.dr.opt10)) 

# Sharpe ratio for daily returns of optimal portfolios under different values of tau for test set
t.Sharpe.dr.opt <- (t.mean.dr.opt - (sharpe.rf.t/100))/(t.sd.dr.opt)

# daily returns on 1/N strategy portfolio for test set
t.N.daily.ret <-data.matrix(t.returns) %*% N.vector
t.mean.N.daily.ret <- mean(t.N.daily.ret) # mean return on 1/N portfolio
t.sd.N.daily.ret <- sd(t.N.daily.ret) # standard deviations for 1/N portfolio

# Sharpe ratio for daily returns on 1/N strategy portfolio for test set
t.Sharpe.N.daily.ret <- (t.mean.N.daily.ret - (sharpe.rf.t/100))%*% (solve(t.sd.N.daily.ret))




