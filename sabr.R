
ink <- c()
strike <- c()
put_price <- c()

strike[1] <- 137.50
strike[2] <- 140.00 
strike[3] <- 142.50 
strike[4] <- 145.00 
strike[5] <- 147.50 
strike[6] <- 150.00 
strike[7] <- 152.50 

put_price[1] <- 2.69
put_price[2] <- 3.80
put_price[3] <- 5.20
put_price[4] <- 6.80
put_price[5] <- 8.65
put_price[6] <- 10.60
put_price[7] <- 12.80
for(i in 1 : 7)
{
ink[i] <- (putImpliedVol(141.15, strike[i], 21, .017, put_price[i]))
}

#plot(strike, ink)
f <- list(
  family = "Courier New, monospace",
  size = 18,
  color = "#7f7f7f"
)
x <- list(
  title = "Strike",
  titlefont = f
)
y <- list(
  title = "Volatility",
  titlefont = f
)
tit <- list(
  title = "/KC Volatility Smile",
  titlefont = f 
)
smile <- data.frame(ink, strike)
p <- ggplot(smile, aes(x = strike, y = ink)) +
  geom_line(colour="red", linetype="dashed", size=1.5) +
  xlab("Strike") + ylab("Implied Volatility") + ggtitle("/KC Volatility Smile (AUG 12 16 Puts)")

price  <- seq(120, 160, 1)

KC_delta <-c()
KC_calldelta <-c()
for(i in 1: length(price))
{
  KC_delta[i]  <- putDeltaBS(price[i], 142.50, 21, .017, .275)
  KC_calldelta[i] <- callDeltaBS(price[i], 142.50, 21, .017, .275)
}

KCd <- data.frame(price, KC_delta)
KCdel <- ggplot(KCd, aes(x = price, y = KC_delta))  +
  geom_line(colour="red", linetype="F1", size=1.5) + 
  xlab("Price") + ylab("Delta") + ggtitle("Delta of /KC ATM Put")

#print(KCdel)

spy <- data_download
ex <-c()
srt <- c()
price <- c(
)
for(i in 1: 3228)
{
  if(spy$call.put[i] == "P")
  {
  ex[i] <- as.numeric(as.Date(spy$expiration[i], format="%m/%d/%Y") - as.Date(spy$date[i], format="%m/%d/%Y"))
  srt[i] <- as.numeric(spy$strike[i])
  price[i] <- as.numeric((spy$ask[i] + spy$bid[i]) /2 )
  }
}

ex <- ex[!is.na(ex)]
srt<- srt[!is.na(srt)]
price  <- price[!is.na(price)]
longiv <- c()

BS <-
  function(S,K,T,r,sig,type = "C"){
    d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig *sqrt(T))
    d2 <- d1 -sig * sqrt(T)
    
    if(type=="C")
    {
      value <- S*pnorm(d1) - K *exp(-r*T)* pnorm(d2)
    }
    
    if(type=="P")
    {
      value <- K*exp(-r*T)*pnorm(-d2)-S*pnorm(-d1)
    }
    return(value)
  }
implied.vol <-
  function(S, K, T, r, market, type){
    sig <- 0.20
    sig.up <- 1
    sig.down <- 0.001
    count <-  0
    err <- BS(S, K, T, r, sig, type) - market 
  }


# for(i in 1: length(price))
# {
#   s <- srt[i]
#   e <- ex[i]/252
#   ppp <- price[i]
#   longiv[i] <- implied.vol(211.57, s, e, .017, ppp, 'P')
#   gc()
#   print(i)
# }
# 
# for(i in 1 :length(longiv))
# {
#   if( longiv[i] < 0 )
#   {
#     longiv[i] <- longiv[i] * -1
#   }
# }
# s <- interp(srt, ex, longiv)
# persp(s$x, s$y, s$z, xlab = "Strike", ylab = "DTE", zlab = "Implied Volatility", main="SPY Volatility Surface ", sub = "Uses puts on 6/01/15 with close price of $211.57", nticks = 10, ticktype ="detailed")


