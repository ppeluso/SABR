require(ggplot2)
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
  xlab("Strike") + ylab("Implied Volatility") + ggtitle("/KC Volatility Smile")
print(p)