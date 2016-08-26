#SABR

This is a small project that I am working on. Currently I am using C++ and R where I am trying to code up all of the different ways to price derivatives. 

##**Plain Vanilla European Option**
The plain vanilla european option is the first derivative I worked on. The two methods that I used to price this derivative is the Black-Scholes Formula and the Monte Carlo Method. I was then able to use the Black-Scholes Formula to calculate the current implied volatility of an option. 

Below is a plot of the volatility smile on /KC AUG 12 16 puts when the price of the underlying was trading at 141.15
![](https://github.com/ppeluso/SABR/blob/master/VolSmileKC.png)

I was then able to also calculate the greeks of a given option by using the Black-Scholes equation. 
![](https://github.com/ppeluso/SABR/blob/master/KC_delta_Put.png)
This is a graph of how the delta of an ATM /KC put option changes as the underlying price changes. 


I was also able to use these functions to create a Volatility surface. 
![](https://github.com/ppeluso/SABR/blob/master/SPYsurface.png)



