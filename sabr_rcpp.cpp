#include <Rcpp.h>
#include <iostream>
#include <algorithm> 
#include <cmath>
#include <vector> 
#define PI 3.14159265359

using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//







double norm_cdf(const double x); 
double norm_pdf(const double x); 
double d_1(const double S, const double K, const double T, const double r, const double sigma);
double d_2(const double S, const double K, const double T, const double r, const double sigma);
double callPriceBS(const double S, const double K, const double T, const double r, const double sigma);
double putPriceBS(const double S, const double K, const double T, const double r, const double sigma);
double callImpliedVol(const double S, const double K, const double T, const double r, const double market);
double putImpliedVol(const double S, const double K, const double T, const double r, const double market);
int main()
{


  return 0; 
  
}

// [[Rcpp::export]]
double norm_cdf(const double x)
{
  double k = 1.0/(1.0 + 0.2316419 * x);
  double k_sum = k * (0.319381530 + k * (-0.356563782 + k * (1.781477937 + k * (-1.821255978 + 1.330274429 * k))));
  
  if( x >= 0.0)
  {
    return(1.0 - (1.0/(pow(2 * PI, 0.5))) * exp(-0.5 * x * x) * k_sum);
  }
  
  else 
  {
    return(1.0 - norm_cdf(-x));
  }
  
}
// [[Rcpp::export]]
double norm_pdf(const double x)
{
  
  return (1/ sqrt(2 * PI)) * exp((-0.5)* (x * x)); 
  
}
// [[Rcpp::export]]
double d_1(const double S, const double K, const double T, const double r, const double sigma)
{
  double t = T/ (double)252.0 ;
  return (log(S/K) + (r + ((sigma * sigma )/ 2)) * t )/ (double)(sigma * sqrt(t)); 
}
// [[Rcpp::export]]
double d_2(const double S, const double K, const double T, const double r, const double sigma)
{
  double t = T/ (double)252.0 ;
  return(d_1(S,K,T,r,sigma) - (sigma * sqrt(t)));
  
}
// [[Rcpp::export]]
double callPriceBS(const double S, const double K, const double T, const double r, const double sigma)
{
  double t = T/ (double)252.0 ;
  
  return norm_cdf(d_1(S,K,T,r,sigma)) * S - norm_cdf(d_2(S,K,T,r,sigma)) * K * exp((-1 *r )* (t));
  
}
// [[Rcpp::export]]
double putPriceBS(const double S, const double K, const double T, const double r, const double sigma)
{
  double t = T/ (double)252.0 ;
  
  return -S* norm_cdf(-d_1( S, K, T, r, sigma))+K*exp(-r*t) * norm_cdf(-d_2(S, K, T, r, sigma));
}
// [[Rcpp::export]]
double callImpliedVol(const double S, const double K, const double T, const double r, const double market)
{
  double m = .01;
  double n = .35; 
  double epsilon = .001; 
  
  double x = 0.5 * (m + n); 
  
  double y = callPriceBS(S,K, T, r, x); 
  
  do 
  {
    if( y < market)
    {
      m = x; 
    }
    
    if( y > market)
    {
      n = x; 
      
    }
    
    x = 0.5 * (m+n); 
    y =  callPriceBS(S,K, T, r, x); 
    
  }while(fabs(y - market) > epsilon); 
  
  return x; 
  
}
// [[Rcpp::export]]
double putImpliedVol(const double S, const double K, const double T, const double r, const double market)
{
  double m = .01;
  double n = .35; 
  double epsilon = .001; 
  
  double x = 0.5 * (m + n); 
  
  double y = putPriceBS(S,K, T, r, x); 
  
  do 
  {
    if( y < market)
    {
      m = x; 
    }
    
    if( y > market)
    {
      n = x; 
      
    }
    
    x = 0.5 * (m+n); 
    y =  putPriceBS(S,K, T, r, x); 
    
  }while(fabs(y - market) > epsilon); 
  
  return x; 
  
}



// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

 /*** R

library(Rcpp)
library(ggplot2)
sourceCpp("/Users/peterpeluso/holydata/f.cpp")
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
put_price[6] <- 12.80
put_price[7] <- 
for(i in 1 : 7)
{
ink[i] <- (putImpliedVol(141.15, strike[i], 21, .017, put_price[i]))
}


smile <- data.frame(ink, strike)
p <- ggplot(smile, aes(x = strike, y = ink)) +
  geom_line(colour="red", linetype="dashed", size=1.5) +
  xlab("Strike") + ylab("Implied Volatility") + ggtitle("/KC Volatility Smile")

 */
