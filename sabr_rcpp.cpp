// #include <Rcpp.h>
#include <iostream>
#include <algorithm> 
#include <cmath>
#include <vector> 
#include <limits>
#define PI 3.14159265359

//using namespace Rcpp;

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
double box_mueller(double sigma = 1, double mu = 0 );
double callPriceMonteCarlo(double sims, double S, double K, double T, double r, double sigma);
double putPriceMonteCarlo(double sims, double S, double K, double T, double r, double sigma);
double callDeltaBS(const double S, const double K, const double T, const double r, const double sigma);
double putDeltaBS(const double S, const double K, const double T, const double r, const double sigma); 
double callGammaBS(const double S, const double K, const double T, const double r, const double sigma);
double putGammaBS(const double S, const double K, const double T, const double r, const double sigma);
double callVegaBS(const double S, const double K, const double T, const double r, const double sigma);
double putVegaBS(const double S, const double K, const double T, const double r, const double sigma);
double callThetaBS(const double S, const double K, const double T, const double r, const double sigma);
double putThetaBS(const double S, const double K, const double T, const double r, const double sigma);
double callPriceCRR(const double S, const double K, const double T, const double r, const double sigma, const double div, int N);
double putPriceCRR(const double S, const double K, const double T, const double r, const double sigma, const double div, int N);
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
// [[Rcpp::export]]
double box_mueller(double sigma, double mu)
{
  const double epsilon = std::numeric_limits<double>::min();
  const double two_pi = 2.0*PI;
  
  static double z0, z1;
  static bool generate;
  generate = !generate;
  
  if (!generate)
    return z1 * sigma + mu;
  
  double u1, u2;
  do
  {
    u1 = rand() * (1.0 / RAND_MAX);
    u2 = rand() * (1.0 / RAND_MAX);
  }
  while ( u1 <= epsilon );
  
  z0 = sqrt(-2.0 * log(u1)) * cos(two_pi * u2);
  z1 = sqrt(-2.0 * log(u1)) * sin(two_pi * u2);
  return z0 * sigma + mu ;
}
// [[Rcpp::export]]
double callPriceMonteCarlo(double sims, double S, double K, double T, double r, double sigma)
{
  double t = T/ (double)252.0 ;
  double S_adjust = S * exp(t*(r-0.5*sigma*sigma));
  double S_cur = 0.0;
  double payoff = 0.0; 
  
  for(int i = 0; i < sims; i++)
  {
    double randNorm = box_mueller();
    S_cur = S_adjust * exp(sqrt(sigma*sigma*t)* randNorm);
    payoff += std::max(S_cur - K, 0.0);
  }
  
  return (payoff/ static_cast<double>(sims)) * exp(-r*t);
}
// [[Rcpp::export]]
double putPriceMonteCarlo(double sims, double S, double K, double T, double r, double sigma)
{
  double t = T/ (double)252.0 ;
  double S_adjust = S * exp(t*(r-0.5*sigma*sigma));
  double S_cur = 0.0;
  double payoff = 0.0; 
  
  for(int i = 0; i < sims; i++)
  {
    double randNorm = box_mueller();
    S_cur = S_adjust * exp(sqrt(sigma*sigma*t)* randNorm);
    payoff += std::max(K - S_cur, 0.0);
  }
  
  return (payoff/ static_cast<double>(sims)) * exp(-r*t);
}

// [[Rcpp::export]]
double callDeltaBS(const double S, const double K, const double T, const double r, const double sigma)
{
  return norm_cdf(d_1(S,K,T,r,sigma));

}
// [[Rcpp::export]]
double putDeltaBS(const double S, const double K, const double T, const double r, const double sigma)
{
  return norm_cdf(d_1(S,K,T,r,sigma)) - 1; 
}
// [[Rcpp::export]]
double callGammaBS(const double S, const double K, const double T, const double r, const double sigma)
{
    double t = T/ (double)252.0 ;
  return (norm_pdf(d_1(S,K,T,r,sigma)))/(S * sigma * sqrt(t));
}
// [[Rcpp::export]]
double putGammaBS(const double S, const double K, const double T, const double r, const double sigma)
{
    double t = T/ (double)252.0 ;
  return (norm_pdf(d_1(S,K,T,r,sigma)))/(S * sigma * sqrt(t));
}
// [[Rcpp::export]]
double callVegaBS(const double S, const double K, const double T, const double r, const double sigma)
{
  double t = T/ (double)252.0 ;
  return S * norm_pdf(d_1(S,K,T,r,sigma)) * sqrt(t);
}
// [[Rcpp::export]]
double putVegaBS(const double S, const double K, const double T, const double r, const double sigma)
{
  double t = T/ (double)252.0 ;
  return S * norm_pdf(d_1(S,K,T,r,sigma)) * sqrt(t);
}
// [[Rcpp::export]]
double callThetaBS(const double S, const double K, const double T, const double r, const double sigma) 
{
  double t = T/ (double)252.0 ;
  return -(S*norm_pdf(d_1( S, K, T, r, sigma))*sigma)/(2*sqrt(t)) 
    - r*K*exp(-r*t)*norm_cdf(d_2(S, K, T, r, sigma));
}
// [[Rcpp::export]]
double putThetaBS(const double S, const double K, const double T, const double r, const double sigma) 
 {
    double t = T/ (double)252.0 ;
 return -(S*norm_pdf(d_1(S, K, T, r, sigma))*sigma)/(2*sqrt(t)) 
    + r*K*exp(-r*t)*norm_cdf(-d_2( S, K, T, r, sigma));
 }

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//


double SABR(const double f, const double K) // f is price and K is strike
{
  double alpha; //can use ATM iv 
  double beta; // can use a known value ex. 1

  double testB; 
for(int i = 0; i <= 1; i += .001)
  {
    testB = log10(alpha) - (1-i)*log10(f)

    if(fabs(testB - log10(alpha)) < .01)
    {
      beta = testB; 

      break; 
    }

  }


}

double callPriceCRR(const double S, const double K, const double T, const double r, const double sigma, const double div, int N)
{
    int i, j ;
    double prob;
    double s[200][200] = {0.0};
    double c[200][200] = {0.0};
    double a;
    double num = 0.0;
    double up = 0.0;
    double down = 0.0;
    double dt = 0.0;
    
    
    dt = T/N;
    up = exp(sigma * sqrt(dt));
    down = 1/up;
    a = exp((r - div)* dt);
    prob = (a- down) / (up - down);
    
    for(i = 0; i <= N; i++)
    {
        for(j = 0; j <= i; j++)
        {
            s[i][j] = S*(pow(up,j)) * pow(down, i-j);
            c[i][j] = 0;
        }
    }
    
    for(j = N; j >= 0; j--)
    {
        c[N][j] = fmax(s[N][j]- K, 0);
    }
    
    for(i = N-1; i >= 0; i--)
    {
        for(j =i; j >= 0; j--)
        {
            c[i][j] = exp(-r*dt) *(prob* (c[i+1][j+1]) + (1-prob)* (c[i+1][j]));
            c[i][j] = fmax(s[i][j] - K, c[i][j]);
        }
    }
    return c[0][0];
}

double putPriceCRR(const double S, const double K, const double T, const double r, const double sigma, const double div, int N)
{
    int i, j ;
    double prob;
    double s[200][200] = {0.0};
    double c[200][200] = {0.0};
    double a;
    double num = 0.0;
    double up = 0.0;
    double down = 0.0;
    double dt = 0.0;
    
    
    dt = T/N;
    up = exp(sigma * sqrt(dt));
    down = 1/up;
    a = exp((r - div)* dt);
    prob = (a- down) / (up - down);
    
    for(i = 0; i <= N; i++)
    {
        for(j = 0; j <= i; j++)
        {
            s[i][j] = S*(pow(up,j)) * pow(down, i-j);
            c[i][j] = 0;
        }
    }
    
    for(j = N; j >= 0; j--)
    {
        c[N][j] = fmax(K - s[N][j], 0);
    }
    
    for(i = N-1; i >= 0; i--)
    {
        for(j =i; j >= 0; j--)
        {
            c[i][j] = exp(-r*dt) *(prob* (c[i+1][j+1]) + (1-prob)* (c[i+1][j]));
            c[i][j] = fmax(K - s[i][j] , c[i][j]);
        }
    }
    return c[0][0];
    
}