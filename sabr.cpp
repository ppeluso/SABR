#include <iostream>
#include <algorithm> 
#include <cmath>
#include <limits>
#define PI 3.14159265359

double norm_cdf(const double x); 
double norm_pdf(const double x); 
double d_1(const double S, const double K, const double T, const double r, const double sigma);
double d_2(const double S, const double K, const double T, const double r, const double sigma);
double callPriceBS(const double S, const double K, const double T, const double r, const double sigma);
double putPriceBS(const double S, const double K, const double T, const double r, const double sigma);
double callImpliedVol(const double S, const double K, const double T, const double r, const double market);
double putImpliedVol(const double S, const double K, const double T, const double r, const double market);
double ranf(double max);
double box_mueller(double sigma = 1, double mu = 0 );
double callPriceMonteCarlo(double sims, double S, double K, double T, double r, double sigma);
double putPriceMonteCarlo(double sims, double S, double K, double T, double r, double sigma); 
int main()
{
  // First we create the parameter list                                                                               
  int num_sims = 10000000;   // Number of simulated asset paths                                                       
  double S = 100.0;  // Option price                                                                                  
  double K = 100.0;  // Strike price                                                                                  
  double r = 0.05;   // Risk-free rate (5%)                                                                           
  double v = 0.2;    // Volatility of the underlying (20%)                                                            
  double T = 252;    // One year until expiry                                                                         

  // Then we calculate the call/put values via Monte Carlo                                                                          
  double call = callPriceMonteCarlo(num_sims, S, K, T, r, v);
  

  // Finally we output the parameters and prices                                                                      
  std::cout << "Number of Paths: " << num_sims << std::endl;
  std::cout << "Underlying:      " << S << std::endl;
  std::cout << "Strike:          " << K << std::endl;
  std::cout << "Risk-Free Rate:  " << r << std::endl;
  std::cout << "Volatility:      " << v << std::endl;
  std::cout << "Maturity:        " << T << std::endl;

  std::cout << "Call Price:      " << call << std::endl;

	
}


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

double norm_pdf(const double x)
{
		
	return (1/ sqrt(2 * PI)) * exp((-0.5)* (x * x)); 

}

double d_1(const double S, const double K, const double T, const double r, const double sigma)
{
	double t = T/ (double)252.0 ;
	return (log(S/K) + (r + ((sigma * sigma )/ 2)) * t )/ (double)(sigma * sqrt(t)); 
}
double d_2(const double S, const double K, const double T, const double r, const double sigma)
{
	double t = T/ (double)252.0 ;
	return(d_1(S,K,T,r,sigma) - (sigma * sqrt(t)));

}

double callPriceBS(const double S, const double K, const double T, const double r, const double sigma)
{
	double t = T/ (double)252.0 ;

	return norm_cdf(d_1(S,K,T,r,sigma)) * S - norm_cdf(d_2(S,K,T,r,sigma)) * K * exp((-1 *r )* (t));

}

double putPriceBS(const double S, const double K, const double T, const double r, const double sigma)
{
	double t = T/ (double)252.0 ;

	return -S* norm_cdf(-d_1( S, K, T, r, sigma))+K*exp(-r*t) * norm_cdf(-d_2(S, K, T, r, sigma));
}

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

double ranf(double max)
{
  return (((double)rand())/RAND_MAX)*max;
}

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





