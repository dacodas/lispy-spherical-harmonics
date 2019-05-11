#include <gsl/gsl_sf_legendre.h>
#include <cmath>
#include <iostream>
#include <sstream>
#include <iomanip>

int main(int argc, char* argv[])
{
    int l, m;
    double phi, theta;

    std::string current_line;

    std::getline(std::cin, current_line);
    std::istringstream current_line_stream(current_line);
    current_line_stream >> l >> m;

    while ( std::getline(std::cin, current_line) )
    {
        std::istringstream current_line_stream(current_line);
        current_line_stream >> theta >> phi;

        std::cout << std::fixed
                  << std::setprecision( 3 ) 
                  << cos(m*phi) * gsl_sf_legendre_sphPlm(l, m, cos(theta))
                  << '\n';
    }
}
