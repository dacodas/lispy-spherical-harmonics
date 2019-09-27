#include <stddef.h>
#include <gsl/gsl_sf_legendre.h>
#include <cmath>
#include <iostream>
#include <sstream>
#include <iomanip>

int main(int argc, char* argv[])
{
    int l, m, row_length, column_count = 0;
    double phi, theta;

    std::string current_line;

    std::getline(std::cin, current_line);
    std::istringstream current_line_stream(current_line);
    current_line_stream >> l >> m >> row_length;

    std::cout << "P3\n# Some picture\n" << row_length << " " << row_length << "\n255\n";

    while ( std::getline(std::cin, current_line) )
    {
        std::istringstream current_line_stream(current_line);
        current_line_stream >> theta >> phi;
        // current_line_stream >> phi >> theta;

        double rho = cos(m*phi) * gsl_sf_legendre_sphPlm(l, m, cos(theta));
        int rho_int = 255 * ( rho + 1.0 ) / 2.0;

        std::cout << std::fixed
                  << std::setprecision( 3 ) 
                  << rho_int << " 0 0"
                  << ( ( column_count + 1 ) % row_length == 0 ? '\n' : ' ' );

        ++column_count;
    }
}
