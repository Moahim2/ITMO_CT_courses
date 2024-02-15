#include <iostream>
#include <stdlib.h>
using namespace std;

int main(int argc, char *argv[]) {
    if (argc != 2) {
	cout << "count of arguments must be 1" << endl;
	return 1;
    }
    
    double x = atof(argv[1]);
    double ans = 1.0;
    double t = 1.0;
    for (double i = 1; i < 300000000; ++i) {
	t = t * x / i;
	ans += t;
    }
    
    cout << ans << endl;
    return 0;
}
