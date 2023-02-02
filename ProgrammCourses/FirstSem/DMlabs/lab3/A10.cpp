#include <iostream>
#include <set>
#include <vector>


typedef std:: vector <int> prefix;
int cur = -1;
int last = 0;

void createVector(int n, int sum) {
    if (last > 0) {
        if (sum == n) {
                for(int i = 0; i < cur; i++) {
                    printf(prefix[i] + "+");
                }
                printf(last);
                printf('\n');
                return;
            }
            prefix[cur] = last;
            for (int i = last; i <= n - sum; i++) {
                cur++;
                last = i;
                createVector(n, sum + i);
                cur--;
                last = prefix[cur];
            }
    } else {
        for (int i = 1; i < n + 1; i++) {
                cur++;
                last = i;
                createVector(n, i);
                last = 0;
                cur--;
            }
    }
}



int main() {
    int n;
    cin >> n;
    createVector(n, 0);
    return 0;
}