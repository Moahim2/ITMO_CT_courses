#include <iostream>
#include <vector>

using namespace std;

long long mod = 104857601LL;

long long m(long long x) {
    if (x < 0) {
        x = mod * mod + x;
    }
    return x % mod;
//    long long r = x % mod;
//    return r + (x & (((r | x) and (r | -r)) >> 63));
}

int main2() {
    size_t k;
    long long N;
    cin >> k >> N;
    N--;
    vector<long long> A{};
    vector<long long> Q{};
    long long t;
    for (size_t i = 0; i < k; i++) {
        cin >> t;
        A.push_back(t);
    }

    Q.push_back(1LL);
    for (size_t i = 0; i < k; i++) {
        cin >> t;
        Q.push_back(m(-t));
    }

    for (size_t i = 0; i < k; i++) {
        A.push_back(0);
    }

    while (N >= k) {
        for (size_t i = k; i < 2 * k; i++) {
            long long sum = 0LL;
            for (size_t j = 1; j < k + 1; j++) {
                sum = m(sum + m(A[i - j] * m(-Q[j])));
            }
            A[i] = sum;
        }
        size_t flag = (N % 2);
        N /= 2;
        vector<long long> D{};
        for (size_t i = 0; i < 2 * k; i++) {
            if (i % 2 == flag) {
                D.push_back(A[i]);
            }
        }

        vector<long long> AltQ{};
        for (size_t i = 0; i < k + 1; i++) {
            if (i % 2 == 1) {
                AltQ.push_back(m(-Q[i]));
            } else {
                AltQ.push_back(Q[i]);
            }
        }

        vector<long long> mulQ{};
        size_t size = Q.size();
        for (size_t n = 0; n < size + size - 1; n++) {
            long long s = 0LL;
            if (n < size) {
                for (size_t i = 0; i <= n; i++) {
                    s = m(s + m(Q[i] * AltQ[n - i]));
                }
            } else {
                for (size_t i = n - size + 1; i < size; i++) {
                    s = m(s + m(Q[i] * AltQ[n - i]));
                }
            }
            mulQ.push_back(s);
        }
        vector<long long> X{};
        for (size_t i = 0; i < mulQ.size(); i++) {
            if (i % 2 == 0) {
                X.push_back(mulQ[i]);
            }
        }

        Q = X;

        A = D;
        for (size_t i = A.size(); i < 2 * k; i++) {
            A.push_back(0LL);
        }
        //cout << A[A.size() - 1] << "\N";
    }
    cout << A[N] << endl;
    //cout << m(m(-1LL) * 104857600LL);
    return 0;
}

