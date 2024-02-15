#include <bits/stdc++.h>

#include <iostream>
#include <vector>
#include <deque>
#include <iterator>
#include <algorithm>

using namespace std;



class Mul {

private:
    vector<complex<double>> AA;
    vector<complex<double>> BB;
    size_t N;
    vector<complex<double>> W {};


public:
    Mul(const vector<int>& A, const vector<int> & B) {
        size_t m = max(A.size(), B.size());
        size_t deg = ceil(log2(m));
        auto newSize = ((size_t) pow(2, deg)) * 2;
        N = newSize;
        AA = vector<complex<double>>(newSize, complex<double>(0, 0));
        BB = vector<complex<double>>(newSize, complex<double>(0, 0));
        for (int i = 0; i < A.size(); i++) {
            AA[i] = A[i];
        }
        for (int i = 0; i < B.size(); i++) {
            BB[i] = B[i];
        }
    }


    vector<int> getMul() {
        double p = 2 * 3.14159265358979323846 / (double) N;
        auto w = complex<double>(cos(p), sin(p));

        for (int i = 0; i < N + 1; i++) {
            W.push_back(pow(w, i));
        }


        auto gA = calcDft(AA, N, 1);
        auto gB = calcDft(BB, N, 1);


        auto C = vector<complex<double>>(N, complex<double>(0, 0));



        for (int i = 0; i < N; i++) {
            C[i] = gA[i] * gB[i];
        }
        auto invgC = calcInvDftImpl(C, N);

        auto ans = vector<int>();
        for (int i = 0; i < N; i++) {
            ans.push_back(int (::round(invgC[i].real())));
        }

        return makeTransfers(ans);
    }


private:
    vector<complex<double>> calcDft(const vector<complex<double>>&  A, size_t n, int k) {
        if (n == 1) {
            return A;
        }

        auto A0 = vector<complex<double>>();
        auto A1 = vector<complex<double>>();

        for (int i = 0; i < A.size(); i++) {
            if (i % 2 == 0) {
                A0.push_back(A[i]);
            } else {
                A1.push_back(A[i]);
            }
        }

        auto g0 = calcDft(A0, n/2, (k * 2));
        auto g1 = calcDft(A1, n/2, (k * 2));
        auto g = vector<complex<double>>(n, complex<double>(0, 0));

        for (int i = 0; i < n/2; i++) {
            g[i] = g0[i] + W[(i * k) % N] * g1[i];
            g[n/2 + i] = g0[i] - W[(i * k) % N] * g1[i];
        }
        return g;
    }

    vector<complex<double>> calcInvDftImpl(const vector<complex<double>>& A, size_t n) {
        for (int i = 0; i < N + 1; i++) {
            W[i] = pow(W[i], -1);
        }

        auto ans = calcDft(A, n, 1);
        for (int i = 0; i < n; i++) {
            ans[i] /= (double) n;
        }

        return ans;
    }

    static vector<int> makeTransfers(vector<int>& A) {
        auto ans = vector<int>();
        ans.push_back(A[0] % 10);
        int adding = A[0] / 10;

        for (int i = 1; i < A.size(); i++) {
            int tmp = A[i] + adding;
            ans.push_back(tmp % 10);
            adding = tmp / 10;
        }

        if (A[A.size() - 1] / 10 > 0) {
            ans.push_back(A[A.size() - 1] / 10);
        }
        std::reverse(ans.begin(), ans.end());
        return ans;
    }



};

int main() {
    string a, b;
    cin >> a >> b;
    if (a.front() == '0' || b.front() == '0') {
        cout << '0';
        return 0;
    }
    vector<int> A, B;
    std::reverse(a.begin(), a.end());
    std::reverse(b.begin(), b.end());
    int minusA = 1;
    int minusB = 1;
    for (char i : a) {
        if (i == '-') {
            minusA = -1;
            continue;
        }
        A.push_back((int) i - 48);
    }

    for (char i : b) {
        if (i == '-') {
            minusB = -1;
            continue;
        }
        B.push_back((int) i - 48);
    }

    auto mul = Mul(A, B);
    auto ans = mul.getMul();

    bool flag = false;
    for (int an : ans) {
        if (!flag && an != 0) {
            flag = true;
            if (minusA * minusB == -1) {
                cout << '-';
            }
        }
        if (flag) {
            cout << an;
        }
    }

}