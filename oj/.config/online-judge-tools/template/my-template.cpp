<%!
    import onlinejudge_template.generator.cplusplus as cplusplus
    import onlinejudge_template.generator.about as about
%>\
<%
    data['config']['using_namespace_std'] = True
    data['config']['long_long_int'] = 'll'
%>\
#include <cstdio>
#include <iostream>
#include <string>
#include <vector>
#include <array>
#include <algorithm>
#include <utility>
#include <numeric>
#include <deque>
#include <stack>
#include <queue>
#include <map>
#include <unordered_map>
#include <set>
#include <unordered_set>
#include <cmath>
#include <limits>
#include <cassert>
#include <bitset>
#include <list>
#include <iomanip>
#include <cctype>
#include<chrono>
#include<random>
#include<variant>

#define for0(i, N)    for (long long i = 0  ; i <  (N); ++i)
#define for1n(i, N)   for (long long i = 1  ; i <= (N); ++i)
#define for1(i, N)    for (long long i = 1  ; i <  (N); ++i)
#define forn(i, N)    for (long long i = 0  ; i <= (N); ++i)
#define forx(i, N, x) for (long long i = (x); i <  (N); ++i)
#define forl(i, N, x, a) for (long long i = (x), a = 0; a < (N); i = (i + 1) % (N), ++a)
#define int long long

long double constexpr pi = 3.141592653589793;

template<class T>
constexpr T sigma1(T i, T n){
    return (n - i + 1) * (i + n) / 2;
}

template<class T>
constexpr T sigma2(T i, T n){
    return (i == 1 ? n * (n + 1) * (2 * n + 1) / 6 :
            sigma2(1, n) - sigma2(1, i - 1));
}

template<class T>
constexpr T sigma3(T i, T n){
    if (i == 1) {
        auto temp = sigma1(i, n);
        return temp * temp;
    }
    return sigma2(1, n) - sigma2(1, i - 1);
}

template<size_t i>
std::bitset<i> operator++(std::bitset<i> b, signed){
    static_assert(i <= 64, "Cannot cast 64bit int.");
    return b = (unsigned long long)b + 1;
}

template<size_t i>
std::bitset<i> operator++(std::bitset<i> b){
    static_assert(i <= 64, "Cannot cast 64bit int.");
    auto temp = b;
    b++;
    return temp;
}

template<size_t i>
std::bitset<i> operator--(std::bitset<i> b, signed){
    static_assert(i <= 64, "Cannot cast 64bit int.");
    return b = (unsigned long long)b - 1;
}

template<size_t i>
std::bitset<i> operator--(std::bitset<i> b){
    static_assert(i <= 64, "Cannot cast 64bit int.");
    auto temp = b;
    b--;
    return temp;
}

struct unionFind{
    std::vector<unsigned long long> parent;
    unsigned long long n;
    unionFind(unsigned long long n) : parent(n), n(n){
        for (auto it = parent.begin(); it != parent.end(); ++it) {
            *it = std::distance(parent.begin(), it);
        }
    }

    long long root(unsigned long long k){
        assert(k < n);

        return parent[k] == k ? k : root(parent[k]);
    }

    bool same(unsigned long long k, unsigned long long l){
        assert(k < n && l < n);

        return root(k) == root(l);
    }

    bool unite(unsigned long long k, unsigned long long l){
        assert(k < n && l < n);

        if (same(k, l)) return false;
        parent[root(l)] = root(k);
        return true;
    }

    auto rootList(void){
        std::unordered_set<unsigned long long> s;
        for (unsigned long long i = 0; i < n; ++i)
            if (i == parent[i]) s.insert(i);
        return s;
    }
};

std::map<unsigned long long, unsigned long long> pf(unsigned long long x){
    unsigned long long y = x;
    unsigned long long a = 2;
    std::map<unsigned long long, unsigned long long> m;
    while (a * a <= x) {
        if (y % a == 0) {
            y /= a;
            ++m[a];
        } else {
            ++a;
        }
    }
    if (y != 1) ++m[y];

    return m;
}

long long constexpr mod = 1000000007LL;

class llm{
    long long l;
    static std::unordered_map<long long, llm> inv;

public:
    llm(long long m = 0LL) : l((m % mod >= 0)?(m % mod):(m % mod + mod)) {};

    llm inverse() const {
        if(inv.find(l) == inv.end()){
            llm x = pow(mod - 2);
            inv.emplace(l, x);
            return x;
        }
        return inv[l];
    }

    llm pow(long long i) const {
        if (i < 0)  return inverse().pow(-i);
        return pow(l, i, 1);
    }

    static llm pow (long long a, long long i, long long base = 1) {
        if (i == 0) return base;
        return pow( a * a % mod, i >> 1, base * ((i & 1) == 0 ? 1 : a) % mod);
    }

    llm operator<< (long long i) const {return pow( i);};
    llm operator>> (long long i) const {return pow(-i);}

    llm operator+ (llm m)       const {return llm(l + m.l);}
    llm operator+ (signed m)       const {return llm(l + (m % mod));}
    llm operator+ (long m)      const {return llm(l + (m % mod));}
    llm operator+ (long long m) const {return llm(l + (m % mod));}

    llm operator- (llm m)       const {return llm(l - m.l);}
    llm operator- (signed m)       const {return llm(l - (m % mod));}
    llm operator- (long m)      const {return llm(l - (m % mod));}
    llm operator- (long long m) const {return llm(l - (m % mod));}

    llm operator* (llm m)       const {return llm(l * m.l);}
    llm operator* (signed m)       const {return llm(l * (m % mod));}
    llm operator* (long m)      const {return llm(l * (m % mod));}
    llm operator* (long long m) const {return llm(l * (m % mod));}

    llm operator/ (llm m)       const {return llm((*this) * m.inverse() % mod);}
    llm operator/ (signed m)       const {return llm(l * llm(m).inverse()) % mod;}
    llm operator/ (long m)      const {return llm(l * llm(m).inverse()) % mod;}
    llm operator/ (long long m) const {return llm(l * llm(m).inverse()) % mod;}

    long long operator++ (void) {return l = (l + 1) % mod;}
    long long operator++ (signed)  {long long tmp = l; ++(*this); return tmp;}

    long long operator-- (void) {return l = (l - 1) % mod;}
    long long operator-- (signed)  {long long tmp = l; --(*this); return tmp;}

    llm operator+ (void) const {return llm( l);}
    llm operator- (void) const {return llm(-l);}

    template<typename T> llm operator+= (T m) {return *this = *this + m;}
    template<typename T> llm operator-= (T m) {return *this = *this - m;}
    template<typename T> llm operator*= (T m) {return *this = *this * m;}
    template<typename T> llm operator/= (T m) {return *this = *this / m;}

    llm combination(long long n, long long r, llm base = 1){ // O(r)
        assert(n >= r);
        if (r == 0) return base;
        return combination(n, r - 1, base * llm(n - r + 1) * llm(r).inverse());
    }

    operator long long() const {return l;}

    friend std::istream &operator>>(std::istream &stream, llm &m);

};

std::unordered_map<long long, llm> llm::inv;

std::istream &operator>>(std::istream &stream, llm &m){
    long long tmp;
    stream >> tmp;
    m.l = tmp % mod;
    return stream;
}


long long constexpr inf = std::numeric_limits<long long>::max();

template<typename Container>
auto compress(Container container){
    using ValueType = typename Container::value_type;
    std::vector<std::pair<ValueType, unsigned long long >> m;
    unsigned long long counter = 1;
    std::variant<ValueType, std::false_type> before = false;
    for(auto &i : container){
        if (std::holds_alternative<ValueType>(before) && std::get<ValueType>(before) == i) {
            ++counter;
        } else {
            m.push_back({i, counter});
            counter = 1;
            before = i;
        }
    }
    m.push_back({*container.rbegin(), counter});
    return m;
}

template<typename Container>
auto summarize(Container container){
    std::unordered_map<typename Container::value_type, unsigned long long> m;
    for(auto &i : container){
        ++m[i];
    }
    return m;
}

using namespace std;

typedef long long ll;
typedef unsigned long long ull;
typedef long double ld;
typedef priority_queue<ll> pq;
typedef priority_queue<ll,vector<ll>, greater<ll>> pql;
typedef stack<ll> stk;
typedef queue<ll> qu;
typedef pair<ll, ll> pll;
typedef map<ll, ll> mll;
typedef unordered_map<ll, ll> umll;
typedef set<ll> sll;
typedef unordered_set<ll> usll;
typedef vector<ll> vll;
typedef vector<ld> vld;
typedef vector<vector<ll>> vvll;

signed main(void){
    cin.tie(0); ios::sync_with_stdio(false);cout<<fixed<<setprecision(10);

${cplusplus.read_input(data)}



    return 0;
}
