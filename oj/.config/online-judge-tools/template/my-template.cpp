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

struct powers{
    long long base;
    long long max_exponent;
    std::vector<long long> baseTo2To;

    powers(long long base, long long max_exponent) : base(base), max_exponent(max_exponent), baseTo2To(max_exponent + 1){
        baseTo2To[0] = base;
        for (auto it = baseTo2To.begin() + 1, bef = baseTo2To.begin(); it != baseTo2To.end(); bef = it++) {
            *it = *bef * *bef;
        }
    }

    long long operator[](long long n){
        return baseTo2To[n];
    }

    long long operator()(long long n){
        long long x = 1;
        size_t s = 0;
        while (n != 0) {
            assert(s <= max_exponent);
            if (n & 1) {
                x *= baseTo2To[s];
            }
            ++s;
            n >>= 1;
        }
        return x;
    }
};

template<typename T>
T constexpr inf = std::numeric_limits<T>::max();

template<typename T>
T constexpr minf = std::numeric_limits<T>::min();

// {1,1,1,4,2,2,3}->{(1,3),(4,1),(2,2),(3,1)}
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

// {1,3,2,3,2,2}->{1: 1, 2: 3, 3: 2}
template<typename Container>
auto summarize(Container container){
    std::unordered_map<typename Container::value_type, unsigned long long> m;
    for(auto &i : container){
        ++m[i];
    }
    return m;
}

template<typename T, typename S>
std::ostream &operator<<(std::ostream &stream, std::pair<T, S> const &p) {
    stream << "(" << p.first << "," << p.second << ")" << "\n";
    return stream;
}

template<typename T>
void outputVectorHorizontal(std::vector<T> const &v){
    for (auto &&it = v.cbegin(); it != v.cend(); ++it) {
        std::cout << (it == v.begin() ? "" : " ");
        std::cout << *it;

    }
    std::cout << "\n";
}

template<typename T>
void outputVectorVertical(std::vector<T> const &v){
    for (auto &&it = v.cbegin(); it != v.cend(); ++it) {
        std::cout << *it << "\n";
    }
}

template<typename T>
void outputVector2D(std::vector<std::vector<T>> const &vv){
    for(auto const &v: vv){
        outputVectorHorizontal(v);
    }
}

// ({1, 3, 4, 8, 10}, 5)->(v.begin()+3, 5-4)
template<typename T>
auto nearest(std::vector<T> const &v, T const &target) {
    auto r = std::lower_bound(v.begin(), v.end(), target);
    auto l = r - 1;

    if (r == v.end()) {
        return std::pair{l, std::abs(*l - target)};
    }
    if (r == v.begin()) {
        return std::pair{r, std::abs(*r - target)};
    }

    if(std::abs(*r - target) < std::abs(*l - target)){
        return std::pair{r, std::abs(*r - target)};
    } else {
        return std::pair{l, std::abs(*l - target)};
    }
}

class factorial{
    std::vector<long long> fact;
public:
    factorial(long long m) : fact(m + 1, 1){
        for (long long i = 1; i <= m; ++i) {
            fact[i] = fact[i - 1] * i;
        }
    }

    long long operator()(size_t i){
        return fact.at(i);
    }
};

class combination{
    std::vector<std::vector<long long>> comb;

    long long C(size_t n, size_t i){
        assert(n < comb.size());
        assert(0 <= i && i <= n);

        if (!comb[n][i]) {
            comb[n][i] = C(n - 1, i - 1) + C(n - 1, i);
        }

        return comb[n][i];
    }

public:
    combination(long long m) : comb(m + 1, std::vector<long long>(m + 1, 0)){
        for (long long n = 0; n <= m; ++n) {
            comb[n][0] = 1;
            comb[n][n] = 1;
        }
    }

    long long operator()(size_t n, size_t i){
        return C(n, i);
    }
};

using namespace std;

using ll = long long;
using ull = unsigned long long;
using ld = long double;
using pq = priority_queue<ll>;
using pql = priority_queue<ll,vector<ll>, greater<ll>>;
using stk = stack<ll>;
using qll = queue<ll>;
using pll = pair<ll, ll>;
using mll = map<ll, ll>;
using umll = unordered_map<ll, ll>;
using sll = set<ll>;
using usll = unordered_set<ll>;
using vll = vector<ll>;
using vld = vector<ld>;
using vvll = vector<vector<ll>>;
using vpll = vector<pair<ll, ll>>;

signed main(void){
    cin.tie(0); ios::sync_with_stdio(false);cout<<fixed<<setprecision(10);

${cplusplus.read_input(data)}



    return 0;
}
