<%!
    import onlinejudge_template.generator.cplusplus as cplusplus
    import onlinejudge_template.generator.about as about
%>\
<%
    data['config']['using_namespace_std'] = True
    data['config']['long_long_int'] = 'll'
%>\
#include <algorithm>
#include <array>
#include <bitset>
#include <cassert>
#include <cctype>
#include <chrono>
#include <cmath>
#include <cstdio>
#include <deque>
#include <iomanip>
#include <iostream>
#include <limits>
#include <list>
#include <map>
#include <numeric>
#include <queue>
#include <random>
#include <set>
#include <stack>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <variant>
#include <vector>

#define for0(i, N) for (long long i = 0; i < (N); ++i)
#define for1n(i, N) for (long long i = 1; i <= (N); ++i)
#define for1(i, N) for (long long i = 1; i < (N); ++i)
#define forn(i, N) for (long long i = 0; i <= (N); ++i)
#define forlr(i, l, r) for (long long i = (l); i < (r); ++i)
#define forl(i, N, x, a)                                                       \
  for (long long i = (x), a = 0; a < (N); i = (i + 1) % (N), ++a)

//// Output

template <typename T, typename S>
std::ostream &operator<<(std::ostream &stream, std::pair<T, S> const &p) {
  stream << "(" << p.first << "," << p.second << ")";
  return stream;
}

template <typename Container>
void outputVectorHorizontal(Container const &v, std::ostream &c = std::cerr) {
  for (auto &&it = v.cbegin(); it != v.cend(); ++it) {
    c << (it == v.begin() ? "" : " ");
    c << *it;
  }
  c << "\n";
}

template <typename Container>
void outputVectorVertical(Container const &v, std::ostream &c = std::cerr) {
  for (auto &&it = v.cbegin(); it != v.cend(); ++it) {
    c << *it << "\n";
  }
}

template <typename Container>
void outputVector2D(Container const &vv, std::ostream &c = std::cerr) {
  for (auto const &v : vv) {
    outputVectorHorizontal(v, c);
  }
}

//// Modclass

long long constexpr mod = 1000000007LL;

class llm {
  long long l;
  static std::unordered_map<long long, llm> inv;

public:
  llm(long long m = 0LL) : l((m % mod >= 0) ? (m % mod) : (m % mod + mod)){};

  llm inverse() const {
    if (inv.find(l) == inv.end()) {
      llm x = pow(mod - 2);
      inv.emplace(l, x);
      return x;
    }
    return inv[l];
  }

  llm pow(long long i) const {
    if (i < 0)
      return inverse().pow(-i);
    return pow(l, i, 1);
  }

  static llm pow(long long a, long long i, long long base = 1) {
    if (i == 0)
      return base;
    return pow(a * a % mod, i >> 1, base * ((i & 1) == 0 ? 1 : a) % mod);
  }

  llm operator<<(long long i) const { return pow(i); };
  llm operator>>(long long i) const { return pow(-i); }

  llm operator+(llm m) const { return llm(l + m.l); }
  llm operator+(signed m) const { return llm(l + (m % mod)); }
  llm operator+(long m) const { return llm(l + (m % mod)); }
  llm operator+(long long m) const { return llm(l + (m % mod)); }

  llm operator-(llm m) const { return llm(l - m.l); }
  llm operator-(signed m) const { return llm(l - (m % mod)); }
  llm operator-(long m) const { return llm(l - (m % mod)); }
  llm operator-(long long m) const { return llm(l - (m % mod)); }

  llm operator*(llm m) const { return llm(l * m.l); }
  llm operator*(signed m) const { return llm(l * (m % mod)); }
  llm operator*(long m) const { return llm(l * (m % mod)); }
  llm operator*(long long m) const { return llm(l * (m % mod)); }

  llm operator/(llm m) const { return llm((*this) * m.inverse() % mod); }
  llm operator/(signed m) const { return llm(l * llm(m).inverse()) % mod; }
  llm operator/(long m) const { return llm(l * llm(m).inverse()) % mod; }
  llm operator/(long long m) const { return llm(l * llm(m).inverse()) % mod; }

  long long operator++(void) { return l = (l + 1) % mod; }
  long long operator++(signed) {
    long long tmp = l;
    ++(*this);
    return tmp;
  }

  long long operator--(void) { return l = (l - 1) % mod; }
  long long operator--(signed) {
    long long tmp = l;
    --(*this);
    return tmp;
  }

  llm operator+(void) const { return llm(l); }
  llm operator-(void) const { return llm(-l); }

  template <typename T> llm operator+=(T m) { return *this = *this + m; }
  template <typename T> llm operator-=(T m) { return *this = *this - m; }
  template <typename T> llm operator*=(T m) { return *this = *this * m; }
  template <typename T> llm operator/=(T m) { return *this = *this / m; }

  llm combination(long long n, long long r, llm base = 1) { // O(r)
    assert(n >= r);
    if (r == 0)
      return base;
    return combination(n, r - 1, base * llm(n - r + 1) * llm(r).inverse());
  }

  operator long long() const { return l; }

  friend std::istream &operator>>(std::istream &stream, llm &m);
};

std::unordered_map<long long, llm> llm::inv;

std::istream &operator>>(std::istream &stream, llm &m) {
  long long tmp;
  stream >> tmp;
  m.l = tmp % mod;
  return stream;
}

//// container utils

// {1,1,1,4,2,2,3}->{(1,3),(4,1),(2,2),(3,1)}
template <typename Container> auto compress(Container container) {
  using ValueType = typename Container::value_type;
  std::vector<std::pair<ValueType, unsigned long long>> m;
  unsigned long long counter = 1;
  std::variant<ValueType, std::false_type> before = false;
  for (auto &i : container) {
    if (std::holds_alternative<ValueType>(before) &&
        std::get<ValueType>(before) == i) {
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
template <typename Container> auto summarize(Container container) {
  std::unordered_map<typename Container::value_type, unsigned long long> m;
  for (auto &i : container) {
    ++m[i];
  }
  return m;
}

// ({1, 3, 4, 8, 10}, 5)->(v.begin()+2, 5-4)
template <typename T> auto nearest(std::vector<T> const &v, T const &target) {
  auto r = std::lower_bound(v.begin(), v.end(), target);
  auto l = r - 1;

  if (r == v.end()) {
    return std::pair{l, std::abs(*l - target)};
  }
  if (r == v.begin()) {
    return std::pair{r, std::abs(*r - target)};
  }

  if (std::abs(*r - target) < std::abs(*l - target)) {
    return std::pair{r, std::abs(*r - target)};
  } else {
    return std::pair{l, std::abs(*l - target)};
  }
}

//// Mathmatical utils

long double constexpr pi = 3.141592653589793;

template <typename T> T constexpr inf = std::numeric_limits<T>::max();

template <typename T> T constexpr minf = std::numeric_limits<T>::min();

template <class T> constexpr T sigma1(T i, T n) {
  return (n - i + 1) * (i + n) / 2;
}

template <class T> constexpr T sigma2(T i, T n) {
  return (i == 1 ? n * (n + 1) * (2 * n + 1) / 6
                 : sigma2(1, n) - sigma2(1, i - 1));
}

template <class T> constexpr T sigma3(T i, T n) {
  if (i == 1) {
    auto temp = sigma1(i, n);
    return temp * temp;
  }
  return sigma2(1, n) - sigma2(1, i - 1);
}

std::map<unsigned long long, unsigned long long>
primaryFactorization(unsigned long long x) {
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
  if (y != 1)
    ++m[y];

  return m;
}

class factorial {
  std::vector<long long> fact;

public:
  factorial(long long m) : fact(m + 1, 1) {
    for (long long i = 1; i <= m; ++i) {
      fact[i] = fact[i - 1] * i;
    }
  }

  long long operator()(std::size_t i) { return fact.at(i); }
};

class combination {
  std::vector<std::vector<long long>> comb;

  long long C(std::size_t n, std::size_t i) {
    assert(n < comb.size());
    assert(0 <= i && i <= n);

    if (!comb[n][i]) {
      comb[n][i] = C(n - 1, i - 1) + C(n - 1, i);
    }

    return comb[n][i];
  }

public:
  combination(long long m) : comb(m + 1, std::vector<long long>(m + 1, 0)) {
    for (long long n = 0; n <= m; ++n) {
      comb[n][0] = 1;
      comb[n][n] = 1;
    }
  }

  long long operator()(std::size_t n, std::size_t i) { return C(n, i); }
};

struct powers {
  long long base;
  long long max_exponent;
  std::vector<long long> baseTo2To;

  powers(long long base, long long max_exponent)
      : base(base), max_exponent(max_exponent), baseTo2To(max_exponent + 1) {
    baseTo2To[0] = base;
    for (auto it = baseTo2To.begin() + 1, bef = baseTo2To.begin();
         it != baseTo2To.end(); bef = it++) {
      *it = *bef * *bef;
    }
  }

  long long operator[](long long n) { return baseTo2To[n]; }

  long long operator()(long long n) {
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

//// Union Find

struct UnionFind {
  std::vector<std::size_t> parent;
  std::vector<std::size_t> size;
  unsigned long long n;

  UnionFind(std::size_t n) : parent(n), size(n, 1), n(n) {
    for (auto it = parent.begin(); it != parent.end(); ++it) {
      *it = std::distance(parent.begin(), it);
    }
  }

  long long root(std::size_t k) {
    assert(k < n);
    // Compress path
    return parent[k] == k ? k : parent[k] = root(parent[k]);
  }

  bool sameRoot(std::size_t k, std::size_t l) {
    assert(k < n && l < n);

    return root(k) == root(l);
  }

  void updateParent(std::size_t const newParent, std::size_t const newChild) {
    parent[newChild] = newParent;
    size[newParent] += size[newChild];
  }

  // Return false if the two have already same root
  bool unite(std::size_t const k, std::size_t const l) {
    assert(k < n && l < n);

    if (sameRoot(k, l)) {
      return false;
    }

    // Union by size
    // size is only used on root
    auto [S, L] = size[l] <= size[k] ? std::tuple{l, k} : std::tuple{k, l};
    updateParent(root(L), root(S));

    return true;
  }

  auto rootList(void) const {
    std::unordered_set<std::size_t> s;
    for (unsigned long long i = 0; i < n; ++i)
      if (i == parent[i])
        s.insert(i);
    return s;
  }
};


// Left is false, right is true.
template<class T, class F>
T binarySearch(T l, T r, F func, T diff = 1) {
    while (std::abs(r - l) > diff) {
        T m = (l + r) / 2;
        (func(m) ? r : l) = m;
    }
    return r;
}

//// Graph

struct Arc {
  std::size_t to;
  long long cost;
  auto operator==(Arc const &a) const { return to == a.to && cost == a.cost; }
  auto operator!=(Arc const &a) const { return not(*this == a); }

  auto operator<(Arc const &a) const {
    return to < a.to || (to == a.to && cost < a.cost);
  }
  auto operator>(Arc const &a) const { return a < *this; }
  auto operator>=(Arc const &a) const { return not(*this < a); }
  auto operator<=(Arc const &a) const { return not(*this > a); }
};

template <typename T> struct Node {
  T value;
  std::vector<Arc> arcs;
  std::vector<Arc> revarcs;
  Node(T v) : value(v) {}

  Node &operator=(T const &a) {
    value = a;
    return *this;
  }
  operator T() const { return value; }
};

template <typename T, bool useRevarc = false> class Graph {
public:
  std::vector<Node<T>> nodes;
  T init;
  std::size_t const n;

  Graph(std::size_t N, T value) : nodes(N, value), init(value), n(N){};

  void addArc(std::size_t from, std::size_t to, long long cost) {
    assert(0 <= from && from <= n - 1 && 0 <= to && to <= n - 1);
    nodes[from].arcs.push_back({to, cost});
    if (useRevarc) {
      nodes[to].revarcs.push_back({from, cost});
    }
  }
  void addEdge(std::size_t a, std::size_t b, long long cost) {
    addArc(a, b, cost);
    addArc(b, a, cost);
  }
  Node<T> &operator[](std::size_t index) { return nodes[index]; }

  auto begin() { return nodes.begin(); }
  auto end() { return nodes.end(); }

  void reset(T init) {
    for (auto &x : nodes) {
      x.value = init;
    }
  }

  void reset(void) { reset(init); }

  void removeArc(std::size_t from, std::size_t to, long long cost) {
    auto removedArcIterator =
        find(nodes[from].arcs.begin(), nodes[from].arcs.end(), Arc{to, cost});
    removedArcIterator->to = -1;
    auto removedReversearcIterator =
        find(nodes[to].arcs.begin(), nodes[to].arcs.end(), Arc{from, cost});
    removedReversearcIterator->to = -1;
  }

  void removeArcEnsure() {
    for (auto &node : nodes) {
      node.arcs.erase(
          std::remove_if(node.arcs.begin(), node.arcs.end(),
                         [](auto const &arc) { return arc.to == -1; }),
          node.arcs.cend());
    }
  }

  void print(bool novalue = false, bool nocost = false) const {
    for (std::size_t i = 0; i < n; ++i) {
      auto const &node = nodes[i];
      std::cerr << i << ":\n";
      if (not novalue) {
        std::cerr << " value: " << node.value << "\n";
      }
      std::cerr << " arcs:"
                << "\n";
      for (auto const &arc : node.arcs) {
        std::cerr << "  - " << arc.to;
        if (not nocost) {
          std::cerr << "\t" << arc.cost;
        }
        std::cerr << "\n";
      }
    }
  }
};

struct GraphComponent : public Graph<std::size_t> {
  GraphComponent(std::size_t n)
      : Graph<std::size_t>(n, std::numeric_limits<std::size_t>::max()) {}
  auto run() {
    std::unordered_map<std::size_t, std::unordered_set<std::size_t>> components;
    std::unordered_set<std::size_t> rest;
    for (std::size_t i = 0; i < n; ++i) {
      rest.insert(i);
    }

    auto dfs = [&](auto currentNode, auto root, auto dfs) -> void {
      (*this)[currentNode].value = root;
      components[root].insert(currentNode);
      rest.erase(currentNode);

      auto const &arcs = (*this)[currentNode].arcs;
      for (auto const &arc : arcs) {
        if ((*this)[arc.to].value != std::numeric_limits<std::size_t>::max()) {
          continue;
        }
        dfs(arc.to, root, dfs);
      }
    };

    while (not rest.empty()) {
      dfs(*rest.begin(), *rest.begin(), dfs);
    }

    return components;
  }

  static void printComponents(
      std::unordered_map<std::size_t, std::unordered_set<std::size_t>> const
          &components) {
    for (auto &&i : components) {
      std::cerr << i.first << ": ";
      for (auto &&j : i.second) {
        std::cerr << j << " ";
      }
      std::cerr << "\n";
    }
  }
};

struct Tree : public Graph<std::size_t, true> {
public:
  Tree(std::size_t n) : Graph<std::size_t, true>(n, 0) {}
  void toTree(size_t root) {
    (*this)[root].value = 0;

    auto const &revarcs = (*this)[root].revarcs;
    while (revarcs.size())
      removeArc(root, revarcs.begin()->to, revarcs.begin()->cost);

    for (auto const &arc : (*this)[root].arcs) {
      toTree(arc.to, root, 1);
    }
  }

  void toTree(std::size_t root, std::size_t parent, std::size_t depth) {
    (*this)[root].value = depth;

    auto const &revarcs = (*this)[root].revarcs;
    while (revarcs.size())
      if (parent != revarcs.begin()->to)
        removeArc(root, revarcs.begin()->to, revarcs.begin()->cost);

    for (auto const &arc : (*this)[root].arcs) {
      toTree(arc.to, root, depth + 1);
    }
  }
};

class Bellmanford : public Graph<long long> {
public:
  Bellmanford(std::size_t N)
      : Graph(N, std::numeric_limits<long long>::max()) {}

  // Return false if there is negative loop
  bool run(std::size_t start) {
    reset();
    nodes[start].value = 0;

    std::unordered_set<std::size_t> froms = {start};
    auto r = [&](unsigned long long termination, auto r) {
      bool changed = false;
      std::unordered_set<std::size_t> nextFroms;
      for (auto &from : froms) {
        for (auto &i : nodes[from].arcs) {
          if (nodes[i.to].value > i.cost + nodes[from].value) {
            nodes[i.to].value = i.cost + nodes[from].value;
            nextFroms.insert(i.to);
            changed = true;
          }
        }
      }
      froms = nextFroms;

      if (!changed) {
        return true;
      }

      if (termination == 0) {
        // If the variable "changed" is true,
        // nagative loop exists.
        return not changed;
      }

      return r(termination - 1, r);
    };

    return r(n - 1, r);
  }
};

struct warshallFloyd : public Graph<long long> {
  static constexpr auto INF = std::numeric_limits<long long>::max();
  warshallFloyd(std::size_t n) : Graph<long long>(n, 0) {}

  auto run(void) {
    std::vector<std::vector<long long>> K(n, std::vector<long long>(n, INF));

    for (size_t i = 0; i < n; ++i) {
      K[i][i] = 0;
      auto const &arcs = (*this)[i].arcs;
      for (auto &x : arcs) {
        K[i][x.to] = std::min<long long>(K[i][x.to], x.cost);
      }
    }

    for (size_t k = 0; k < n; ++k) {
      for (size_t i = 0; i < n; ++i) {
        for (size_t j = 0; j < n; ++j) {
          if (K[i][k] == INF || K[k][j] == INF)
            continue;
          K[i][j] = std::min(K[i][j], K[i][k] + K[k][j]);
        }
      }
    }
    return K;
  }

  auto runWithPath(void) {
    std::vector<std::vector<std::pair<long long, std::list<long long>>>> K(
        n, std::vector<std::pair<long long, std::list<long long>>>(
               n, std::make_pair(INF, std::list<long long>())));

    for (size_t i = 0; i < n; ++i) {
      K[i][i].first = 0;
      auto const &arcs = (*this)[i].arcs;
      for (auto &x : arcs) {
        if (K[i][x.to].first > x.cost) {
          K[i][x.to].first = x.cost;
          K[i][x.to].second.push_back(x.to);
        }
      }
    }

    for (size_t k = 0; k < n; ++k) {
      for (size_t i = 0; i < n; ++i) {
        for (size_t j = 0; j < n; ++j) {
          if (K[i][k].first == INF || K[k][j].first == INF)
            continue;

          if (K[i][j].first > K[i][k].first + K[k][j].first) {
            K[i][j].first = K[i][k].first + K[k][j].first;
            K[i][j].second.assign(K[i][k].second.begin(), K[i][k].second.end());
            K[i][j].second.insert(K[i][j].second.end(), K[k][j].second.begin(),
                                  K[k][j].second.end());
          }
        }
      }
    }
    return K;
  }
};

constexpr long long warshallFloyd::INF;

template <typename T, typename Operator, bool debug = false>
struct SegmentTree {
  std::vector<T> v;
  std::size_t virtualSize;
  std::size_t width;
  std::size_t depth;
  T unitElement;
  Operator const op;

  std::size_t leftChildOf(std::size_t const parentSegment) const {
    assert(parentSegment <= width - 1);
    return 2 * (parentSegment + 1) - 1;
  }

  std::size_t rightChildOf(std::size_t const parentSegment) const {
    assert(parentSegment <= width - 1);

    return 2 * (parentSegment + 1);
  }

  std::size_t parentOf(std::size_t const childSegment) const {
    assert(childSegment != 0);
    assert(childSegment < v.size());

    return (childSegment - 1) / 2;
  }

  std::size_t positionToSegment(std::size_t const position) const {
    assert(position < virtualSize);
    return position + width - 1;
  }

  SegmentTree(std::vector<T> const &initialValues, T unitElement, Operator op)
      : virtualSize(initialValues.size()), width(1), depth(0),
        unitElement(unitElement), op(op) {
    assert(virtualSize != 0);

    while (width < initialValues.size()) {
      width <<= 1;
      ++depth;
    }

    v = std::vector<T>(2 * width - 1, unitElement);
    std::copy(initialValues.begin(), initialValues.end(),
              v.begin() + (width - 1));

    // Counting down cannot be used due to underflow
    for (std::size_t i = 1; i <= width - 1; ++i) {
      std::size_t const j = width - 1 - i;
      v[j] = op(v[leftChildOf(j)], v[rightChildOf(j)]);
    }

    // Output debug infomation
    if constexpr (debug) {
      std::cerr << "SegmentTreeSize: " << v.size() << "\n";
      std::cerr << "Width: " << width << "\n";
      std::cerr << "Depth: " << depth << "\n";
      outputVectorHorizontal(v);
    }
  }

public:
  // Get value on [l,r)
  T get(std::size_t const l, std::size_t const r) const {
    return get(l, r, 0, 0, width);
  }

  T get(std::size_t const position) { return v[positionToSegment(position)]; }

private:
  T get(std::size_t const l, std::size_t const r, std::size_t const segment,
        std::size_t const segmentL, std::size_t const segmentR) const {
    assert(l <= virtualSize);
    assert(r <= virtualSize);
    assert(segment < v.size());

    // Output debug infomation
    if constexpr (debug) {
      std::cerr << l << "/" << r << ":" << segmentL << "/" << segmentR << ":"
                << segment << "\n";
    }

    if (l <= segmentL && segmentR <= r) {
      return v[segment];
    }

    if (r <= segmentL || segmentR <= l) {
      return unitElement;
    }

    auto const segmentM = (segmentL + segmentR) / 2;
    return op(get(l, r, leftChildOf(segment), segmentL, segmentM),
              get(l, r, rightChildOf(segment), segmentM, segmentR));
  }

private:
  void updateSegment(std::size_t segment) {
    // Output debug information
    if constexpr (debug) {
      std::cerr << "Update segment: " << segment << "\n";
    }
    v[segment] = op(v[leftChildOf(segment)], v[rightChildOf(segment)]);

    if (segment != 0) {
      updateSegment(parentOf(segment));
    }
  }

public:
  void set(std::size_t const position, T const value) {
    auto const segment = positionToSegment(position);

    // Output debug information
    if constexpr (debug) {
      std::cerr << "Update position: " << position << " (segment: " << segment
                << ")"
                << "\n";
    }

    v[positionToSegment(position)] = value;
    updateSegment(parentOf(segment));

    // Output debug information
    if constexpr (debug) {
      outputVectorHorizontal(v);
    }
  }
};

//// Some shorthands

using namespace std;

using ll = long long;
using ull = unsigned long long;
using ld = long double;
using pq = priority_queue<ll>;
using pql = priority_queue<ll, vector<ll>, greater<ll>>;
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

//// Main is here!
signed main(void){
    cin.tie(0); ios::sync_with_stdio(false);cout<<fixed<<setprecision(10);

${cplusplus.read_input(data)}



    return 0;
}
