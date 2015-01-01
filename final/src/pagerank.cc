#include <algorithm>
#include <cstdio>
#include <map>
#include <stack>
#include <string.h>
#include <string>
#include <type_traits>
#include <unistd.h>
#include <vector>
using namespace std;
#define FOR(i, a, b) for (remove_reference<remove_cv<decltype(b)>::type>::type i = (a); i < (b); i++)
#define REP(i, n) FOR(i, 0, n)
#define ALL(x) (x).begin(), (x).end()

typedef double ft;


int main(int argc, char *argv[])
{
  char *line = NULL;
  size_t len = 0;
  int n;
  scanf("%d\n", &n);
  vector<vector<int> > e(n);
  while (getline(&line, &len, stdin) != -1) {
    int u, v;
    sscanf(line, "%d %d", &u, &v);
    e[u].push_back(v);
  }

  ft residual = 0.85;
  vector<ft> rank(n, 1.0/n), rank2(n);
  rank.assign(n, 1.0/n);
  rank.assign(n, 0.0);
  REP(iter, 50) {
    ft deadend = 1.0-residual;
    fill_n(rank2.begin(), n, 0.0);
    REP(u, n)
      if (e[u].empty())
        deadend += residual*rank[u];
      else {
        ft w = residual/e[u].size();
        for (auto &v: e[u])
          rank2[v] += rank[u]*w;
      }
    REP(u, n)
      rank2[u] += deadend / n;
    rank.swap(rank2);
  }
  free(line);

  ft sum = 0;
  printf("%lf\n", sum);
  REP(i, n)
    printf("%lf\n", rank[i]);
}
