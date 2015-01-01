#include <algorithm>
#include <cstdio>
#include <err.h>
#include <errno.h>
#include <functional>
#include <getopt.h>
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

const char *argv0;

void print_help(FILE *h)
{
  fprintf(h, "Usage: %s [OPTIONS] node_map graph\n", argv0);
  fprintf(h, "\n");
  fprintf(h, "Options:\n");
  fprintf(h, " -i, --niter NITER     number of iterations (default: 30)\n");
  fprintf(h, " -r, --residual NITER  residual probability (default: 0.85)\n");
  fprintf(h, " -s, --scc             strongly-connected components\n");
  fprintf(h, " -w, --wcc             weakly-connected components\n");
}

vector<int> outd, from, to, ends, djs;

vector<ft> page_rank(int n, int niter, ft residual)
{
  vector<ft> rank(n, 1.0/n), rank2(n);
  rank.assign(n, 1.0/n);
  rank.assign(n, 0.0);
  REP(iter, niter) {
    ft deadend = 1.0-residual;
    fill_n(rank2.begin(), n, 0.0);
    REP(u, n)
      if (from[u] == to[u])
        deadend += residual*rank[u];
      else {
        ft w = residual/(to[u]-from[u]);
        FOR(i, from[u], to[u]) {
          int v = ends[i];
          rank2[v] += rank[u]*w;
        }
      }
    REP(u, n)
      rank2[u] += deadend / n;
    rank.swap(rank2);
  }
  return rank;
}

vector<int> dfn, block, block_size;
int nblock, tick;

int tarjan_scc(int u)
{
  static stack<int> st;
  dfn[u] = tick;
  int low = tick++;
  st.push(u);
  FOR(i, from[u], to[u]) {
    int v = ends[i];
    if (dfn[v] == -1)
      low = min(low, tarjan_scc(v));
    else if (block[v] == -1)
      low = min(low, dfn[v]);
  }
  if (dfn[u] == low) {
    int v;
    block_size[nblock] = 0;
    do {
      v = st.top();
      block[v] = nblock;
      block_size[nblock]++;
      st.pop();
    } while (v != u);
    nblock++;
  }
  return low;
}

int find(int x)
{
  while (djs[x] >= 0) {
    if (djs[djs[x]] >= 0)
      djs[x] = djs[djs[x]];
    x = djs[x];
  }
  return x;
}

void union_(int x, int y)
{
  x = find(x);
  y = find(y);
  if (x != y) {
    if (djs[x] > djs[y]) swap(x, y);
    djs[x] += djs[y];
    djs[y] = x;
  }
}

int main(int argc, char *argv[])
{
  int opt;
  int niter = 30;
  ft residual = 0.85;
  enum Mode { PAGE_RANK, SCC, WCC } mode = PAGE_RANK;

  argv0 = argv[0];
  static struct option long_options[] = {
    {"niter",       required_argument,      NULL,   'i'},
    {"residual",    required_argument,      NULL,   'r'},
    {"wcc",         no_argument,            NULL,   'w'},
    {"scc",         no_argument,            NULL,   's'},
    {"help",        no_argument,            NULL,   0},
    {0,             0,                      0,      0}
  };

  while ((opt = getopt_long(argc, argv, "hi:r:ws", long_options, NULL)) != -1) {
    switch (opt) {
    case 'i':
      niter = atoi(optarg);
      if (niter <= 0) {
        fprintf(stderr, "niter must be greater than 0\n");
        return 1;
      }
      break;
    case 'r':
      residual = atof(optarg);
      if (residual <= 0 || residual > 1) {
        fprintf(stderr, "residual must be in (0,1]\n");
        return 1;
      }
      break;
    case 's':
      mode = SCC;
      break;
    case 'w':
      mode = WCC;
      break;
    case 'h':
      print_help(stdout);
      return 0;
    }
  }

  if (argc-optind != 2) {
    fprintf(stderr, "node_map and graph files must be specified\n");
    print_help(stderr);
    return 1;
  }

  FILE *f0 = fopen(argv[optind], "r");
  if (! f0) {
    fprintf(stderr, "fopen %s: %s\n", argv[1], strerror(errno));
    return 2;
  }
  FILE *f1 = fopen(argv[optind+1], "r");
  if (! f1) {
    fprintf(stderr, "fopen %s: %s\n", argv[2], strerror(errno));
    return 2;
  }

  map<string, int> name2wid;
  map<int, int> wid2id;
  map<int, string> id2name;
  int u, v;
  char c;
  char *line = NULL;
  size_t len = 0;

  while (getline(&line, &len, f0) != -1) {
    char *p = strstr(line, "-->");
    if (p) {
      *p = '\0';
      sscanf(p+3, "%d", &v);
      name2wid[line] = v;
      int id = (int)wid2id.size();
      wid2id[v] = id;
      id2name[id] = line;
    }
  }
  fclose(f0);
  free(line);

  int n = (int)wid2id.size();
  outd.assign(n, 0);
  from.assign(n, -1);
  to.assign(n, -1);
  if (mode == WCC)
    djs.assign(n, -1);

  while (fscanf(f1, "%d%c", &v, &c) == 2)
    if (c == ':') {
      u = wid2id[v];
      from[u] = to[u] = ends.size();
    } else {
      v = wid2id[v];
      ends.push_back(v);
      to[u] = ends.size();
      if (mode == WCC)
        union_(u, v);
    }
  fclose(f1);

  switch (mode) {
  case PAGE_RANK:
    {
      vector<ft> rank = page_rank(n, niter, residual);
      vector<int> pi(n);
      REP(i, n)
        pi[i] = i;
      sort(ALL(pi), [&](int x, int y) { return rank[x] > rank[y]; });

      REP(i, n) {
        int id = pi[i];
        printf("%s\t%lg\n", id2name[id].c_str(), rank[id]);
      }
    }
    break;

  case SCC:
    {
      dfn.assign(n, -1);
      block.assign(n, -1);
      block_size.assign(n, 0);
      tick = nblock = 0;
      REP(i, n)
        if (dfn[i] == -1)
          tarjan_scc(i);
      REP(i, nblock)
        printf("%d\n", block_size[i]);
    }
    break;

  case WCC:
    {
      REP(i, n)
        if (djs[i] < 0)
          printf("%d\n", -djs[i]);
    }
    break;
  }
}
