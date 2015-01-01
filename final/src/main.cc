#include <algorithm>
#include <arpa/inet.h>
#include <cassert>
#include <cinttypes>
#include <climits>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <err.h>
#include <errno.h>
#include <fcntl.h>
#include <ftw.h>
#include <getopt.h>
#include <map>
#include <netinet/in.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <type_traits>
#include <unistd.h>
#include <unordered_map>
#include <utility>
#include <vector>
using namespace std;
#define OS_LINUX
#include "ICTCLAS50.h"

typedef double ft;
typedef unsigned u;
typedef vector<int> VI;
typedef vector<VI> VVI;
typedef pair<int, int> PII;
typedef vector<PII> VPII;
typedef vector<u> VU;
typedef vector<VU> VVU;
typedef pair<u, u> PUU;
typedef vector<PUU> VPUU;
typedef vector<string> VS;
#define FOR(i, a, b) for (remove_cv<decltype(b)>::type i = (a); i < (b); i++)
#define REP(i, n) FOR(i, 0, n)
#define REP1(i, n) for (decltype(n) i = 1; i <= (n); i++)
#define mp make_pair
#define pb push_back
#define eb emplace_back
#define ALL(x) (x).begin(), (x).end()
#define fi first
#define se second

const int NB = 50000017;
const int CAP = 20000003;
const int BASE = 31;

const char MIRROR_PREFIX[] = "html";
const ft THRESHOLD = 0.6;
const int PORT = 1337;
const int NRESULTS = 10;
const int MAX_RESULTS = 100;
const ft BM25_K1 = 1.2, BM25_B = 0.75;

#define B_RABIN_KARP(str, n, i)    \
  {                                \
    u h = 0;                       \
    REP(i, q-1)                    \
      h = h * BASE + str[i];       \
    if (n >= q-1)                  \
      REP(i, n-(q-1)) {            \
        h = h * BASE + str[i+q-1];

#define E_RABIN_KARP(str, n, i)    \
        h -= q_pow_p1 * str[i];    \
      }                            \
  }

template<u NB>
struct IdAllocator
{
  u q, cap, size = 0;
  int *b;
  struct Node { int next; u k; const char *kk; } *a;

  IdAllocator(u q, u cap) : q(q), cap(cap), b(new int[NB]), a((Node *)malloc(sizeof(Node)*cap)) {
    fill_n(b, NB, -1);
  }

  ~IdAllocator() {
    delete[] b;
    free(a);
  }

  u hash(u k) const {
    return k % NB;
  }

  int get(u k, const char *kk) {
    u h = hash(k);
    int i;
    for (i = b[h]; i != -1; i = a[i].next)
      if (a[i].k == k && ! memcmp(a[i].kk, kk, q))
        return i;
    return -1;
  }

  int find(u k, const char *kk) {
    u h = hash(k);
    int i;
    for (i = b[h]; i != -1; i = a[i].next)
      if (a[i].k == k && ! memcmp(a[i].kk, kk, q))
        return i;

    if (size == cap) {
      cap <<= 1;
      a = (Node *)realloc(a, sizeof(Node)*cap);
    }

    i = size++;
    a[i].k = k;
    a[i].kk = kk;
    a[i].next = b[h];
    b[h] = i;
    return i;
  }
};

template<typename T, typename Cmp>
struct Heap
{
  void down(u x) {
    T key = a[x];
    for (u y; y = 2*x+1, y < n; x = y) {
      if (y+1 < n && cmp(a[y+1], a[y])) y++;
      if (! cmp(a[y], key)) break;
      a[x] = a[y];
    }
    a[x] = key;
  }

  void up(u x) {
    T key = a[x];
    for (; x && Cmp()(key, a[(x-1)/2]); x = (x-1)/2)
      a[x] = a[(x-1)/2];
    a[x] = key;
  }

  void make() {
    for (u i = n/2; i > 0; )
      down(--i);
  }

  template<typename... Args>
  void emplace_back(Args&&... args) {
    a.eb(forward<Args>(args)...);
    n++;
  }

  void push_back(const T &v) {
    a.pb(v);
    n++;
  }

  u size() const {
    return a.size();
  }

  bool empty() const {
    return a.empty();
  }

  void pop() {
    a[0] = a.back();
    a.pop_back();
    n--;
    down(0);
  }

  void pop_to_back() {
    swap(a[0], a[--n]);
    down(0);
  }

  void emplace() {
    n++;
    up(n-1);
  }

  T &operator[](u x) {
    return a[x];
  }

  vector<T> a;
  Cmp cmp;
  u n = 0;
};

u ipow(u a, u n)
{
  u s = 1;
  for (; n; n >>= 1) {
    if (n & 1)
      s *= a;
    a *= a;
  }
  return s;
}

enum FileType { HTML, DOC, PDF };
enum ResultType { EXACT, FUZZY };

unordered_map<string, ft> uri2pagerank;
ft default_pagerank;

class Searcher
{
public:
  struct Compare
  {
    bool operator()(const pair<int*,int> &a, const pair<int*,int> &b) const {
      return *a.fi < *b.fi;
    }
  };
  struct Compare2
  {
    bool operator()(const pair<PII*,vector<PII>*> &a, const pair<PII*,vector<PII>*> &b) const {
      return *a.fi < *b.fi;
    }
  };
  struct SimResult { int did, ed, pos, len; ft score; };
  struct Doc {
    char *title, *uri;
    ft pagerank;
    FileType filetype;
    char *content;
    int len;
    int nwords;
  };
  IdAllocator<NB> *s2gid;
  vector<Doc> d;
  int q = 3, q_pow_p1 = ipow(BASE, q-1);
  VVI g2d;
  VI freqs;
  vector<bool> ticks;
  bool flip;
  int &freq(int id) {
    if (ticks[id] != flip) {
      ticks[id] = flip;
      freqs[id] = 0;
    }
    return freqs[id];
  }
  int maxL = 0;
  int **seller_dp, **seller_tb;

  unordered_map<string, VPII> w2d;
  ft avg_nwords;

  Searcher() {
    s2gid = new IdAllocator<NB>(q, CAP);
    if (! ICTCLAS_Init())
      errx(3, "failed to initialize ICTCLAS");
    ICTCLAS_SetPOSmap(2);
  }
  void add_html(const char *path) {
    add(path, HTML);
  }
  void add_doc(const char *path) {
    add(path, DOC);
  }
  void add_pdf(const char *path) {
    add(path, PDF);
  }
  void add(const char *path, FileType filetype) {
    int did = d.size();
    Doc doc;
    doc.filetype = filetype;
    doc.uri = strdup(path+sizeof(MIRROR_PREFIX));

    // pagerank
    if (uri2pagerank.count(doc.uri))
      doc.pagerank = uri2pagerank[doc.uri];
    else
      doc.pagerank = default_pagerank;

    // len & content
    struct stat sb;
    int fd = open(path, O_RDONLY);
    if (fd == -1 || fstat(fd, &sb) == -1)
      errx(1, "fail to open %s: %s", path, strerror(errno));
    doc.len = int(sb.st_size);
    doc.content = (char *)malloc(sb.st_size);
    if (read(fd, doc.content, doc.len) != doc.len)
      errx(1, "fail to read %s", path);
    close(fd);

    // sentence for word segmentation
    char *sentence = (char *)malloc(doc.len*2);
    ICTCLAS_ParagraphProcess(doc.content, doc.len, sentence, CODE_TYPE_UTF8, 0);
    int nn = int(strlen(sentence));
    doc.nwords = 0;
    for (int i = 0, j; i < nn; i = j) {
      for (; i < nn && isspace(sentence[i]); i++);
      for (j = i; j < nn && ! isspace(sentence[j]); j++);
      if (i < j) {
        VPII &ds = w2d[string(sentence+i, j-i)];
        if (ds.empty() || ds.back().fi != did)
          ds.pb(mp(did, 1));
        else
          ds.back().se++;
        doc.nwords++;
      }
    }
    free(sentence);

    // split title & content
    char *sep = strchr(doc.content, '\n');
    if (sep) {
      doc.title = doc.content;
      *sep = '\0';
      doc.len -= sep+1-doc.content;
      doc.content = sep+1;
    } else
      doc.title = doc.uri;
    d.pb(doc);
    maxL = max(maxL, doc.len);

    B_RABIN_KARP(doc.content, doc.len, i)
      int ngid = s2gid->size, gid = s2gid->find(h, doc.content+i);
      if (gid == ngid) {
        g2d.emplace_back();
        g2d[gid].pb(did);
      } else {
        if (g2d[did].size() && g2d[gid].back() != did)
          g2d[gid].pb(did);
      }
    E_RABIN_KARP(doc.content, doc.len, i)
  }
  void end() {
    for (auto &d: g2d)
      d.pb(INT_MAX);
    freqs.assign(max(u(d.size()), s2gid->size), 0);
    ticks.assign(max(u(d.size()), s2gid->size), 0);
    flip = false;
    seller_dp = new int*[2];
    seller_dp[0] = new int[maxL+1];
    seller_dp[1] = new int[maxL+1];
    seller_tb = new int*[2];
    seller_tb[0] = new int[maxL+1];
    seller_tb[1] = new int[maxL+1];

    for (auto &d: w2d)
      d.se.pb(mp(INT_MAX, 0));
    avg_nwords = 0.0;
    for (auto &dd: d)
      avg_nwords += dd.nwords;
    avg_nwords /= max(int(d.size()), 1);
    puts("+ end");
  }

  int overlap(const char *a, int m, const char *b, int n)
  {
    flip = ! flip;
    B_RABIN_KARP(a, m, i)
      int id = s2gid->get(h, &a[i]);
      if (id != -1)
        freq(id)++;
    E_RABIN_KARP(a, m, i)

    int o = 0;
    B_RABIN_KARP(b, n, i)
      int id = s2gid->get(h, &b[i]);
      if (id != -1 && freq(id) > 0) {
        freq(id)--;
        o++;
      }
    E_RABIN_KARP(b, n, i)
    return o;
  }

  template<typename T>
  int send(int fd, T v) {
    return send(fd, (char*)&v, sizeof(T));
  }

  int send(int fd, const char *buf, int rest) {
    int nsent = 0;
    while (rest > 0) {
      int r = write(fd, buf+nsent, rest);
      if (r < 0) return -1;
      nsent += r;
      rest -= r;
    }
    return nsent;
  }

  int send_content(int fd, SimResult res) {
    const int MARGIN = 20;
    const unsigned char *doc = (const unsigned char *)d[res.did].content;
    int len = d[res.did].len, size;
    bool left_ellipsis = false, right_ellipsis = false;
    int l = res.pos, r = res.pos+res.len, ll = 0, rr = 0;

    while (l < r && 0x80 <= doc[l] && doc[l] < 0xc0) {
      l++;
      ll++;
    }
    REP(i, MARGIN)
      while (l > 0 && 0x80 <= doc[--l] && doc[l] < 0xc0);
    if (l > 0)
      left_ellipsis = true;
    while (l < r && (0x80 <= doc[r] && doc[r] < 0xc0)) {
      r--;
      rr--;
    }
    REP(i, MARGIN)
      while (r < len && (r++, 0x80 <= doc[r] && doc[r] < 0xc0));
    if (r < len)
      right_ellipsis = true;
    size = r-l+(left_ellipsis?3:0)+(right_ellipsis?3:0);

    if (send(fd, size) < 0) return -1;
    if (left_ellipsis && send(fd, "...", 3) < 0) return -1;
    if (send(fd, (char *)doc+l, r-l) < 0) return -1;
    if (right_ellipsis && send(fd, "...", 3) < 0) return -1;

    int pos = res.pos-l+(left_ellipsis?3:0);
    if (send(fd, pos+ll) < 0) return -1;
    if (send(fd, res.len+rr-ll) < 0) return -1;
    return 0;
  }

  void minimum_window(const char *doc, int n, vector<string> words, SimResult &res)
  {
    vector<int> c(n, 0);

    int j = 0, num = 0, span = 0;
    res.pos = 0;
    res.len = n;
    for (int i = 0, j = 0; i < n; i++) {
      for (; j < n && num < words.size(); j++)
        REP(k, words.size()) {
          if (! strncmp(doc+j, words[k].c_str(), words[k].size())) {
            int &cnt = c[k];
            if (! cnt++) num++;
            span = max(span, int(j+words[k].size()));
            break;
          }
        }
      if (num < words.size())
        break;
      if (span-i < res.len) {
        res.pos = i;
        res.len = span-i;
      }
      REP(k, words.size()) {
        if (! strncmp(doc+i, words[k].c_str(), words[k].size())) {
          int &cnt = c[k];
          if (! --cnt) num--;
          break;
        }
      }
    }
  }

  int exact_search(const char *query, int n, int offset, vector<SimResult> &result) {
    Heap<pair<PII*,vector<PII>*>, Compare2> L;
    char *sentence = (char *)malloc(n*2);
    ICTCLAS_ParagraphProcess(query, n, sentence, CODE_TYPE_UTF8, 0);
    int nn = int(strlen(sentence));
    VS words, words2;
    for (int i = 0, j; i < nn; i = j) {
      for (; i < nn && isspace((unsigned char)sentence[i]); i++);
      for (j = i; j < nn && ! isspace((unsigned char)sentence[j]); j++);
      if (i < j)
        words.eb(sentence+i, j-i);
    }
    free(sentence);

    for (auto &s: words) {
      bool flag = true;
      for (auto &ss: words)
        if (s.size() < ss.size() && ss.find(s) != string::npos) {
          flag = false;
          break;
        }
      if (flag)
        words2.pb(s);
    }
    words.swap(words2);

    for (auto &s: words)
      if (w2d.count(s))
        L.pb(mp(&w2d[s][0], &w2d[s]));
      else
        return 0;

    flip = ! flip;
    for (auto x: L.a)
      freq(x.fi->fi)++;
    while (L[0].fi->fi < INT_MAX) {
      if (freq(L[0].fi->fi) == L.size()) {
        SimResult res;
        res.did = L[0].fi->fi;
        res.score = 0;
        REP(i, L.size()) {
          int ndocs = L[i].se->size(), freq = L[i].fi->se;
          res.score += log((d.size()-n+0.5)/(n+0.5)) * (freq*(BM25_K1+1)/(freq+BM25_K1*(1-BM25_B+BM25_B*d[res.did].nwords/avg_nwords)));
        }
        res.score *= d[res.did].pagerank;
        result.pb(res);
      }

      int t = L[0].fi->fi;
      while (L[0].fi->fi == t) {
        int old = L[0].fi++->fi;
        freq(old)--;
        if (L[0].fi->fi < INT_MAX) {
          freq(L[0].fi->fi)++;
          L.down(0);
        } else
          goto break_loop;
      }

      FOR(i, 1, L.size()) {
        if (L[0].fi->fi < INT_MAX)
          freq(L[0].fi->fi)--;
        L.pop_to_back();
      }

      t = L[0].fi->fi;
      FOR(i, 1, L.size()) {
        PII *l = L[i].fi, *h = &(*L[i].se)[0]+L[i].se->size();
        while (l < h) {
          PII *x = l + ((h-l)>>1);
          if (x->fi < t) l = x+1;
          else h = x;
        }
        L[i].fi = l;
        if (L[i].fi->fi < INT_MAX)
          freq(L[i].fi->fi)++;
        L.emplace();
      }
    }
break_loop:

    sort(ALL(result), [](const SimResult &a, const SimResult &b) {
      return a.score > b.score;
    });
    if (result.size() > MAX_RESULTS)
      result.resize(MAX_RESULTS);
    int ret = result.size();
    if (offset+NRESULTS < result.size())
      result.resize(offset+NRESULTS);
    result.erase(result.begin(), result.begin()+offset);
    for (auto &r: result)
      minimum_window(d[r.did].content, d[r.did].len, words, r);
    return ret;
  }

  void run_query(const char *query, bool fuzzy, ft threshold, int offset, int fd) {
    const char *filetype2s[3];
    filetype2s[HTML] = "html";
    filetype2s[DOC] = "doc";
    filetype2s[PDF] = "pdf";
    vector<SimResult> result;

    int n = strlen(query), ng = n-q+1, size = 0;
    if (fuzzy) {
      int overlap = max(int(ceil(threshold*ng)), 1);
      if (ng > 0)
        size = sim_search(query, overlap, offset, result);
    } else {
      size = exact_search(query, n, offset, result);
    }
    if (send(fd, size) < 0) return;
    if (send(fd, int(result.size())) < 0) return;
    if (send(fd, int(fuzzy ? FUZZY : EXACT)) < 0) return;
    for (auto &r: result) {
      if (fuzzy) {
        if (send(fd, r.ed) < 0) return;
      } else {
        if (send(fd, r.score) < 0) return;
      }
      if (send(fd, int(d[r.did].filetype)) < 0) return;
      int len = int(strlen(d[r.did].uri));
      if (send(fd, len) < 0) return;
      if (send(fd, d[r.did].uri, len) < 0) return;
      len = int(strlen(d[r.did].title));
      if (send(fd, len) < 0) return;
      if (send(fd, d[r.did].title, len) < 0) return;
      if (send_content(fd, r) < 0) return;
    }
  }

  // a: needle
  // b: haystack
  void seller(const char *a, int m, SimResult &res) {
    const char *b = d[res.did].content;
    int n = d[res.did].len, ** const f = seller_dp, ** const g = seller_tb;
    REP(i, n+1) {
      f[0][i] = 0;
      g[0][i] = i;
    }
    REP1(i, m) {
      f[i&1][0] = i;
      g[i&1][0] = 0;
      REP1(j, n)
        if (a[i-1] == b[j-1]) {
          f[i&1][j] = f[i-1&1][j-1];
          g[i&1][j] = g[i-1&1][j-1];
        } else {
          f[i&1][j] = f[i-1&1][j];
          g[i&1][j] = g[i-1&1][j];
          if (f[i-1&1][j-1]+1 < f[i&1][j]) {
            f[i&1][j] = f[i-1&1][j-1];
            g[i&1][j] = g[i-1&1][j-1];
          }
          if (f[i&1][j-1]+1 < f[i&1][j]) {
            f[i&1][j] = f[i&1][j-1];
            g[i&1][j] = g[i&1][j-1];
          }
          f[i&1][j]++;
        }
    }
    res.ed = m;
    res.pos = res.len = 0;
    REP1(j, n)
      if (f[m&1][j] < res.ed) {
        res.ed = f[m&1][j];
        res.pos = g[m&1][j];
        res.len = j-g[m&1][j];
      }
  }

  int sim_search(const char *query, int overlap, int offset, vector<SimResult> &result) {
    int n = int(strlen(query));
    Heap<pair<int*,int>, Compare> L;
    B_RABIN_KARP(query, n, i)
      int gid = s2gid->get(h, query+i);
      if (gid != -1)
        L.pb(mp(&g2d[gid][0], gid));
    E_RABIN_KARP(query, n, i)

    if (L.size() < overlap)
      return 0;

    flip = ! flip;
    for (auto x: L.a)
      freq(*x.fi)++;
    while (*L[0].fi < INT_MAX) {
      if (freq(*L[0].fi) >= overlap) {
        SimResult res;
        res.did = *L[0].fi;
        res.ed = - freq(*L[0].fi);
        //seller(query, n, res);
        result.pb(res);
      }

      int t = *L[0].fi;
      while (*L[0].fi == t) {
        int old = *L[0].fi++;
        freq(old)--;
        if (*L[0].fi < INT_MAX) {
          freq(*L[0].fi)++;
          L.down(0);
        } else
          L.pop();
      }
      if (L.size() < overlap) break;

      REP(i, overlap-1) {
        if (*L[0].fi < INT_MAX)
          freq(*L[0].fi)--;
        L.pop_to_back();
      }

      t = *L[0].fi;
      FOR(i, L.size()-(overlap-1), L.size()) {
        int *l = L[i].fi, *h = &g2d[L[i].se][0]+g2d[L[i].se].size();
        while (l < h) {
          int *x = l + ((h-l)>>1);
          if (*x < t) l = x+1;
          else h = x;
        }
        L[i].fi = l;
        if (*L[i].fi < INT_MAX)
          freq(*L[i].fi)++;
        L.emplace();
      }
    }

    if (result.size() >= MAX_RESULTS) {
      // ed = - freq
      sort(ALL(result), [&](const SimResult &a, const SimResult &b) {
        if (a.ed != b.ed) return a.ed < b.ed;
        return d[a.did].pagerank < d[b.did].pagerank;
      });
      result.resize(MAX_RESULTS);
    }
    int ret = result.size();
    if (offset+NRESULTS < result.size())
      result.resize(offset+NRESULTS);
    result.erase(result.begin(), result.begin()+offset);
    for (auto &r: result)
      seller(query, n, r);
    sort(ALL(result), [&](const SimResult &a, const SimResult &b) {
      if (a.ed != b.ed) return a.ed < b.ed;
      return d[a.did].pagerank < d[b.did].pagerank;
    });
    return ret;
  }
} searcher;

int walk(const char *fpath, const struct stat *sb, int tflag)
{
  static int cnt = 0, html_cnt = 0, doc_cnt = 0, pdf_cnt = 0;
  if (tflag == FTW_F) {
    const char *p = strrchr(fpath, '.');
    if (p) {
      if (! strcmp(p+1, "html") || ! strcmp(p+1, "htm")) {
        html_cnt++;
        searcher.add_html(fpath);
      } else if (! strcmp(p+1, "doc")) {
        doc_cnt++;
        searcher.add_doc(fpath);
      } else if (! strcmp(p+1, "pdf")) {
        pdf_cnt++;
        searcher.add_pdf(fpath);
      }
    }
  }
  if (++cnt%1000 == 0)
    printf("+++ all:%d html:%d pdf:%d doc:%d\n", cnt, html_cnt, pdf_cnt, doc_cnt);
  return 0;
}

const char *argv0;

void print_help(FILE *h)
{
  fprintf(h, "Usage: %s [OPTIONS]\n", argv0);
  fprintf(h, "\n");
  fprintf(h, "Options:\n");
  fprintf(h, " -p, --pagerank FILE     pagerank file\n");
}

int main(int argc, char *argv[])
{
  argv0 = argv[0];

  int opt;
  static struct option long_options[] = {
    {"pagerank",    required_argument,      NULL,   'p'},
    {"help",        no_argument,            NULL,   'h'},
    {0,             0,                      0,      0}
  };

  default_pagerank = 1.0;
  while ((opt = getopt_long(argc, argv, "hp:", long_options, NULL)) != -1) {
    switch (opt) {
    case 'p': {
      char uri[1024];
      char *line = NULL;
      size_t len = 0;
      ft pagerank;
      FILE *f = fopen(optarg, "r");
      if (! f)
        errx(1, "failed to open %s: %s", optarg, strerror(errno));
      uri2pagerank.clear();
      default_pagerank = 1.0;
      while (getline(&line, &len, f) != -1) {
        if (sscanf(line, "%lf%s", &pagerank, uri) != 2)
          errx(1, "2 arguments");
        string u(uri);
        if (u.substr(0, 7) == "http://")
          u = u.substr(7);
        if (u.back() == '/')
          u += "index.html";
        uri2pagerank[u] = pagerank;
        default_pagerank = min(default_pagerank, pagerank);
      }
      printf("read %zd pagerank\n", uri2pagerank.size());
      fclose(f);
      break;
    }
    case 'h':
      print_help(stdout);
      return 0;
    }
  }

  int yes = 1, sockfd = socket(AF_INET, SOCK_STREAM, 0);
  if (sockfd == -1)
    errx(2, "failed to socket: %s", strerror(errno));
  if (setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof yes) == -1)
    errx(2, "failed to setsocket: %s", strerror(errno));
  struct sockaddr_in sin;
  sin.sin_family = AF_INET;
  sin.sin_addr.s_addr = INADDR_ANY;
  sin.sin_port = htons(PORT);
  if (bind(sockfd, (const struct sockaddr *)&sin, sizeof sin) == -1)
    errx(2, "failed to bind: %s", strerror(errno));
  if (listen(sockfd, 1) == -1)
    errx(2, "failed to listen: %s", strerror(errno));

  ftw(MIRROR_PREFIX, walk, 20);
  searcher.end();

  for(;;) {
    struct sockaddr_storage ss;
    socklen_t sslen = sizeof ss;
    int clifd = accept(sockfd, (struct sockaddr *)&ss, &sslen);
    if (clifd == -1) break;
    pid_t pid = fork();
    if (pid == -1) break;
    if (pid == 0) {
      char querytype;
      int offset;
      char query[128];
      alarm(60);
      FILE *f = fdopen(clifd, "r+");
      if (! fgets(query, sizeof query, f) || sscanf(query, "%c", &querytype) != 1)
        goto err;
      if (! fgets(query, sizeof query, f) || sscanf(query, "%d", &offset) != 1)
        goto err;
      if (! fgets(query, sizeof query, f))
        goto err;
      searcher.run_query(query, querytype == 'f', THRESHOLD, offset, clifd);
err:
      fclose(f);
    }
    close(clifd);
  }
  return 0;
}
