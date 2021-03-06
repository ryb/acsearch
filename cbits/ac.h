
#ifndef _AC_H_
#define _AC_H_

typedef struct actreenode {
  char ch;
  int matchid;
  struct actreenode *outlink, *faillink;
  struct actreenode *children, *sibling;
} ACTREE_NODE, *AC_TREE;


typedef struct {
  AC_TREE tree;
  int ispreprocessed, errorflag;

  int Psize;
  int *Plengths;

  char *T;
  int N, c, initflag, endflag;
  AC_TREE w, output;

  int prep_new_edges, prep_old_edges, prep_fail_compares;
  int num_compares, num_failures, edges_traversed, outlinks_traversed;
} AC_STRUCT;


AC_STRUCT *ac_alloc(void);
AC_STRUCT *ac_shallow_cpy(AC_STRUCT *node);
int ac_add_string(AC_STRUCT *node, char *P, int M, int id);
int ac_del_string(AC_STRUCT *node, char *P, int M, int id);
int ac_prep(AC_STRUCT *node);
void ac_search_init(AC_STRUCT *node, char *T, int N);
char *ac_search(AC_STRUCT *node, int *match_start, int *length_out, int *id_out);
void ac_cpy_free(AC_STRUCT *node);
void ac_free(AC_STRUCT *node);

#endif
