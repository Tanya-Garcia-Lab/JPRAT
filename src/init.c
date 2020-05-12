#include <R_ext/RS.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/*
The following name(s) appear with different usages
e.g., with different numbers of arguments:

kmjack

This needs to be resolved in the tables and any declarations.
*/

/* FIXME:
Check these declarations against the C/Fortran source code.
*/

/* .Fortran calls */
extern void F77_NAME(get_count_new)(void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(get_count_outside)(void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(kmjack)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(kmjack)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);

static const R_FortranMethodDef FortranEntries[] = {
  {"get_count_new",     (DL_FUNC) &F77_NAME(get_count_new),      7},
  {"get_count_outside", (DL_FUNC) &F77_NAME(get_count_outside),  7},
  {"kmjack",            (DL_FUNC) &F77_NAME(kmjack),            15},
  {"kmjack",            (DL_FUNC) &F77_NAME(kmjack),            16},
  {NULL, NULL, 0}
};

void R_init_JPRAT(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, NULL, FortranEntries, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
