#define LISP_IMPLEMENTATION
#define LISP_DEFINE_MAIN
#define LISP_API static inline
#define LISP_EXTERN LISP_API
#include "lisp.h"

static lisp_cell_t *lisp_prim_assoc(lisp_t *l, lisp_cell_t *args, void *param) { 
	(void)param; 
	return lisp_assoc(l, lisp_car(l, args), lisp_car(l, lisp_cdr(l, args))); 
}

static lisp_cell_t *lisp_prim_eval(lisp_t *l, lisp_cell_t *args, void *param) { 
	(void)param; 
	return lisp_eval(l, 0, lisp_car(l, args), lisp_car(l, lisp_cdr(l, args)), l->depth + 1); 
}

static lisp_cell_t *lisp_prim_evlis(lisp_t *l, lisp_cell_t *args, void *param) { 
	(void)param; 
	return lisp_eval(l, 1, lisp_car(l, args), lisp_car(l, lisp_cdr(l, args)), l->depth + 1); 
}

static lisp_cell_t *lisp_prim_env(lisp_t *l, lisp_cell_t *args, void *param) { 
	if (lisp_asserts(l) < 0) return l->Error; 
	if (lisp_isnil(l, args)) return (lisp_cell_t*)param;
	lisp_cell_t *func = lisp_car(l, args);
	if (lisp_type_get(func) != LISP_FUNCTION) return l->Error;
	return lisp_procenv(func);
}

static lisp_cell_t *lisp_prim_gc(lisp_t *l, lisp_cell_t *args, void *param) {
	if (lisp_asserts(l) < 0) return l->Error; 
	(void)param;
	if (lisp_isnil(l, args)) {
		lisp_gc(l, 0);
		return l->gc ? l->Tee : l->Nil;
	}
	l->gc = !lisp_isnil(l, lisp_car(l, args));
	return l->gc ? l->Tee : l->Nil;
}

static lisp_cell_t *lisp_prim_scope(lisp_t *l, lisp_cell_t *args, void *param) {
	if (lisp_asserts(l) < 0) return l->Error; 
	(void)param;
	if (lisp_isnil(l, args))
		return l->dynamic_scope ? l->Tee : l->Nil;
	l->dynamic_scope = !lisp_isnil(l, lisp_car(l, args));
	return l->dynamic_scope ? l->Tee : l->Nil;
}

static lisp_cell_t *lisp_prim_depth(lisp_t *l, lisp_cell_t *args, void *param) { 
	(void)param; 
	(void)args;
	return lisp_mkint(l, l->depth);
}

static lisp_cell_t *lisp_prim_fatal(lisp_t *l, lisp_cell_t *args, void *param) {
	(void)param; 
	(void)args;
	l->fatal = 1;
	return l->Error;
}

static lisp_cell_t *lisp_prim_put(lisp_t *l, lisp_cell_t *args, void *param) {
	FILE *out = param;
	lisp_cell_t *v = lisp_car(l, args);
	const intptr_t p = lisp_ptrval(v) & 255;
	return fputc(p, out) != p ? l->Error: v;
}

static lisp_cell_t *lisp_prim_get(lisp_t *l, lisp_cell_t *args, void *param) {
	FILE *in = param;
	(void)args;
	return lisp_mkint(l, fgetc(in));
}

int lisp_extend(lisp_t *l) {
	if (lisp_asserts(l) < 0) return -1;
	if (lisp_function_add(l, "assoc", lisp_prim_assoc, NULL) < 0) goto fail;
	if (lisp_function_add(l, "eval", lisp_prim_eval, NULL) < 0) goto fail;
	if (lisp_function_add(l, "evlis", lisp_prim_evlis, NULL) < 0) goto fail;
	if (lisp_function_add(l, "env", lisp_prim_env, l->env) < 0) goto fail;
	if (lisp_function_add(l, "gc", lisp_prim_gc, NULL) < 0) goto fail;
	if (lisp_function_add(l, "scope", lisp_prim_scope, NULL) < 0) goto fail;
	if (lisp_function_add(l, "depth", lisp_prim_depth, NULL) < 0) goto fail;
	if (lisp_function_add(l, "fatal", lisp_prim_fatal, NULL) < 0) goto fail;
	if (lisp_function_add(l, "put", lisp_prim_put, stdout) < 0) goto fail;
	if (lisp_function_add(l, "get", lisp_prim_get, stdin) < 0) goto fail;
	return 0;
fail:
	l->fatal = 1;
	return -1;
}

