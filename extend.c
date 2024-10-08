#define LISP_IMPLEMENTATION
#define LISP_DEFINE_MAIN
#define LISP_API static inline
#define LISP_EXTERN LISP_API
#include "lisp.h"
#include <time.h>
#include <signal.h>

static lisp_cell_t *lisp_prim_gc(lisp_t *l, lisp_cell_t *args, void *param) {
	if (lisp_asserts(l) < 0) return l->Error; 
	(void)param;
	if (lisp_isnil(l, args)) { /* we could set the collection period */
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
	lisp_cell_t *r = fputc(p, out) != p ? l->Error: v;
	if (fflush(out) < 0) r = l->Error;
	return r;
}

static lisp_cell_t *lisp_prim_get(lisp_t *l, lisp_cell_t *args, void *param) {
	FILE *in = param;
	(void)args;
	return lisp_mkint(l, fgetc(in));
}

static lisp_cell_t *lisp_prim_getenv(lisp_t *l, lisp_cell_t *args, void *param) {
	(void)param;
	lisp_cell_t *str = lisp_car(l, args);
	if (lisp_type_get(str) != LISP_SYMBOL) return l->Error;
	char *s = getenv(str->t[0].s);
	if (!s || !s[0]) return l->Nil;
	return lisp_intern(l, s);
}

static lisp_cell_t *lisp_prim_save(lisp_t *l, lisp_cell_t *args, void *param) {
	(void)param;
	lisp_cell_t *file = lisp_car(l, args);
	lisp_cell_t *expr = lisp_car(l, lisp_cdr(l, args));
	if (lisp_type_get(file) != LISP_SYMBOL) return l->Error;
	FILE *f = fopen(file->t[0].s, "wb");
	if (!f) return l->Error;
	lisp_cell_t *r = l->Tee;
	if (lisp_write(l, lisp_put_ch, f, expr, l->depth + 1) < 0) r = l->Error;
	if (fclose(f) < 0) r = l->Error;
	return r;
}

static lisp_cell_t *lisp_prim_load(lisp_t *l, lisp_cell_t *args, void *param) {
	(void)param;
	lisp_cell_t *file = lisp_car(l, args);
	if (lisp_type_get(file) != LISP_SYMBOL) return l->Error;
	FILE *f = fopen(file->t[0].s, "rb");
	if (!f) return l->Error;
	lisp_cell_t *expr = lisp_read(l, lisp_get_ch, f, l->depth + 1);
	if (!expr) expr = l->Error;
	if (fclose(f) < 0) expr = l->Error;
	return expr;
}

static lisp_cell_t *lisp_prim_symbols(lisp_t *l, lisp_cell_t *args, void *param) {
	(void)param;
	if (!lisp_isnil(l, args)) return l->Error;
	return l->interned;
}

static lisp_cell_t *lisp_prim_exit(lisp_t *l, lisp_cell_t *args, void *param) {
	(void)param;
	if (lisp_isnil(l, args)) exit(0);
	exit(lisp_ptrval(lisp_car(l, args)));
	return l->Error;
}

static lisp_cell_t *lisp_prim_memory(lisp_t *l, lisp_cell_t *args, void *param) {
	(void)param;
	(void)args;
	if (!l->arena) return l->Error;
	lisp_arena_t *a = l->arena;
	lisp_node_t *n = a->head;
	size_t i = 0;
	for (;n; n = n->next, i++)
		;
	return lisp_cons(l, lisp_mkint(l, a->bytes),
		lisp_cons(l, lisp_mkint(l, a->allocs), 
		lisp_cons(l, lisp_mkint(l, a->frees),
		lisp_cons(l, lisp_mkint(l, a->reallocs),
		lisp_cons(l, lisp_mkint(l, LISP_ARENA_SIZE),
		lisp_cons(l, lisp_mkint(l, i),
		lisp_cons(l, lisp_mkint(l, l->buf_size),
		lisp_cons(l, lisp_mkint(l, l->gc_stack_allocated),
		l->Nil))))))));
}

static lisp_cell_t *lisp_prim_cell(lisp_t *l, lisp_cell_t *args, void *param) {
	(void)param;
	const uintptr_t idx = lisp_compval(lisp_car(l, lisp_cdr(l, args)));
	lisp_cell_t *obj = lisp_car(l, args);
	switch (lisp_type_get(obj)) {
	case LISP_SYMBOL: {
		const size_t length = lisp_length_get(obj);
		if (!length) return l->Nil;
		return lisp_mkint(l, obj->t[0].s[LISP_MIN(length - 1, idx)]);
	}
	case LISP_PRIMITIVE:
		assert(lisp_length_get(obj) >= 2);
		return lisp_mkint(l, (uintptr_t)obj->t[LISP_MIN(idx, 1)].l);
	case LISP_FUNCTION:
		assert(lisp_length_get(obj) >= 3);
		return obj->t[LISP_MIN(idx, 2)].l;
	case LISP_CONS:
		assert(lisp_length_get(obj) >= 2);
		return obj->t[LISP_MIN(idx, 1)].l;
	case LISP_INTEGER: break;
	case LISP_INVALID: break;
	}
	return l->Error;
}

static lisp_cell_t *lisp_prim_param(lisp_t *l, lisp_cell_t *args, void *param) {
	(void)l;
	(void)args;
	return (lisp_cell_t*)param;
}

static lisp_cell_t *lisp_prim_time(lisp_t *l, lisp_cell_t *args, void *param) {
	(void)args;
	(void)param;
	return lisp_mkint(l, time(NULL));
}

static lisp_cell_t *lisp_prim_clock(lisp_t *l, lisp_cell_t *args, void *param) {
	(void)param;
	const int div = lisp_isnil(l, lisp_car(l, args));
	return lisp_mkint(l, div ? clock() / CLOCKS_PER_SEC : clock());
}

static lisp_cell_t *lisp_prim_raise(lisp_t *l, lisp_cell_t *args, void *param) {
	(void)param;
	return lisp_mkint(l, raise(lisp_compval(lisp_car(l, args))));
}

int lisp_extend(lisp_t *l) { /* We could put some stuff in `atexit` maybe */
	if (lisp_asserts(l) < 0) return -1;
	typedef struct { const char *name; lisp_function_t fn; void *arg; } lisp_extend_t;
	lisp_extend_t fns[] = {
		{  "depth",    lisp_prim_depth,    NULL,    },
		{  "fatal",    lisp_prim_fatal,    NULL,    },
		{  "gc",       lisp_prim_gc,       NULL,    },
		{  "get",      lisp_prim_get,      stdin,   },
		{  "put",      lisp_prim_put,      stdout,  },
		{  "scope",    lisp_prim_scope,    NULL,    },
		{  "getenv",   lisp_prim_getenv,   NULL,    },
		{  "save",     lisp_prim_save,     NULL,    },
		{  "load",     lisp_prim_load,     NULL,    },
		{  "symbols",  lisp_prim_symbols,  NULL,    },
		{  "exit",     lisp_prim_exit,     NULL,    },
		{  "memory",   lisp_prim_memory,   NULL,    },
		{  "cell",     lisp_prim_cell,     NULL,    },
		{  "eof",      lisp_prim_param,    l->Eof,  },
		{  "time",     lisp_prim_time,     NULL,    },
		{  "raise",    lisp_prim_raise,    NULL,    },
		{  "clock",    lisp_prim_clock,    NULL,    },
	};
	for (size_t i = 0; i < LISP_NELEMS(fns); i++) {
		lisp_extend_t *f = &fns[i];
		if (lisp_function_add(l, f->name, f->fn, f->arg) < 0) goto fail;
	}
	return 0;
fail:
	l->fatal = 1;
	return -1;
}

