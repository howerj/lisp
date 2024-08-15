#ifndef LISP_H
#define LISP_H
#ifdef __cplusplus
extern "C" {
#endif
#define LISP_PROJECT "A tiny embeddable lisp interpreter"
#define LISP_AUTHOR  "Richard James Howe"
#define LISP_EMAIL   "howe.r.j.89@gmail.com"
#define LISP_REPO    "https://github.com/howerj/lisp" /* For more information consult project readme.md */
#define LISP_VERSION "1.0.0"
#define LISP_LICENSE "Public Domain / 0BSD"

#include <assert.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h> 

#ifndef LISP_EXTEND /* add `lisp_extend()` to `lisp_init()`, a function *you* can define */
#define LISP_EXTEND (0)
#endif

#ifndef LISP_MAX_DEPTH /* Maximum recursion depth, set to zero to disable checks (may cause stack overflow) */
#define LISP_MAX_DEPTH (128)
#endif

#ifndef LISP_COLLECT_EVERY /* Call garbage collector every X allocations */
#define LISP_COLLECT_EVERY (0)
#endif

#ifndef LISP_EXTERN /* applied to API function declarations */
#define LISP_EXTERN extern
#endif

#ifndef LISP_API /* applied to API function definitions */
#define LISP_API
#endif

enum { LISP_INVALID, LISP_CONS, LISP_SYMBOL, LISP_INTEGER, LISP_FUNCTION, LISP_PRIMITIVE, };

struct lisp;
typedef struct lisp lisp_t;
struct lisp_cell;
typedef struct lisp_cell lisp_cell_t;

typedef lisp_cell_t *(*lisp_function_t)(lisp_t *, lisp_cell_t *args, void *param);

struct lisp_cell {
	uintptr_t tag; /* lowest 3 bits = type, next 1 bit = gc, rest = length */
	union { void *v; intptr_t n; lisp_cell_t *l; lisp_function_t fn; char s[sizeof (char*)]; } t[];
};

typedef struct { uintptr_t state; lisp_cell_t *obj; } lisp_stack_t;
typedef struct gc_list { lisp_cell_t *ref; struct gc_list *next; } lisp_gc_list_t;

struct lisp { /* Should be opaque, do not mess with the internals */
	void *(*alloc)(void *arena, void *ptr, size_t oldsz, size_t newsz);
	void *arena;
	lisp_cell_t *interned /* interned symbols */, *env /* top level environment */,
		    **gc_stack, /* garbage collection stack used in eval */
		    *Nil, *Tee, *Fn, *Quote, *If, *Loop, *Define, *Set, *Progn, *Error;
	lisp_stack_t *stack; /* execution sack, used to avoid recursion in read/write */
	lisp_gc_list_t *gc_head; /* list of all allocated cells */
	int ungetch /* single character unget buffer */, depth /* saved recursion depth for eval */;
	char *buf, *bufback; /* used to store symbols/strings whilst parsing */
	size_t buf_used, buf_size, gc_stack_allocated, gc_stack_used, gc_collect;
	unsigned fatal: 1, gc: 1 /* gc on/off*/, dynamic_scope: 1, init: 1, unget: 1, putback: 1 /* token put back */;
};

LISP_EXTERN int lisp_asserts(lisp_t *l);
LISP_EXTERN int lisp_init(lisp_t *l);
LISP_EXTERN int lisp_write(lisp_t *l, int (*put)(void *param, int ch), void *param, lisp_cell_t *obj);
LISP_EXTERN int lisp_function_add(lisp_t *l, const char *symbol, lisp_function_t fn, void *param);
LISP_EXTERN int lisp_unit_tests(lisp_t *l);
LISP_EXTERN int lisp_gc(lisp_t *l, int force);
LISP_EXTERN int lisp_deinit(lisp_t *l);
LISP_EXTERN lisp_cell_t *lisp_read(lisp_t *l, int (*get)(void *param), void *param, int depth);
LISP_EXTERN lisp_cell_t *lisp_eval(lisp_t *l, int list, lisp_cell_t *exp, lisp_cell_t *env, int depth);
LISP_EXTERN lisp_cell_t *lisp_assoc(lisp_t *l, lisp_cell_t *key, lisp_cell_t *alist); /* find sym in '((sym . val) ...) */

extern int lisp_extend(lisp_t *l); /* You define this function if LISP_EXTEND is defined, it will be called in `lisp_init` */

/* Do not call `lisp_make_string` and `lisp_make_object` directly */
LISP_EXTERN lisp_cell_t *lisp_make_string(lisp_t *l, int type, size_t length, const char *string);
LISP_EXTERN lisp_cell_t *lisp_make_object(lisp_t *l, int type, size_t count, ...);
/* Lisp Cell property manipulation function */
static inline int lisp_type_get(lisp_cell_t *c) { if (!c) return LISP_INVALID; assert(c); return c->tag & 0x7; }
static inline void lisp_type_set(lisp_cell_t *c, int type) { assert(c); assert(type <= 7); c->tag = type | (c->tag & ~7ull); }
static inline int lisp_gc_get(lisp_cell_t *c) { assert(c); return (c->tag >> 3) & 0x1; }
static inline void lisp_gc_set(lisp_cell_t *c, int gc) { assert(c); assert(gc <= 3); c->tag = (gc << 3) | (c->tag & ~(1ull << 3)); }
static inline size_t lisp_length_get(lisp_cell_t *c) { assert(c); return c->tag >> 4; }
static inline int lisp_length_valid(uintptr_t length) { return length < (1ull << ((sizeof(uintptr_t)*8) - 4)); }
static inline void lisp_length_set(lisp_cell_t *c, uintptr_t length) { assert(c); assert(lisp_length_valid(length)); c->tag = length << 4 | (c->tag & 15); }
/* Lisp Cell helper function */
static inline int lisp_iscons(lisp_cell_t *c) { assert(c); return lisp_type_get(c) == LISP_CONS; }
static inline int lisp_isnil(lisp_t *l, lisp_cell_t *c) { lisp_asserts(l); if (!c) { l->fatal = 1; return 0; } return c == l->Nil; }
static inline lisp_cell_t *lisp_car(lisp_t *l, lisp_cell_t *cons) { if (l->fatal || !cons || !lisp_iscons(cons)) return l->Error; return cons->t[0].l ? cons->t[0].l : l->Error; }
static inline lisp_cell_t *lisp_cdr(lisp_t *l, lisp_cell_t *cons) { if (l->fatal || !cons || !lisp_iscons(cons)) return l->Error; return cons->t[1].l ? cons->t[1].l : l->Error; }
static inline void lisp_setcar(lisp_t *l, lisp_cell_t *cons, lisp_cell_t *car) { assert(l); if (l->fatal || !cons || !lisp_iscons(cons) || !car) return; cons->t[0].l = car; }
static inline void lisp_setcdr(lisp_t *l, lisp_cell_t *cons, lisp_cell_t *cdr) { assert(l); if (l->fatal || !cons || !lisp_iscons(cons) || !cdr) return; cons->t[1].l = cdr; }
static inline intptr_t lisp_intval(lisp_cell_t *c) { assert(c); assert(lisp_type_get(c) == LISP_INTEGER); return c->t[0].n; }
static inline intptr_t lisp_ptrval(lisp_cell_t *c) { assert(c); return c->t[0].n; }
static inline intptr_t lisp_compval(lisp_cell_t *c) { assert(c); return lisp_type_get(c) == LISP_INTEGER ? lisp_ptrval(c) : (intptr_t)c; }
static inline const char *lisp_strval(lisp_cell_t *c) { assert(c); assert(lisp_type_get(c) == LISP_SYMBOL); return c->t[0].s; }
static inline lisp_cell_t *lisp_procargs(lisp_cell_t *c) { assert(c); assert(lisp_type_get(c) == LISP_FUNCTION); return c->t[0].l; }
static inline lisp_cell_t *lisp_proccode(lisp_cell_t *c) { assert(c); assert(lisp_type_get(c) == LISP_FUNCTION); return c->t[1].l; }
static inline lisp_cell_t *lisp_procenv(lisp_cell_t *c) { assert(c); assert(lisp_type_get(c) == LISP_FUNCTION); return c->t[2].l; }
static inline lisp_function_t lisp_primop(lisp_cell_t *c) { assert(c); assert(lisp_type_get(c) == LISP_PRIMITIVE); return c->t[0].fn; }
static inline lisp_cell_t *lisp_cons(lisp_t *l, lisp_cell_t *car, lisp_cell_t *cdr) { return lisp_make_object(l, LISP_CONS, 2, car, cdr); }
static inline lisp_cell_t *lisp_mksym(lisp_t *l, const char *sym) { return lisp_make_string(l, LISP_SYMBOL, strlen(sym), sym); }
static inline lisp_cell_t *lisp_mkprimop(lisp_t *l, lisp_function_t fn, void *param) { return lisp_make_object(l, LISP_PRIMITIVE, 2, fn, param); }
static inline lisp_cell_t *lisp_mkint(lisp_t *l, intptr_t n) { return lisp_make_object(l, LISP_INTEGER, 1, n); }
static inline lisp_cell_t *lisp_mkproc(lisp_t *l, lisp_cell_t *args, lisp_cell_t *code, lisp_cell_t *env) { return lisp_make_object(l, LISP_FUNCTION, 3, args, code, env); }
static inline void *lisp_primparam(lisp_cell_t *c) { assert(c); assert(lisp_type_get(c) == LISP_PRIMITIVE); return c->t[1].v; }
static inline int lisp_ispropercons(lisp_t *l, lisp_cell_t *c) { 
	if (!c) { l->fatal = 1; return 0; } 
	if (lisp_asserts(l) < 0) return 0;
	return lisp_type_get(c) == LISP_CONS && (lisp_isnil(l, lisp_cdr(l, c)) || lisp_type_get(lisp_cdr(l, c)) == LISP_CONS); 
}

#ifdef LISP_IMPLEMENTATION

#define LISP_MAX(X, Y)     ((X) > (Y) ? (X) : (Y))
#define LISP_NELEMS(X)     (sizeof(X) / sizeof ((X)[0]))
#define LISP_MEMBER_SIZE(TYPE, MEMBER) (sizeof (((TYPE*)0)->MEMBER))
#define lisp_implies(P, Q) assert(!(P) || (Q))
#define lisp_mutual(P, Q)  do { lisp_implies(P, Q); lisp_implies(Q, P); } while (0);

static int lisp_isspace(int ch) { return ch == ' ' || ch == '\n' || ch == '\r' || ch == '\t' || ch == '\v' || ch == '\f'; }

LISP_API int lisp_asserts(lisp_t *l) {
	assert(l);
	assert(l->alloc);
	lisp_mutual(l->gc_stack, l->gc_stack_allocated);
	if (l->init) {
		assert(l->interned); assert(l->env); 
		assert(l->Nil);      assert(l->Tee); 
		assert(l->Fn);       assert(l->Quote); 
		assert(l->If);       assert(l->Loop); 
		assert(l->Define);   assert(l->Set); 
		assert(l->Progn);    assert(l->Error);
		assert(l->gc_stack); /* not explicitly initialized, it should be non-null though */
	}
	/*assert(l->gc_stack_used < l->gc_stack_allocated);*/
	/*assert(l->init);*/
	return l->fatal ? -1 : 0;
}

static void *lisp_realloc(lisp_t *l, void *ptr, size_t sz) {
	lisp_asserts(l); /* do not return NULL, can cause other leaks */
	void *r = l->alloc(l->arena, ptr, 0, sz);
	if (sz && !r) {
		lisp_gc(l, 1); /* TODO: Will this cause problems? If we gc something that has not yet been added to a gc stack? */
		if (!(r = l->alloc(l->arena, ptr, 0, sz))) {
			l->fatal = 1;
			return NULL;
		}
	}
	return r;
}

static void *lisp_alloc(lisp_t *l, size_t sz) {
	lisp_asserts(l); 
	void *r = lisp_realloc(l, NULL, sz);
	if (!r) {
		l->fatal = 1;
		return NULL;
	}
	return memset(r, 0, sz);
}

static int lisp_free(lisp_t *l, void *ptr) {
	int r = 0;
	if (lisp_asserts(l) < 0) r = -1;
	(void)(l->alloc(l->arena, ptr, 0, 0));
	return r;
}

static lisp_cell_t *lisp_gc_stack_add(lisp_t * l, lisp_cell_t *op) {
	lisp_asserts(l);
	if (lisp_type_get(op) == LISP_SYMBOL) return op; /* should be interned, no need to add yet again */
	if (l->gc_stack_used)
		if (l->gc_stack[l->gc_stack_used - 1] == op)
			return op;
	if (l->gc_stack_used++ > l->gc_stack_allocated - 1) {
		l->gc_stack_allocated = l->gc_stack_used * 2;
		if (l->gc_stack_allocated < l->gc_stack_used) goto fatal;
		lisp_cell_t **olist = (lisp_cell_t**)lisp_realloc(l, l->gc_stack, l->gc_stack_allocated * sizeof(*l->gc_stack));
		if (!olist) goto fatal;
		l->gc_stack = olist;
	}
	l->gc_stack[l->gc_stack_used - 1] = op;	/* anything reachable in here is not freed */
	return op;
fatal:
	l->fatal = 1;
	assert(l->Error);
	return l->Error;
}

static lisp_cell_t *lisp_gc_add(lisp_t *l, lisp_cell_t *op) {
	lisp_asserts(l);
	lisp_gc_list_t *node = (lisp_gc_list_t*)lisp_alloc(l, sizeof (*node));
	if (!node) { assert(l->Error); return l->Error; }
	node->ref = op;
	node->next = l->gc_head;
	l->gc_head = node;
	lisp_cell_t *r = lisp_gc_stack_add(l, op); /* TODO: Handle error conditions... */
	if (++l->gc_collect > LISP_COLLECT_EVERY) {
		lisp_gc(l, 0);
		l->gc_collect = 0;
	}
	return r;
}

/* TODO: Serialization/Deserialization examples (probably need strings and floats 
 * for that, which could be done with custom types)
 * This would be for the `https://github.com/howerj/gladiator` project, so
 * would need scanf/printf formatting serdes functions */
/* TODO: If we are making an integer we should keep a small cache of them around and return that instead (at least 0, 1, -1). */
LISP_API lisp_cell_t *lisp_make_object(lisp_t *l, int type, size_t count, ...) {
	if (lisp_asserts(l) < 0) { assert(l->Error); return l->Error; }
	assert(type != LISP_INVALID);
	lisp_cell_t *r = (lisp_cell_t*)lisp_alloc(l, sizeof (*r) + count*(sizeof (r->t[0])));
	if (!r) goto end;
	lisp_type_set(r, type);
	lisp_length_set(r, count);
	va_list ap;
	va_start(ap, count);
	for (size_t i = 0; i < count; i++) {
		lisp_cell_t *v = va_arg(ap, lisp_cell_t *);
		if (type != LISP_INTEGER && type != LISP_PRIMITIVE && !v) { l->fatal = 1; v = l->Error; }
		r->t[i].l = v;
	}
	va_end(ap);
end:
	if (!r) { assert(l->Error); return l->Error; }
	return lisp_gc_add(l, r); /* TODO: Handle `lisp_gc_add` error condition */
}

LISP_API lisp_cell_t *lisp_make_string(lisp_t *l, int type, size_t length, const char *string) {
	if (lisp_asserts(l) < 0) return NULL;
	assert(length + 1 > length);
	const size_t alloc = LISP_MAX(length + 1, sizeof (char *));
	lisp_cell_t *r = (lisp_cell_t*)lisp_alloc(l, sizeof (*r) + alloc);
	if (!r) { assert(l->Error); return l->Error; }
	lisp_type_set(r, type);
	lisp_length_set(r, length + 1);
	memcpy(r->t[0].s, string, length + 1);
	return lisp_gc_add(l, r);
}

static lisp_cell_t *lisp_find_symbol(lisp_t *l, const char *symbol) {
	if (lisp_asserts(l) < 0) return NULL;
	assert(symbol);
	const size_t len = strlen(symbol) + 1;
	for (lisp_cell_t *c = l->interned; !lisp_isnil(l, c); c = lisp_cdr(l, c)) {
		lisp_cell_t *o = lisp_car(l, c);
		if (len != lisp_length_get(o)) continue;
		if (!memcmp(symbol, lisp_strval(o), len)) return c;
	}
	return l->Nil;
}

static lisp_cell_t *lisp_intern(lisp_t *l, const char *symbol) {
	if (lisp_asserts(l) < 0) return NULL;
	assert(symbol);
	lisp_cell_t *c = lisp_find_symbol(l, symbol);
	if (!lisp_isnil(l, c)) return lisp_car(l, c);
	const int gc = l->gc; 
	l->gc = 0; /* Turn GC off temporarily; we do not add symbols to GC stack as they are in `l->interned` */
	c = lisp_mksym(l, symbol);
	l->interned = lisp_cons(l, c, l->interned);
	l->gc = gc; /* Restore GC settings */
	return c;
}

static void lisp_mark(lisp_t *l, lisp_cell_t *c) {
	if (lisp_asserts(l) < 0) return;
	if (!c) return;
	if (lisp_gc_get(c)) return;
	switch (lisp_type_get(c)) {
		case LISP_FUNCTION:
			lisp_gc_set(c, 1);
			lisp_mark(l, lisp_procargs(c));
			lisp_mark(l, lisp_proccode(c));
			lisp_mark(l, lisp_procenv(c));
			break;
		case LISP_CONS:
			lisp_gc_set(c, 1);
			lisp_mark(l, lisp_car(l, c));
			lisp_mark(l, lisp_cdr(l, c));
			break;
		case LISP_SYMBOL: case LISP_INTEGER: case LISP_PRIMITIVE:
			lisp_gc_set(c, 1);
			break;
		default: l->fatal = 1; return;
	}
}

static int lisp_sweep(lisp_t * l) {
	assert(l);
	if (!l->gc)
		return 0;
	for (lisp_gc_list_t **p = &l->gc_head; *p != NULL;) {
		lisp_gc_list_t *v = *p;
		if (lisp_gc_get(v->ref)) {
			p = &v->next;
			lisp_gc_set(v->ref, 0);
		} else {
			*p = v->next;
			lisp_free(l, v->ref);
			lisp_free(l, v);
		}
	}
	return 0;
}

LISP_API int lisp_gc(lisp_t *l, int force) {
	(void)lisp_asserts(l);
	if (!force && !l->gc) return l->fatal ? -1 : 0;
	lisp_mark(l, l->interned);
	lisp_mark(l, l->env);
	for (size_t i = 0; i < l->gc_stack_used; i++)
		lisp_mark(l, l->gc_stack[i]);
	lisp_sweep(l);
	return l->fatal ? -1 : 0;
}

LISP_API lisp_cell_t *lisp_assoc(lisp_t *l, lisp_cell_t *key, lisp_cell_t *alist) {
	lisp_asserts(l);
	assert(key);
	assert(alist);
	for (;!lisp_isnil(l, alist); alist = lisp_cdr(l, alist))
		if (lisp_car(l, lisp_car(l, alist)) == key) return lisp_car(l, alist);
	return l->Nil;
}

static lisp_cell_t *lisp_extend_env(lisp_t *l, lisp_cell_t *env, lisp_cell_t *sym, lisp_cell_t *val) {
	return lisp_cons(l, lisp_cons(l, sym, val), env);
}

static lisp_cell_t *lisp_extends(lisp_t *l, lisp_cell_t *env, lisp_cell_t *syms, lisp_cell_t *vals) {
	lisp_asserts(l);
	assert(env);
	assert(syms);
	assert(vals);
	for (; lisp_iscons(env) && lisp_iscons(vals); syms = lisp_cdr(l, syms), vals = lisp_cdr(l, vals))
		env = lisp_extend_env(l, env, lisp_car(l,syms), lisp_car(l, vals));
	if (!lisp_isnil(l, syms))
		env = lisp_extend_env(l, env, syms, vals);
	return env;
}

static lisp_cell_t *lisp_extend_top(lisp_t *l, lisp_cell_t *sym, lisp_cell_t *val) {
	lisp_asserts(l);
	if (!sym || !val) { l->fatal = 1; return l->Error; }
	assert(sym);
	assert(val);
	lisp_setcdr(l, l->env, lisp_cons(l, lisp_cons(l, sym, val), lisp_cdr(l, l->env)));
	return val;
}

/* TODO: Rewrite so this is not recursive but uses an explicit stack, and with error checking... */
LISP_API lisp_cell_t *lisp_eval(lisp_t *l, int list, lisp_cell_t *exp, lisp_cell_t *env, int depth) {
	if (lisp_asserts(l) < 0) { assert(l->Error); return l->Error; }
	if (lisp_init(l) < 0) { assert(l->Error); return l->Error; }
	if (LISP_MAX_DEPTH && depth > LISP_MAX_DEPTH) return l->Error;
again:
	assert(exp);
	assert(env);
	size_t gc_saved_stack = l->gc_stack_used;
	if (l->fatal) return l->Error;
	if (lisp_isnil(l, exp)) return l->Nil;
	if (list) {
		lisp_cell_t *op = lisp_car(l, exp);
		exp = lisp_cdr(l, exp);
		op = lisp_cons(l, lisp_eval(l, 0, op, env, depth + 1), l->Nil);
		lisp_cell_t *head = op;
		for (; lisp_iscons(exp); exp = lisp_cdr(l, exp), op = lisp_cdr(l, op))
			lisp_setcdr(l, op, lisp_cons(l, lisp_eval(l, 0, lisp_car(l, exp), env, depth + 1), l->Nil));
		if (!lisp_isnil(l, exp)) return l->Error;
		return head;
	}
	lisp_gc_stack_add(l, exp);
	lisp_gc_stack_add(l, env);
	switch (lisp_type_get(exp)) { /* A VM would be faster than this tree walker, but more complex */
	case LISP_CONS: { /* This could be turned into `apply` */
		lisp_cell_t *op = lisp_car(l, exp), *n = lisp_cdr(l, exp);
		if (!lisp_isnil(l, n) && !lisp_ispropercons(l, n)) return l->Error;
		if (op == l->Error) return l->Error;
		if (op == l->If) {
			lisp_cell_t *v = lisp_eval(l, 0, lisp_car(l, n), env, depth + 1);
			exp = lisp_car(l, lisp_cdr(l, lisp_cdr(l, lisp_isnil(l, v) ? n : exp)));
			goto again;
		} else if (op == l->Loop) {
			lisp_cell_t *last = l->Nil, *cond = lisp_car(l, n);
			exp = lisp_cdr(l, n);
			gc_saved_stack = l->gc_stack_used;
			for (;last != l->Error;) {
				lisp_cell_t *res = lisp_eval(l, 0, cond, env, depth + 1);
				if (lisp_isnil(l, res) || res == l->Error) break;
				l->gc_stack_used = gc_saved_stack;
				last = lisp_eval(l, 0, exp, env, depth + 1);
			}
			l->gc_stack_used = gc_saved_stack;
			return lisp_gc_stack_add(l, last);
		} else if (op == l->Fn) {
			lisp_cell_t *t = lisp_mkproc(l, lisp_car(l, n), lisp_cdr(l, n), env);
			l->gc_stack_used = gc_saved_stack;
			return lisp_gc_stack_add(l, t);
		} else if (op == l->Define) {
			l->gc_stack_used = gc_saved_stack;
			lisp_cell_t *t = lisp_eval(l, 0, lisp_car(l, lisp_cdr(l, n)), env, depth + 1);
			return lisp_gc_stack_add(l, lisp_extend_top(l, lisp_car(l, n), t));
		} else if (op == l->Set) {
			lisp_cell_t *c = lisp_assoc(l, lisp_car(l, n), env);
			lisp_cell_t *v = lisp_eval(l, 0, lisp_car(l, lisp_cdr(l, n)), env, depth + 1);
			if (lisp_isnil(l, c)) return l->Error;
			lisp_setcdr(l, c, v);
			return v;
		} else if (op == l->Quote) {
			return lisp_car(l, n);
		} else if (op == l->Progn) {
			exp = n;
			if (lisp_isnil(l, exp)) return l->Nil;
			for(;!lisp_isnil(l, lisp_cdr(l, exp));) {
				l->gc_stack_used = gc_saved_stack;
				(void)lisp_eval(l, 0,  lisp_car(l, exp), env, depth + 1);
				exp = lisp_cdr(l, exp);
			}
			exp = lisp_car(l, exp);
			goto again;
		} /* TODO: handle (fn x ...) as well as (fn (x) ...) */
                lisp_cell_t *proc = lisp_eval(l, 0, op, env, depth + 1);
                lisp_cell_t *vals = lisp_eval(l, 1, n,  env, depth + 1);
                if (lisp_type_get(proc) == LISP_FUNCTION) {
			lisp_cell_t *scope = l->dynamic_scope ? env : lisp_procenv(proc);
                	env = lisp_extends(l, scope, lisp_procargs(proc), vals);
			exp = lisp_proccode(proc);
			l->gc_stack_used = gc_saved_stack;
			lisp_gc_stack_add(l, env);
			lisp_gc_stack_add(l, exp);
                	goto again;
                }
		l->depth = depth;
                if (lisp_type_get(proc) == LISP_PRIMITIVE) /* N.B. we could add an `env` parameter to all primitives */
                	return lisp_primop(proc)(l, vals, lisp_primparam(proc));
		return l->Error;
		}
		break;
	case LISP_SYMBOL: {
			lisp_cell_t *t = lisp_assoc(l, exp, env);
			if (lisp_isnil(l, t)) return l->Error;
			return lisp_cdr(l, t);
		}
		break;
	case LISP_INTEGER: case LISP_FUNCTION: case LISP_PRIMITIVE:
		return exp;
	default: l->fatal = 1; return NULL; // TODO: Custom types -> Return Self
	}
	return exp;
}

static int lisp_buf_add(lisp_t *l, int ch) {
	if (lisp_asserts(l) < 0) return -1;
	if ((l->buf_used + 2) > l->buf_size) {
		if ((l->buf_size * 2ull) < l->buf_size) goto fatal;
		l->buf_size = l->buf_size ? l->buf_size * 2ull : 128;
		if (!(l->buf = (char*)lisp_realloc(l, l->buf, l->buf_size))) goto fatal;
		if (!(l->bufback = (char*)lisp_realloc(l, l->bufback, l->buf_size))) goto fatal;
	}
	l->buf[l->buf_used++] = ch;
	l->buf[l->buf_used] = 0;
	return 0;
fatal:
	l->fatal = 1;
	return -1;
}

static int lisp_buf_putback(lisp_t *l) {
	if (lisp_asserts(l) < 0) return -1;
	assert(l->putback == 0);
	memcpy(l->bufback, l->buf, l->buf_used + 1);
	l->putback = 1;
	return 0;
}

static int lisp_get(lisp_t *l, int (*get)(void *param), void *param) {
	if (l->unget) { l->unget = 0; return l->ungetch; }
	return get(param);
}

static int lisp_unget(lisp_t *l, int ch) {
	assert(l->unget == 0);
	l->unget = 1;
	l->ungetch = ch;
	return 0;
}

static char *lisp_token(lisp_t *l, int (*get)(void *param), void *param) {
	if (lisp_asserts(l) < 0) return NULL;
	assert(get);
	l->buf_used = 0;
	if (l->putback) { l->putback = 0; return l->bufback; }
	int ch = 0, comment = 0;
	do {
		if ((ch = lisp_get(l, get, param)) < 0) return NULL;
		if (ch == ';') comment = 1;
		if (ch == '\n') comment = 0;
	} while (lisp_isspace(ch) || comment);
	if (lisp_buf_add(l, ch) < 0) return NULL;
	if (strchr("()'", ch)) return l->buf;
	for (ch = 0; (ch = lisp_get(l, get, param)) >= 0;) {
		if (strchr("()'", ch) || lisp_isspace(ch)) {
			lisp_unget(l, ch);
			return l->buf;
		}
		if (lisp_buf_add(l, ch) < 0) return NULL;
	}
	return l->buf;
}

static inline int lisp_digit(const int digit) {
	const char *string_digits = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
	const char *found = strchr(string_digits, digit);
	return found ? (int)(found - string_digits) : -1;
}

static inline int lisp_is_digit(const int digit, const int base) {
	const int r = lisp_digit(digit);
	return r < base ? r : -1;
}

static int lisp_string_to_number(const char *s, intptr_t *o, int base) {
	assert(s);
	assert(o);
	assert(base >= 2 && base <= 36);
	*o = 0;
	int ch = s[0];
	const int negate = ch == '-';
	const int prefix = negate || ch == '+';
	uintptr_t n = 0, np = 0;
	if (prefix && !s[1]) return -1;
	for (size_t j = prefix; (ch = s[j]); j++) {
		int digit = lisp_is_digit(ch, base);
		if (digit < 0) return -1;
		np = (uintptr_t)digit + (n * (uintptr_t)base);
		if (np < n) return -1;
		n = np;
	}
	if (!negate && (n > INTPTR_MAX)) return -1;
	if (negate && (n > ((uintptr_t)INTPTR_MAX + 1))) return -1;
	*o = n;
	if (negate) *o = -*o;
	return 0;
}

#if 1
LISP_API lisp_cell_t *lisp_read(lisp_t *l, int (*get)(void *param), void *param, int depth) {
	if (lisp_asserts(l) < 0) return NULL;
	if (lisp_init(l) < 0) return NULL;
	if (LISP_MAX_DEPTH && depth > LISP_MAX_DEPTH) return l->Error;
	assert(get);
	intptr_t n = 0;
	char *t = NULL;
	if (!(t = lisp_token(l, get, param))) return NULL;
	if (!strcmp(t, ")")) return l->Error;
	if (!strcmp(t, "(")) { 
		lisp_cell_t *head = NULL, *r = NULL;
		for (;(t = lisp_token(l, get, param));) {
			if (!strcmp(t, ")")) return head ? head : l->Nil;
			if (!strcmp(t, ".")) {
				if (!head || !r) return l->Error;
				lisp_cell_t *v = lisp_read(l, get, param, depth + 1);
				if (v == l->Error) return l->Error;
				lisp_setcdr(l, r, v);
				if (!(t = lisp_token(l, get, param))) return l->Error;
				if (strcmp(t, ")")) return l->Error;
				return head;
			}
			if (lisp_buf_putback(l) < 0) return l->Error;
			lisp_cell_t *v = lisp_read(l, get, param, depth + 1);
			v = lisp_cons(l, v, l->Nil);
			if (head)
				lisp_setcdr(l, r, v);
			else
				head = v;
			r = v;
		}
		return NULL;
	}
	if (!strcmp(t, "'")) { 
		lisp_cell_t *r = lisp_read(l, get, param, depth + 1);
		return lisp_cons(l, l->Quote, lisp_cons(l, r, l->Nil));
	}
	if (!lisp_string_to_number(t, &n, 10)) return lisp_mkint(l, n);
	return lisp_intern(l, t);
}
#else
LISP_API lisp_cell_t *lisp_read(lisp_t *l, int (*get)(void *param), void *param, int depth) {
	enum { BEGIN = 'B', TOKEN = 'T', QUOTE = 'Q', CONS = 'C', DOTTED = '.', POP = 'P', RPOP = 'R', TPOP = 'K', };
	intptr_t n = 0, sp = 0, state = BEGIN;
	lisp_cell_t *c = NULL, *r = NULL, *tok = NULL;
	char *t = NULL;
again:
	printf("sp %d st %c c=%p r=%p tok=%p\n", (int)sp, (int)state, c, r, tok);
	switch (state) {
	case BEGIN: state = TOKEN; break;
	case POP:
		if (!sp) return c;
		state = l->stack[sp - 1].state;
		c = l->stack[sp - 1].obj;
		sp--;
		break;
	case RPOP:
		if (!sp) return c;
		state = l->stack[sp - 1].state;
		r = l->stack[sp - 1].obj;
		sp--;
		break;
	case TPOP:
		if (!sp) return c;
		state = l->stack[sp - 1].state;
		tok = l->stack[sp - 1].obj;
		sp--;
		break;
	case TOKEN:
		state = POP;
		if (!(t = lisp_token(l, get, param))) return NULL;
		if (!strcmp(t, ")")) return l->Error;
		if (!strcmp(t, "(")) {
			if (!(t = lisp_token(l, get, param))) return NULL;
			if (!strcmp(t, ")")) {
				l->stack[sp++] = (lisp_stack_t) { .state = POP, .obj = tok = l->Nil, };
				break;
			}
			if (lisp_buf_putback(l) < 0) return l->Error;
			lisp_cell_t *p = r;
			r = lisp_cons(l, l->Nil, l->Nil);
			//l->stack[sp++] = (lisp_stack_t) { .state = POP,  .obj = c, };
			//l->stack[sp++] = (lisp_stack_t) { .state = RPOP, .obj = p, };
			//l->stack[sp++] = (lisp_stack_t) { .state = TPOP, .obj = tok, };
			l->stack[sp++] = (lisp_stack_t) { .state = CONS, .obj = tok = r, };
			state = TOKEN;
			break;
		}
		if (!strcmp(t, "'")) { 
			l->stack[sp++] = (lisp_stack_t) { .state = QUOTE, .obj = tok = lisp_cons(l, l->Quote, lisp_cons(l, l->Nil, l->Nil)), };
			state = TOKEN;
			break;
		}
		if (!lisp_string_to_number(t, &n, 10)) { 
			l->stack[sp++] = (lisp_stack_t) { .state = POP, .obj = tok = lisp_mkint(l, n), };
			break;
		}
		l->stack[sp++] = (lisp_stack_t) { .state = POP, .obj = tok = lisp_intern(l, t), };
		break;
	case QUOTE:
		state = POP;
		r = l->stack[sp].obj;
		lisp_setcar(l, lisp_cdr(l, r), l->stack[sp + 1].obj);
		c = r;
		break;
	case DOTTED:
		state = POP;
		lisp_setcdr(l, r, l->stack[sp + 1].obj);
		if (!(t = lisp_token(l, get, param))) return NULL;
		if (strcmp(t, ")")) return l->Error;
		break;
	case CONS:
		state = TOKEN;
		if (!(t = lisp_token(l, get, param))) return NULL;
		if (!strcmp(t, ".")) { // TODO: Test (a . b), (a . (b)), etcetera
			lisp_setcar(l, r, tok);
			l->stack[sp++] = (lisp_stack_t) { .state = DOTTED, .obj = c, };
			state = TOKEN;
			break;
		}
		if (!strcmp(t, ")")) {
			lisp_setcar(l, r, tok);
			tok = r;
			state = POP;
			break;
		}
		if (!strcmp(t, "(")) {
			lisp_buf_putback(l);
			if (!strcmp(t, ")")) break; // TODO: Test nil
			l->stack[sp++] = (lisp_stack_t) { .state = CONS, .obj = c, };
			l->stack[sp++] = (lisp_stack_t) { .state = RPOP, .obj = r, };
			l->stack[sp++] = (lisp_stack_t) { .state = POP,  .obj = c, };
			//l->stack[sp++] = (lisp_stack_t) { .state = TPOP, .obj = tok, };
			break;
		}
		lisp_buf_putback(l);
		lisp_setcar(l, r, tok);
		lisp_setcdr(l, r, lisp_cons(l, l->Nil, l->Nil));
		r = lisp_cdr(l, r);
		l->stack[sp++] = (lisp_stack_t) { .state = CONS, .obj = c, };
		break;
	default:
		l->fatal = 1;
		return NULL;
	}
goto again;
	return NULL;
}
#endif

static int lisp_puts(int (*put)(void *param, int ch), void *param, const char *s) {
	assert(put);
	for (int ch = 0; (ch = *s++);)
		if (put(param, ch) != ch) return -1;
	return 0;
}

static inline char *lisp_string_reverse(char *s, size_t length) { /* Modifies Argument */
	assert(s);
	for (size_t i = 0; i < (length / 2); i++) {
		char *a = &s[i], *b = &s[(length - i) - 1], t = 0;
		t = *a; *a = *b; *b = t;
	}
	return s;
}

static char *lisp_number_to_string(char buf[64 + 1 + 1], intptr_t n, int base) {
	assert(buf);
	assert(base >= 2 && base <= 36);
	int negate = n < 0, i = 0;
	uintptr_t u = negate ? -n : n;
	do buf[i++] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"[u % base]; while ((u /= base));
	if (negate) buf[i++] = '-';
	buf[i] = 0;
	return lisp_string_reverse(buf, i);
}

static int lisp_print_number(intptr_t n, int (*put)(void *param, int ch), void *param) {
	assert(put);
	char b[64 + 2];
	return lisp_puts(put, param, lisp_number_to_string(b, n, 10));
}

#if 0
LISP_API int lisp_write(lisp_t *l, int (*put)(void *param, int ch), void *param, lisp_cell_t *obj, int depth) {
	lisp_asserts(l);
	assert(put);
	if (!obj) goto fatal;
	if (lisp_init(l) < 0) return -1;
	if (LISP_MAX_DEPTH && depth > LISP_MAX_DEPTH) return -1;
	switch (lisp_type_get(obj)) {
	case LISP_CONS: {
		if (lisp_puts(put, param, "(") < 0) goto fatal;
		do {
			if (lisp_write(l, put, param, lisp_car(l, obj), depth + 1) < 0) return -1;
			if (lisp_isnil(l, lisp_cdr(l, obj))) {
				if (lisp_puts(put, param, ") ") < 0) goto fatal;
				return 0;
			}
			obj = lisp_cdr(l, obj);
		} while (lisp_iscons(obj));
		if (lisp_puts(put, param, ". ") < 0) goto fatal;
		if (lisp_write(l, put, param, obj, depth + 1) < 0) return -1;
		if (lisp_puts(put, param, ") ") < 0) goto fatal;
		break;
	}
	case LISP_SYMBOL: {
		if (lisp_isnil(l, obj)) return lisp_puts(put, param, "() ");
		if (lisp_puts(put, param, lisp_strval(obj)) < 0) goto fatal;
		return lisp_puts(put, param, " ");
	}
	case LISP_INTEGER: {
		if (lisp_print_number(lisp_intval(obj), put, param) < 0) goto fatal;
		return lisp_puts(put, param, " ");
	}
	case LISP_FUNCTION: {
		if (lisp_puts(put, param, "(fn ") < 0) goto fatal;
		if (lisp_write(l, put, param, lisp_procargs(obj), depth + 1) < 0) return -1;
		if (lisp_puts(put, param, " ") < 0) goto fatal;
		for (lisp_cell_t *c = lisp_proccode(obj); !lisp_isnil(l, c); c = lisp_cdr(l, c))
			if (lisp_write(l, put, param, lisp_car(l, c), depth + 1) < 0) return -1;
		if (lisp_puts(put, param, ") ") < 0) goto fatal;
		break;
	}
	case LISP_PRIMITIVE: {
		if (lisp_puts(put, param, "PRIMITIVE:") < 0) goto fatal;
		if (lisp_print_number(lisp_ptrval(obj), put, param) < 0) goto fatal;
		if (lisp_puts(put, param, ":") < 0) goto fatal;
		if (lisp_print_number((uintptr_t)lisp_primparam(obj), put, param) < 0) goto fatal;
		if (lisp_puts(put, param, " ") < 0) goto fatal;
		break;
	}
	default: goto fatal;
	}
	return 0;
fatal:
	l->fatal = 1;
	return -1;
}
#else
LISP_API int lisp_write(lisp_t *l, int (*put)(void *param, int ch), void *param, lisp_cell_t *obj) {
	lisp_asserts(l);
	assert(put);
	enum { START = 100, CONS_LOOP, CONS_END, CONS_DOTTED, POP, FUNC_LOOP, FUNC_END, };
	int state = START, sp = 0;
	if (lisp_init(l) < 0) return -1;
start:
	if (!obj) goto fatal;
	assert(sp < LISP_MAX_DEPTH); /* We could realloc here */
	switch (state) {
	case START:
		state = lisp_type_get(obj);
		assert(state < START);
		if (sp > LISP_MAX_DEPTH - 1) return -1;
		break;
	case LISP_CONS:
		state = CONS_LOOP;
		if (lisp_puts(put, param, "(") < 0) goto fatal;
		break;
	case CONS_LOOP:
		state = START;
		l->stack[sp++] = (lisp_stack_t){ .state = CONS_END, .obj = lisp_cdr(l, obj), };
		obj = lisp_car(l, obj);
		break;
	case CONS_END:
		if (lisp_isnil(l, obj)) {
			if (lisp_puts(put, param, ") ") < 0) goto fatal;
			state = POP;
		} else if (lisp_type_get(obj) != LISP_CONS) {
			state = START;
			if (lisp_puts(put, param, ". ") < 0) goto fatal;
			l->stack[sp++] = (lisp_stack_t){ .state = CONS_DOTTED, .obj = obj, };
		} else {
			state = CONS_LOOP;
		}
		break;
	case CONS_DOTTED:
		state = POP;
		if (lisp_puts(put, param, ") ") < 0) goto fatal;
		break;
	case LISP_FUNCTION:
		state = START;
		if (lisp_puts(put, param, "(fn ") < 0) goto fatal;
		l->stack[sp++] = (lisp_stack_t){ .state = FUNC_LOOP, .obj = lisp_proccode(obj), };
		obj = lisp_procargs(obj);
		break;
	case FUNC_LOOP:
		state = START;
		l->stack[sp++] = (lisp_stack_t){ .state = FUNC_END, .obj = lisp_cdr(l, obj), };
		obj = lisp_car(l, obj);
		break;
	case FUNC_END:
		if (lisp_isnil(l, obj)) {
			if (lisp_puts(put, param, ") ") < 0) goto fatal;
			state = POP;
		} else if (lisp_type_get(obj) != LISP_CONS) {
			state = START;
			if (lisp_puts(put, param, ". ") < 0) goto fatal;
			l->stack[sp++] = (lisp_stack_t){ .state = CONS_DOTTED, .obj = obj, };
		} else {
			state = FUNC_LOOP;
		}
		break;
	case LISP_SYMBOL:
		state = POP;
		if (lisp_isnil(l, obj)) { 
			if (lisp_puts(put, param, "() ") < 0) goto fatal;
			break;
		}
		if (lisp_puts(put, param, lisp_strval(obj)) < 0) goto fatal;
		if (lisp_puts(put, param, " ") < 0) goto fatal;
		break;
	case LISP_INTEGER:
		state = POP;
		if (lisp_print_number(lisp_intval(obj), put, param) < 0) goto fatal;
		if (lisp_puts(put, param, " ") < 0) goto fatal;
		break;
	case LISP_PRIMITIVE:
		state = POP;
		if (lisp_puts(put, param, "PRIMITIVE:") < 0) goto fatal;
		if (lisp_print_number(lisp_ptrval(obj), put, param) < 0) goto fatal;
		if (lisp_puts(put, param, ":") < 0) goto fatal;
		if (lisp_print_number((uintptr_t)lisp_primparam(obj), put, param) < 0) goto fatal;
		if (lisp_puts(put, param, " ") < 0) goto fatal;
		break;
	case POP:
		if (!sp) return 0;
		state = l->stack[sp - 1].state;
		obj = l->stack[sp - 1].obj;
		sp--;
		break;
	default: l->fatal = 1; return -1;
	}
goto start;
fatal:
	l->fatal = 1;
	return -1;
}
#endif

LISP_API int lisp_function_add(lisp_t *l, const char *symbol, lisp_function_t fn, void *param) {
	assert(l);
	assert(symbol);
	assert(fn);
	lisp_cell_t *name = lisp_intern(l, symbol), *prim = lisp_mkprimop(l, fn, param);
	if (!name || !prim) return -1;
	return lisp_extend_top(l, name, prim) ? 0 : -1;
}

static lisp_cell_t *lisp_prim_arith(lisp_t *l, lisp_cell_t *args, void *param) {
	if (lisp_asserts(l) < 0) return NULL;
	assert(args);
	if (lisp_isnil(l, args)) return lisp_mkint(l, 0);
	intptr_t n = lisp_compval(lisp_car(l, args)), op = (intptr_t)param;
	for (args = lisp_cdr(l, args); lisp_ispropercons(l, args); args = lisp_cdr(l, args)) {
		const intptr_t v = lisp_compval(lisp_car(l, args));
		switch (op) { /* mod, min and max would also make good operators */
		case 1: n -= v; break;
		case 2: n &= v; break;
		case 3: n |= v; break;
		case 4: n ^= v; break;
		case 5: n *= v; break;
		case 6: if (!v) return l->Error; n /= v; break;
		case 7: n <<= v; break;
		case 8: n >>= v; break;
		case 0: default: n += v; break;
		}
	}
	return lisp_mkint(l, n);
}


static lisp_cell_t *lisp_prim_comp(lisp_t *l, lisp_cell_t *args, void *param) {
	if (lisp_asserts(l) < 0) return NULL;
	assert(args);
	if (lisp_isnil(l, args)) return l->Tee;
	if (args == l->Error) return l->Error;
	intptr_t prev = lisp_compval(lisp_car(l, args)), op = (intptr_t)param;
	for (args = lisp_cdr(l, args); lisp_ispropercons(l, args); args = lisp_cdr(l, args)) {
		const intptr_t cur = lisp_compval(lisp_car(l, args));
		switch (op) {
		case 1: if (prev == cur) return l->Nil; break;
		case 2: if (prev >= cur) return l->Nil; break;
		case 3: if (prev >  cur) return l->Nil; break;
		case 4: if (prev <= cur) return l->Nil; break;
		case 5: if (prev <  cur) return l->Nil; break;
		case 0: default: if (prev != cur) return l->Nil; break;
		}
		prev = cur;
	}
	return l->Tee;
}

static lisp_cell_t *lisp_prim_cons(lisp_t *l, lisp_cell_t *args, void *param) { (void)param; return lisp_cons(l, lisp_car(l, args), lisp_car(l, lisp_cdr(l, args))); }
static lisp_cell_t *lisp_prim_car(lisp_t *l, lisp_cell_t *args, void *param) { (void)param; return lisp_car(l, lisp_car(l, args)); }
static lisp_cell_t *lisp_prim_cdr(lisp_t *l, lisp_cell_t *args, void *param) { (void)param; return lisp_cdr(l, lisp_car(l, args)); }
static lisp_cell_t *lisp_prim_type(lisp_t *l, lisp_cell_t *args, void *param) { (void)param; return lisp_mkint(l, lisp_type_get(lisp_car(l, args))); }

LISP_API int lisp_init(lisp_t *l) {
	assert(l);
	if (l->init) return 0;
	if (lisp_unit_tests(l) < 0) return -1;
	static const size_t ini = 16;
	if (!(l->gc_stack = (lisp_cell_t**)lisp_alloc(l, ini * sizeof(*l->gc_stack)))) goto fail;
	l->gc_stack_allocated = ini;
	if (!(l->stack = (lisp_stack_t*)lisp_alloc(l, LISP_MAX_DEPTH * sizeof(l->stack)))) goto fail;
	if (!(l->Nil    = lisp_mksym(l, "nil"))) goto fail;
	if (!(l->interned = lisp_cons(l, l->Nil, l->Nil))) goto fail;
	if (!(l->env = lisp_cons(l, lisp_cons(l, l->Nil, l->Nil), l->Nil))) goto fail;
	if (!(l->Tee    = lisp_intern(l, "t"))) goto fail;
	if (!(lisp_extend_top(l, l->Tee, l->Tee))) goto fail;
	if (!(l->Quote  = lisp_intern(l, "quote"))) goto fail; /* these definitions could be static */
	if (!(l->If     = lisp_intern(l, "if"))) goto fail;
	if (!(l->Loop   = lisp_intern(l, "do"))) goto fail;
	if (!(l->Fn     = lisp_intern(l, "fn"))) goto fail;
	if (!(l->Set    = lisp_intern(l, "set"))) goto fail;
	if (!(l->Progn  = lisp_intern(l, "pgn"))) goto fail;
	if (!(l->Define = lisp_intern(l, "def"))) goto fail;
	if (!(l->Error  = lisp_intern(l, "!"))) goto fail;
	if (lisp_function_add(l, "cons", lisp_prim_cons, NULL) < 0) goto fail;
	if (lisp_function_add(l, "car", lisp_prim_car, NULL) < 0) goto fail;
	if (lisp_function_add(l, "cdr", lisp_prim_cdr, NULL) < 0) goto fail;
	if (lisp_function_add(l, "type", lisp_prim_type, NULL) < 0) goto fail;
	static const char *arith[] = { "add", "sub", "and", "or", "xor", "mul", "div", "lls", "lrs", };
	for (size_t i = 0; i < LISP_NELEMS(arith); i++)
		if (lisp_function_add(l, arith[i], lisp_prim_arith, (void*)i) < 0)
			goto fail;
	static const char *comp[] = { "eq", "neq", "less", "leq", "more", "meq", };
	for (size_t i = 0; i < LISP_NELEMS(comp); i++)
		if (lisp_function_add(l, comp[i], lisp_prim_comp, (void*)i) < 0)
			goto fail;
	if (l->fatal) return -1;
	l->gc = 1;
	l->init = 1;
	l->gc_stack_used = 0;
	if (LISP_EXTEND) { if (lisp_extend(l) < 0) goto fail; }
	return 0;
fail:
	lisp_deinit(l);
	return -1;
}

LISP_API int lisp_deinit(lisp_t *l) {
	assert(l);
	l->gc = 1;
	lisp_sweep(l);
	lisp_free(l, l->gc_stack);
	lisp_free(l, l->stack);
	lisp_free(l, l->buf);
	lisp_free(l, l->bufback);
	l->gc_stack = NULL;
	l->gc_stack_allocated = 0;
	l->stack = NULL;
	l->buf = NULL;
	l->bufback = NULL;
	l->init = 0;
	return 0;
}

LISP_API int lisp_unit_tests(lisp_t *l) {
	assert(l);
	return 0; /* We could add tests here if needed, it is probably best to read them from a file instead. */
}

#ifdef LISP_DEFINE_MAIN
#include <stdio.h>
#include <stdlib.h>

#ifndef LISP_CUSTOM_ARENA /* enable custom allocator; smart enough to be dangerous */
#define LISP_CUSTOM_ARENA (1)
#endif
#ifndef LISP_ARENA_SIZE
#define LISP_ARENA_SIZE (4096)
#endif

static int lisp_put_ch(void *param, int ch) { return fputc(ch, (FILE*)param); }
static int lisp_get_ch(void *param) { return fgetc((FILE*)param); }

struct lisp_node; /* custom allocator type; enough to store most common allocation type */
typedef struct lisp_node lisp_node_t;
struct lisp_node { uintptr_t d[3]; lisp_node_t *next; };
typedef struct { lisp_node_t n[LISP_ARENA_SIZE]; lisp_node_t *head; int ini; } lisp_arena_t;

static inline lisp_node_t *lisp_node_alloc(lisp_arena_t *a, size_t sz) {
	if (!LISP_CUSTOM_ARENA) return NULL;
	assert(a);
	lisp_node_t *n = a->n;
	if (!a->ini) {
		a->head = n;
		for (size_t i = 0; i < (LISP_ARENA_SIZE - 1); i++)
			n[i].next = &n[i + 1];
		n[LISP_ARENA_SIZE - 1].next = NULL;
		a->ini = 1;
	}
	if (!sz) return NULL;
	if (sz <= sizeof(n->d)) {
		if (a->head) {
			lisp_node_t *r = a->head;
			a->head = r->next;
			r->next = NULL;
			return r;
		}
	}
	return NULL;
}

static inline int lisp_node_is(lisp_arena_t *a, void *p) {
	assert(a);
	lisp_node_t *f = (lisp_node_t *)p, *n = a->n;
	return f >= n && f <= &n[LISP_ARENA_SIZE - 1];
}

static inline int lisp_node_free(lisp_arena_t *a, void *p) {
	if (!LISP_CUSTOM_ARENA) return 0;
	assert(a);
	assert(a->ini);
	lisp_node_t *f = (lisp_node_t *)p;
	if (lisp_node_is(a, p)) {
		f->next = a->head;
		a->head = f;
		return 1;
	}
	return 0;
}

static void *lisp_allocator(void *arena, void *ptr, const size_t oldsz, const size_t newsz) {
	if (newsz == 0) { if (!lisp_node_free((lisp_arena_t *)arena, ptr)) free(ptr); return NULL; }
	if (newsz > oldsz) { 
		if (lisp_node_is((lisp_arena_t*)arena, ptr)) {
			if (newsz <= LISP_MEMBER_SIZE(lisp_node_t, d)) return ptr;
			void *r = calloc(newsz, 1);
			memcpy(r, ptr, LISP_MEMBER_SIZE(lisp_node_t, d));
			lisp_node_free((lisp_arena_t*)arena, ptr);
			return r;
		}
		void *r = lisp_node_alloc((lisp_arena_t *)arena, newsz);
		if (r) return r;
		return realloc(ptr, newsz); /* realloc fallback */
	}
	return ptr;
}

typedef struct { int (*get)(void *); int (*put)(void *, int ch); void *in, *out; } lisp_io_t;

static lisp_cell_t *lisp_prim_read(lisp_t *l, lisp_cell_t *args, void *param) {
	if (lisp_asserts(l) < 0) return NULL;
	assert(args);
	assert(param);
	lisp_io_t *io = (lisp_io_t*)param;
	return lisp_read(l, io->get, io->in, 0);
}

static lisp_cell_t *lisp_prim_write(lisp_t *l, lisp_cell_t *args, void *param) {
	if (lisp_asserts(l) < 0) return NULL;
	assert(args);
	assert(param);
	lisp_io_t *io = (lisp_io_t*)param;
	return lisp_mkint(l, lisp_write(l, io->put, io->out, args));
}

int main(void) {
	lisp_arena_t arena = { .ini = 0, };
	lisp_t lisp = { .alloc = lisp_allocator, .arena = &arena, }, *l = &lisp;
	lisp_io_t io = { .get = lisp_get_ch, .put = lisp_put_ch, .in = stdin, .out = stdout, };
	if (lisp_init(l) < 0) return 1;
	if (lisp_function_add(l, "in", lisp_prim_read, &io) < 0) goto fail;
	if (lisp_function_add(l, "out", lisp_prim_write, &io) < 0) goto fail;
	for (lisp_cell_t *c = NULL;(c = lisp_read(l, lisp_get_ch, io.in, 0));) {
		if (!(c = lisp_eval(l, 0, c, l->env, 0))) goto fail;
		if (lisp_write(l, lisp_put_ch, io.out, c) < 0) goto fail;
		if (fputc('\n', (FILE*)io.out) != '\n') goto fail;
	}
	lisp_deinit(l);
	return 0;
fail:
	lisp_deinit(l);
	return 1;
}
#endif /* LISP_DEFINE_MAIN */
#endif /* LISP_IMPLEMENTATION */
#ifdef __cplusplus
}
#endif
#endif /* LISP_H */
