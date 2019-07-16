//// pc.h - prolog runtime system


#ifndef PC_H
#define PC_H


// as if superficial warnings would make C any safer...
#if defined(_FORTIFY_SOURCE)
# undef _FORTIFY_SOURCE
#endif


/// header files

#include <stdio.h>
#include <stdarg.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include <time.h>
#include <sys/stat.h>
#include <errno.h>
#include <strings.h>
#include <limits.h>
#include <signal.h>


#ifdef _WIN32
# include <windows.h>
#else
# include <sys/mman.h>
#endif 

#include <fcntl.h>
#include <sys/types.h>
#include <setjmp.h>

#ifdef __MACH__
# include <mach/mach_time.h>
#endif

#ifdef PROFILE
# include <pthread.h>
# ifndef PROFILE_PERIOD
// nanoseconds
#  define PROFILE_PERIOD    10000
# endif
#endif


/// limits - all sizes are in bytes

#ifndef HEAP_SIZE
# define HEAP_SIZE  100000000
#endif

#ifndef TRAIL_STACK_SIZE
# define TRAIL_STACK_SIZE 1000000
#endif

#ifndef CHOICE_POINT_STACK_SIZE
# define CHOICE_POINT_STACK_SIZE 10000000
#endif

#ifndef ENVIRONMENT_STACK_SIZE 
# define ENVIRONMENT_STACK_SIZE 10000000
#endif

#ifndef ARGUMENT_STACK_SIZE
# define ARGUMENT_STACK_SIZE 10000000
#endif

#ifndef IFTHEN_STACK_SIZE
# define IFTHEN_STACK_SIZE 10000
#endif

#ifndef SYMBOL_TABLE_SIZE
# define SYMBOL_TABLE_SIZE 3001
#endif

#ifndef SHARED_TERM_TABLE_SIZE
# define SHARED_TERM_TABLE_SIZE 100000
#endif


// percentages
#ifndef HEAP_RESERVE
# define HEAP_RESERVE 5
#endif

#ifndef TRAIL_STACK_RESERVE
# define TRAIL_STACK_RESERVE 5
#endif


// these sizes are given in elements
#define DEBUG_WRITE_TERM_LIST_LENGTH_LIMIT 10
#define TRACE_DEBUG_WRITE_LIMIT 5
#define MAX_GLOBAL_VARIABLES 256
#define STRING_BUFFER_SIZE 1024
#define CATCHER_STACK_SIZE 1024
#define TRAIL_STACK_GAP_BUFFER_SIZE 1024
#define CYCLE_STACK_SIZE 4096


/// miscellanous

#if defined(__LLP64__) || defined(_WIN64)
# define XWORD_OUTPUT_FORMAT_LENGTH  "ll"
#else
# define XWORD_OUTPUT_FORMAT_LENGTH  "l"
#endif

#define XWORD_OUTPUT_FORMAT   "%" XWORD_OUTPUT_FORMAT_LENGTH "d"
#define XFLOAT_OUTPUT_FORMAT  "%.15g"

#if !defined(UNSAFE) && !defined(NDEBUG) && !defined(NOTRACE)
# define TRACE
#endif

#define EXIT_EXCEPTION   (-1)


/// types

typedef void *X;

#if defined(__LLP64__) || defined(_WIN64)
typedef long long XWORD;
typedef unsigned long long XUWORD;
#else
typedef long XWORD;
typedef unsigned long XUWORD;
#endif

typedef double XFLOAT;
typedef char XCHAR;

typedef struct BLOCK { 
  XWORD h;
  X d[];
} BLOCK;

typedef struct SPECIALBLOCK { 
  XWORD h;
  void *p;
  X d[];
} SPECIALBLOCK;

typedef struct BYTEBLOCK { 
  XWORD h;
  char d[];
} BYTEBLOCK;

typedef struct STRING_BLOCK {
  XWORD h;
  XCHAR s[];
} STRING_BLOCK;

typedef struct SYMBOL_BLOCK {
  XWORD h;
  X s;
  struct SYMBOL_STRUCT *next;
  X hash;
} SYMBOL_BLOCK;

typedef struct STRUCTURE_BLOCK {
  XWORD h;
  X f;
  X a[];
} STRUCTURE_BLOCK;

typedef struct FLONUM_BLOCK {
  XWORD h;
  XFLOAT n;
} FLONUM_BLOCK;

typedef struct PORT_BLOCK {
  XWORD h;
  FILE *fp;
  X dir;			/* 1 = input */
  X open;			/* 1 = open */
  X data;
} PORT_BLOCK;

typedef struct CHOICE_POINT {
  X *T, *R, *E, *A, *env_top, *arg_top;
  struct CHOICE_POINT *C0;
  void *P;
  struct CATCHER *catch_top;
  XWORD timestamp;
#if defined(PROFILE) || defined(PROFILE_MEMORY)
  struct PINFO *where;
#endif
} CHOICE_POINT;

typedef struct FINALIZER
{
  X object;
  void (*finalizer)(X);
  struct FINALIZER *next;
} FINALIZER;

typedef struct DB
{
  XCHAR *name;
  XWORD tablesize;
  struct DB_BUCKET **table;
} DB;

typedef struct DB_BUCKET
{
  DB *db;
  XWORD index;
  XCHAR *key;
  XWORD keylen;
  struct DB_ITEM *firstitem, *lastitem;
  struct DB_BUCKET *previous, *next;
} DB_BUCKET;

typedef struct DB_ITEM 
{
  X val;
  XWORD refcount;
  XWORD erased;
  DB_BUCKET *bucket;
  struct DB_ITEM *previous, *next;
  struct DB_ITEM *next_deleted;
} DB_ITEM;

typedef struct CATCHER
{
  CHOICE_POINT *C0;
  X ball;
  X *E, *T, *env_top, *arg_top;
  void *P;
  void **ifthen_top;
} CATCHER;

typedef struct SAVED_STATE
{
  X *A, *E;
  void *R, *P;
  CHOICE_POINT *C0, *C;
  X result;
} SAVED_STATE;

typedef struct TRAIL_STACK_GAP 
{
  X *position;
  XWORD size; 
} TRAIL_STACK_GAP;

typedef struct BLOCK_INTEGER_DISPATCH
{
  XWORD num;
  XWORD index;
} BLOCK_INTEGER_DISPATCH;

typedef struct BLOCK_SYMBOL_DISPATCH
{
  X symbol;
  XWORD index;
} BLOCK_SYMBOL_DISPATCH;

typedef struct BLOCK_STRUCTURE_DISPATCH
{
  X name;
  int arity;
  XWORD index;
} BLOCK_STRUCTURE_DISPATCH;

typedef struct SYMBOL_DISPATCH
{
  X symbol;
  void *label;
} SYMBOL_DISPATCH;

typedef struct STRUCTURE_DISPATCH
{
  X name;
  int arity;
  void *label;
} STRUCTURE_DISPATCH;

#ifdef PROFILE_MEMORY
typedef struct PINFO_AVG_COUNT
{
  XWORD total;
} PINFO_AVG_COUNT;

typedef struct PINFO_TOTAL_COUNT
{
  XWORD total;
} PINFO_TOTAL_COUNT;
#endif

typedef struct PINFO
{
  XCHAR *name;
  int count;
#ifdef PROFILE_MEMORY
  PINFO_TOTAL_COUNT heap;
  PINFO_AVG_COUNT cp;
  PINFO_AVG_COUNT tp;
  PINFO_AVG_COUNT ep;
#endif
  struct PINFO *next;
} PINFO;


/// tags and type-codes

#define FIXNUM_MARK_BIT 1

#define ALIGNMENT_HOLE_MARKER  ((X)0xdeadbeefL)

//XXX add more here
#if defined(__x86_64__) || defined(__LLP64__) || defined(__LP64__)
# define SIXTYFOUR
#endif

#if defined(__LLP64__) || defined(_WIN64)
# define XWORD_SIGN_BIT   0x8000000000000000LL
# define XWORD_TOP_BIT    0x4000000000000000LL
#elif defined(SIXTYFOUR)
# define XWORD_SIGN_BIT   0x8000000000000000L
# define XWORD_TOP_BIT    0x4000000000000000L
#else
# define XWORD_SIGN_BIT   0x80000000L
# define XWORD_TOP_BIT    0x40000000L
#endif


#ifdef SIXTYFOUR
# define TYPE_SHIFT 56
# define XWORD_SIZE  8
#else
# define TYPE_SHIFT 24
# define XWORD_SIZE  4
#endif

#ifdef DEBUG_GC
# ifdef SIXTYFOUR
#  if defined(__LLP64__) || defined(_WIN64)
#   define TAINTED_PTR_A   0xbabababababababaLL
#   define TAINTED_PTR_B   0xfefefefefefefefeLL
#  else
#   define TAINTED_PTR_A   0xbabababababababaL
#   define TAINTED_PTR_B   0xfefefefefefefefeL
#  endif
# else
#  define TAINTED_PTR_A   0xbabababaL
#  define TAINTED_PTR_B   0xfefefefeL
# endif
# define IS_TAINTED_PTR(p)   ((p) == TAINTED_PTR_A || (p) == TAINTED_PTR_B)
#else
# define IS_TAINTED_PTR(p)   0
#endif 

#define GC_MARK_BIT ((XWORD)0x80 << TYPE_SHIFT)
#define BYTEBLOCK_MARK_BIT ((XWORD)0x20 << TYPE_SHIFT)
#define SPECIALBLOCK_MARK_BIT ((XWORD)0x40 << TYPE_SHIFT)

#define HEADER_BITS_MASK  ((XWORD)0xff << TYPE_SHIFT)
#define HEADER_SIZE_MASK  (~HEADER_BITS_MASK)
#define HEADER_TYPE_MASK  ((XWORD)0x3f << TYPE_SHIFT)

#define FIXNUM_TYPE  1
#define END_OF_LIST_TYPE  2
#define SYMBOL_TYPE 3
#define FLONUM_TYPE  0x24
#define PORT_TYPE  0x45
#define VAR_TYPE 6
#define STRING_TYPE  0x27
#define STRUCTURE_TYPE  8
#define PAIR_TYPE  9
#define DBREFERENCE_TYPE 0x4a
#define POINTER_TYPE 0x4b

#define TYPE_TO_TAG(t)  ((XWORD)(t) << TYPE_SHIFT)
#define TAG_TO_TYPE(t)  ((XWORD)(t) >> TYPE_SHIFT)

#define PAIR_TAG  TYPE_TO_TAG(PAIR_TYPE)
#define VAR_TAG  TYPE_TO_TAG(VAR_TYPE)
#define STRING_TAG  TYPE_TO_TAG(STRING_TYPE)
#define STRUCTURE_TAG  TYPE_TO_TAG(STRUCTURE_TYPE)
#define FLONUM_TAG  TYPE_TO_TAG(FLONUM_TYPE)
#define END_OF_LIST_TAG  TYPE_TO_TAG(END_OF_LIST_TYPE)
#define SYMBOL_TAG  TYPE_TO_TAG(SYMBOL_TYPE)
#define PORT_TAG  TYPE_TO_TAG(PORT_TYPE)
#define DBREFERENCE_TAG TYPE_TO_TAG(DBREFERENCE_TYPE)

#ifdef USE_DELAY
# define VAR_SIZE 4
#else
# define VAR_SIZE 3
#endif

#define fixnum_to_word(n)  ((XWORD)(n) >> 1)
#define word_to_fixnum(n)  ((X)((XWORD)(n) << 1 | FIXNUM_MARK_BIT))
#define word_to_float(n)   ((XFLOAT)(n))
#define flonum_to_float(x)  (((FLONUM_BLOCK *)(x))->n)
#define fixnum_to_float(x)  ((XFLOAT)fixnum_to_word(x))

#define is_forwarded(x)    ((objbits(x) & GC_MARK_BIT) != 0)

#define ptr_to_fptr(ptr)        (((XWORD)(ptr) >> 1) | GC_MARK_BIT)
#define fptr_to_ptr(fptr)       ((X)((fptr) << 1))

#define ZERO     word_to_fixnum(0)
#define ONE      word_to_fixnum(1)

#define PREVIOUS_SYMBOL  END_OF_LIST_VAL

#define HASH_LENGTH_CUTOFF 100
#define HASH_MASK  0x3fffffffUL


/// predefined literals and global variables

#ifdef COMPILED_PROLOG_PROGRAM

#define END_OF_LIST_VAL  ((X)(&END_OF_LIST_VAL_BLOCK))

static BLOCK END_OF_LIST_VAL_BLOCK = { END_OF_LIST_TAG };
static X dot_atom, system_error_atom, type_error_atom, evaluation_error_atom;
static X instantiation_error_atom, user_interrupt_atom, end_of_file_atom;
static X user_input_atom, user_output_atom, user_error_atom;
static X current_input_atom, current_output_atom, current_error_atom;

static PORT_BLOCK default_input_port = { PORT_TAG|4, NULL, ONE, ONE, END_OF_LIST_VAL };
static PORT_BLOCK default_output_port = { PORT_TAG|4, NULL, ZERO, ONE, END_OF_LIST_VAL };
static PORT_BLOCK default_error_port = { PORT_TAG|4, NULL, ZERO, ONE, END_OF_LIST_VAL };

static X standard_input_port;
static X standard_output_port;
static X standard_error_port;

static X *fromspace, *fromspace_end, *fromspace_limit, *tospace, *tospace_end;
static X *tospace_top, *scan_ptr;
static X *alloc_top;
static X symbol_table[ SYMBOL_TABLE_SIZE ];
static XWORD heap_reserve, trail_stack_reserve;
static int verbose;
static int variable_counter;
static X *environment_stack;
static X *argument_stack;
static X *trail_stack;
static X *trail_top, *trail_stack_limit;
static X *env_top, *arg_top;
static CHOICE_POINT *choice_point_stack;
static FINALIZER *active_finalizers, *free_finalizers;
static XWORD gc_count;
static char *mmapped_heap = NULL;
static int mmapped_fd;
static void *mmapped_buffer;
static char **global_argv;
static int global_argc;
static void **ifthen_stack, **ifthen_top;
static XWORD clock_ticks;
static int global_variable_counter = 0;
static int initial_global_variable_count;
static X *shared_term_table;
static XWORD *shared_term_table_positions;
static XWORD shared_term_table_size;
static int shared_term_counter;
static XCHAR *string_buffer, *string_buffer_top;
static int string_buffer_length;
static DB_ITEM *deleted_db_items;
static int debugging;
static XWORD environment_stack_size, argument_stack_size, choice_point_stack_size;
static XWORD trail_stack_size, ifthen_stack_size, heap_size;
static jmp_buf exception_handler;
static CATCHER catch_stack[ CATCHER_STACK_SIZE ];
static CATCHER *catch_top;
static SAVED_STATE saved_state;
static X global_variables[ MAX_GLOBAL_VARIABLES ];
static TRAIL_STACK_GAP *trail_stack_gap_buffer;
static XWORD trail_stack_gap_buffer_size = TRAIL_STACK_GAP_BUFFER_SIZE;
static int gc_caused_by_trailing = 0;
static X triggered_frozen_goals;
static X first_distinct_variable;
static X *cycle_stack, *cycle_stack_top;

#if defined(PROFILE) || defined(PROFILE_MEMORY)
static PINFO system_pinfo = { .name = "<system>", .next = NULL };
static volatile PINFO *where;
static PINFO *pinfo_list;
#endif

#ifdef PROFILE
static volatile XWORD total_counts;
static XFLOAT start_time;
static pthread_t profile_thread;
static volatile int finish_profiling;
#endif

#ifdef _WIN32
static LARGE_INTEGER win32_perf_freq;
#endif


static XCHAR *type_names[] = { 
  "invalid", "integer", "null", "atom", "float", "stream", "variable", "string", "structure", "list", "dbreference"
};

#endif


#define type_name(t)         (type_names[ (XWORD)(t) & 0x1f ])
#define tag_to_type_name(t)  type_name(objtype(t))


/// debugging and termination

#define COUTPUT(...)  { fflush(stdout); fprintf(stderr, __VA_ARGS__); } 
#define CRASH(...)   { fflush(stdout); fprintf(stderr, "\n" __VA_ARGS__); fputc('\n', stderr); crash_hook(); exit(EXIT_FAILURE); }

#ifdef UNSAFE
# define NDEBUG
#endif

#ifdef NDEBUG
# define ASSERT(x, ...)   ;
# define DBG(...)         ;
# define DRIBBLE          ;
#else
# define ASSERT(x, ...)   { if(!(x)) CRASH("[internal error] " __VA_ARGS__) }
# define DBG              COUTPUT
# define DRIBBLE(...)     { if(verbose) COUTPUT(__VA_ARGS__); }
#endif

#ifdef DEBUGGING
# define DBG_DRIBBLE(...)   DRIBBLE(__VA_ARGS__);
#else
# define DBG_DRIBBLE(...)
#endif

#ifndef NO_CHECK_CYCLES

# define CHECK_CYCLE_STACK						\
  ASSERT(cycle_stack_top + 2 < cycle_stack + CYCLE_STACK_SIZE,		\
	 "cycle-stack overflow - cycle-checks may be ineffective")

# define SEARCH_IN_CYCLE_STACK(val)				\
  for(X *ptr = cycle_stack; ptr < cycle_stack_top; ptr += 2) {	\
    if(*ptr == val) return ptr[ 1 ]; }				\

# define PUSH_ON_CYCLE_STACK(val)  *(cycle_stack_top++) = (val)
# define POP_CYCLE_STACK   cycle_stack_top -= 2
# define INIT_CYCLE_STACK  cycle_stack_top = cycle_stack

#else
# define CHECK_CYCLE_STACK
# define SEARCH_IN_CYCLE_STACK(val)
# define INIT_CYCLE_STACK
# define PUSH_ON_CYCLE_STACK(val)
# define POP_CYCLE_STACK
#endif


/// accessors and type-testing

#ifdef SIXTYFOUR
# define FPALIGNED(x)  (1)
# define FPALIGN(alloc)
# define ALIGN(n)  (((n) + 7) & ~7L)
#else
# define FPALIGNED(x)  (((XWORD)objdata(x) & 0x7) == 0)
# define FPALIGN(alloc)  alloc = (X)((((XWORD)alloc + 7) & ~7L) + 4)
# define ALIGN(n)  (((n) + 3) & ~3L)
#endif

#define bytes_to_words(n)  (ALIGN(n) / XWORD_SIZE)
#define words_to_bytes(n)  ((n) * XWORD_SIZE)
#define floats_to_bytes(n)  ((n) * sizeof(XFLOAT))

#define objbits(x)  (((BLOCK *)(x))->h)
#define objtype(x)  (objbits(x) >> TYPE_SHIFT)
#define objdata(x)  (((BLOCK *)(x))->d)
#define objsize(x)  (((BLOCK *)(x))->h & HEADER_SIZE_MASK)

#define is_byteblock(x)  ((objbits(x) & BYTEBLOCK_MARK_BIT) != 0)
#define is_specialblock(x)  ((objbits(x) & SPECIALBLOCK_MARK_BIT) != 0)

#define is_FIXNUM(x)  (((XWORD)(x) & FIXNUM_MARK_BIT) != 0)

// END_OF_LIST_VAL is not available to external C code
#ifdef COMPILED_PROLOG_PROGRAM
# define is_END_OF_LIST(x)  ((x) == END_OF_LIST_VAL)
#else
# define is_END_OF_LIST(x)  is(END_OF_LIST_TYPE, (x))
#endif

#define is(t, x)    (objtype(x) == (t))
#define is_PAIR(x)  is(PAIR_TYPE, (x))
#define is_VAR(x)  is(VAR_TYPE, (x))
#define is_STRING(x)  is(STRING_TYPE, (x))
#define is_SYMBOL(x)  is(SYMBOL_TYPE, (x))
#define is_STRUCTURE(x)  is(STRUCTURE_TYPE, (x))
#define is_PORT(x)  is(PORT_TYPE, (x))
#define is_FLONUM(x)  is(FLONUM_TYPE, (x))
#define is_DBREFERENCE(x)  is(DBREFERENCE_TYPE, (x))
#define is_POINTER(x)  is(POINTER_TYPE, (x))

#define GLOBAL_REF(index)  global_variables[ index ]
#define GLOBAL_SET(index, x)  global_variables[ index ] = deref(x)

#define string_length(x)  ((objsize(x) / sizeof(XCHAR)) - 1)

#define IS_IN_HEAP(ptr)  ((X*)(ptr) >= fromspace && (X*)(ptr) < fromspace_end)

#define slot_ref(x, i)          (objdata(x)[ i ])

// doesn't make a difference, currently
#define atomic_slot_set(x, i, y)  (objdata(x)[ i ] = (y))

#define ASSIGN(dest, x)  dest = (x)

#define SLOT_SET(x, i, y)       ASSIGN(objdata(x)[ i ], (y))
#define SLOT_INIT(x, i, y)      atomic_slot_set(x, i, y)

#define port_file(p)  (((PORT_BLOCK *)(p))->fp)


static inline int is_number(X x)
{
  return is_FIXNUM(x) || is_FLONUM(x);
}


static inline int is_compound(X x)
{
  return !is_FIXNUM(x) && (is_PAIR(x) || is_STRUCTURE(x));
}


static inline int is_in_fixnum_range(XWORD n) {
  return (n & XWORD_SIGN_BIT) == ((n & XWORD_TOP_BIT) << 1);
}


static inline int is_atom(X x)
{
  return !is_FIXNUM(x) && (is_END_OF_LIST(x) || is_SYMBOL(x));
}


static inline int is_atomic(X x)
{
  return is_FIXNUM(x) || is_END_OF_LIST(x) || is_FLONUM(x) || is_SYMBOL(x) || is_PORT(x);
}


static inline int is_variable(X x)
{
  return !is_FIXNUM(x) && is_VAR(x);
}


static inline int is_pair(X x)
{
  return !is_FIXNUM(x) && is_PAIR(x);
}


static inline int is_stream(X x)
{
  return !is_FIXNUM(x) && is_PORT(x);
}


static inline int is_float(X x)
{
  return !is_FIXNUM(x) && is_FLONUM(x);
}


static inline int is_dbreference(X x)
{
  return !is_FIXNUM(x) && is_DBREFERENCE(x);
}


static inline int is_pointer(X x)
{
  return !is_FIXNUM(x) && is_POINTER(x);
}


static inline int is_structure(X x)
{
  return !is_FIXNUM(x) && is_STRUCTURE(x);
}


/// Memory-profiling counters

#ifdef PROFILE_MEMORY
# define COUNT_INC              ++where->count
# define COUNT_TOTAL(var, val)  where->var.total += (val)
#else
# define COUNT_INC
# define COUNT_TOTAL(var, val)
#endif


/// General allocators (externally visible9

#define ALLOCATE_BLOCK1(alloc, dest, type, size)			\
  dest = (void *)alloc;							\
  ((BLOCK *)alloc)->h = ((XWORD)(type) << TYPE_SHIFT) | (size);		\
  COUNT_TOTAL(heap, ((size) + 1) * sizeof(XWORD));			\
  alloc += (size) + 1

#define ALLOCATE_BYTEBLOCK1(alloc, dest, type, size)			\
  dest = (void *)alloc;							\
  ((BLOCK *)alloc)->h = ((XWORD)(type) << TYPE_SHIFT) | (size);		\
  COUNT_TOTAL(heap, (size) * sizeof(XWORD));				\
  alloc = (X)ALIGN((XWORD)alloc + (size) + sizeof(XWORD))

#define FLONUM1(alloc, m)  \
  ({ FPALIGN(alloc);							\
    ALLOCATE_BYTEBLOCK1(alloc, FLONUM_BLOCK *p_, FLONUM_TYPE, sizeof(XFLOAT)); \
    p_->n = (m); \
    (X)p_; })

#define PAIR1(alloc, x, y) \
  ({ ALLOCATE_BLOCK1(alloc, BLOCK *p_, PAIR_TYPE, 2); \
    p_->d[ 0 ] = (x); \
    p_->d[ 1 ] = (y); \
    (X)p_; })

#define PORT1(alloc, f, i, o, d)			   \
  ({ ALLOCATE_BLOCK1(alloc, PORT_BLOCK *p_, PORT_TYPE, 4);	\
     p_->fp = (f); \
     p_->dir = (i); \
     p_->open = (o); \
     p_->data = (d); \
     (X)p_; })

#define STRING1(alloc, len)						\
  ({ XWORD len2_ = (len);							\
    ALLOCATE_BYTEBLOCK1(alloc, BLOCK *p_, STRING_TYPE, len2_ + 1);	\
    ((XCHAR *)(p_->d))[ len2_ ] = '\0';					\
    (X)p_; })
  
#define CSTRING1(alloc, str)			\
  ({ char *str_ = str; \
    XWORD len3_ = strlen(str);						\
    X s_ = (X)STRING1(alloc, len3_);					\
    memcpy(objdata(s_), str_, len3_);					\
    s_; })
  
#define SYMBOL1(alloc, name, next, hash)				\
  ({ ALLOCATE_BLOCK1(alloc, BLOCK *p_, SYMBOL_TYPE, 3);			\
    SLOT_INIT((X)p_, 0, (name));					\
    SLOT_INIT((X)p_, 1, (next));					\
    SLOT_INIT((X)p_, 2, (hash));					\
    (X)p_; })

// does not initialize args
#define STRUCTURE1(alloc, functor, arity)				\
  ({ ALLOCATE_BLOCK1(alloc, BLOCK *s_, STRUCTURE_TYPE, (arity) + 1);	\
  SLOT_INIT((X)s_, 0, (functor));				\
  (X)s_; })

#define CSYMBOL1(alloc, str)   intern(CSTRING1(alloc, str))

#define POINTER1(alloc, ptr)  \
  ({ ALLOCATE_BLOCK1(alloc, SPECIALBLOCK *p_, POINTER_TYPE, 1);	\
    SLOT_INIT((X)p_, 0, ptr);					\
    (X)p_; })


#ifdef COMPILED_PROLOG_PROGRAM


/// Internal allocators

#define ALLOCATE_BLOCK(dest, type, size)  ALLOCATE_BLOCK1(alloc_top, dest, type, size)
#define ALLOCATE_BYTEBLOCK(dest, type, size)  ALLOCATE_BYTEBLOCK1(alloc_top, dest, type, size)
#define FLONUM(m)  FLONUM1(alloc_top, m)
#define PAIR(x, y)  PAIR1(alloc_top, x, y)
#define PORT(f, i, o, d)  PORT1(alloc_top, f, i, o, d)
#define STRING(len)  STRING1(alloc_top, len)
#define CSTRING(str)  CSTRING1(alloc_top, str)
#define SYMBOL(name, next, hash)  SYMBOL1(alloc_top, name, next, hash)
#define CSYMBOL(str)  CSYMBOL1(alloc_top, str)
#define STRUCTURE(functor, arity)  STRUCTURE1(alloc_top, functor, arity)
#define POINTER(ptr)  POINTER1(alloc_top, ptr)
#define BLOB(ptr, len)  ({ XWORD len_ = (len); X x_ = STRING(len_); memcpy(objdata(x_), (ptr), len_); intern(x_); })


/// Hook called when the system terminates abnormally

static void crash_hook()
{
  return;
}


/// Custom variants of some libc functions

static char *xstrndup(XCHAR *str, int len)
{
  XCHAR *buf = malloc(len + 1);
  ASSERT(buf, "can not duplicate string");
  strncpy(buf, str, len);
  return buf;
}


#ifdef DEBUG_GC
static void XFREE(void *mem, size_t size) 
{
  memset(mem, 0xd0, size);
  free(mem);
}
#else
# define XFREE(mem, size)   free(mem)
#endif



/// Converting port to string

static char *port_name(X x)
{
  PORT_BLOCK *p = (PORT_BLOCK *)x;
  static XCHAR buffer[ 256 ];
  sprintf(buffer, "<%s-stream>(%p)", p->dir != ZERO ? "input" : "output", (void *)p->fp);
  return buffer;
}


/// Variable dereferencing

#define deref(x)   ({ X _x = (x); is_FIXNUM(_x) || !is_VAR(_x) ? _x : deref1(_x); })

static X deref1(X val)
{
  for(;;) {
    if(is_FIXNUM(val) || !is_VAR(val))
      return val;

    X val2 = slot_ref(val, 0);

    if(val == val2) return val;
    
    val = val2;
  }
}


/// basic I/O 


// doesn't respect operators - expects deref'd datum
static void basic_write_term(FILE *fp, int debug, int limit, int quote, X x) { 
  if(limit == 0) fputs("...", fp);
  else if(is_FIXNUM(x)) 
    fprintf(fp, XWORD_OUTPUT_FORMAT, fixnum_to_word(x));
  else {
    switch(objtype(x)) {
    case FLONUM_TYPE:
      fprintf(fp, XFLOAT_OUTPUT_FORMAT, flonum_to_float(x));
      break;

    case END_OF_LIST_TYPE:
      fputs("[]", fp);
      break;

    case VAR_TYPE:
      fprintf(fp, "_" XWORD_OUTPUT_FORMAT, fixnum_to_word(slot_ref(x, 1)));
      break;

    case POINTER_TYPE:
      fprintf(fp, "<foreign pointer %p>", ((SPECIALBLOCK *)x)->p);
      break;

    case SYMBOL_TYPE: {
      X str = slot_ref(x, 0);
      char *name = (char *)objdata(str);
      XWORD len = string_length(str);
      int q = 0;

      if(quote) {
	if(len == 0 || !islower(name[ 0 ])) q = 1;
	else {
	  for(int i = 0; i < len; ++i) {
	    if(name[ i ] != '_' && !isalpha(name[ i ]) && !isdigit(name[ i ])) {
	      q = 1;
	      break;
	    }
	  }
	}
      }
      
      if(q) { 
	fputc('\'', fp);

	while(len--) {
	  int c = *(name++);
	  
	  switch(c) {
	  case '\n': fputs("\\n", fp); break;
	  case '\r': fputs("\\r", fp); break;
	  case '\t': fputs("\\t", fp); break;
	  case '\'': fputs("\\'", fp); break;
	  case '\\': fputs("\\\\", fp); break;
	  default:
	    if(c < 32 || c > 127) 
	      fprintf(fp, "\\x%02x", c);
	    else 
	      fputc(c, fp);
	  }
	}

	fputc('\'', fp);
      }
      else fwrite(name, sizeof(XCHAR), len, fp);

      break;
    }

    case PORT_TYPE:
      fputs(port_name(x), fp);
      break;

    case PAIR_TYPE: { 
      fputc('[', fp);
      --limit;
      basic_write_term(fp, debug, limit, quote, deref(slot_ref(x, 0)));
      int len = debug ? DEBUG_WRITE_TERM_LIST_LENGTH_LIMIT : 999999;

      for(x = deref(slot_ref(x, 1)); 
	  --len > 0 && !is_FIXNUM(x) && objtype(x) == PAIR_TYPE; 
	  x = deref(slot_ref(x, 1))) {
	fputc(',', fp); 
	basic_write_term(fp, debug, limit, quote, deref(slot_ref(x, 0)));
      }

      if(x == END_OF_LIST_VAL)
	fputc(']', fp);
      else if(len == 0)
	fputs("|...]", fp);
      else {
	fputc('|', fp);
	basic_write_term(fp, debug, limit, quote, deref(x));
	fputc(']', fp);
      }

      break;
    }

    case STRUCTURE_TYPE: {
      --limit;
      basic_write_term(fp, debug, limit, quote, slot_ref(x, 0));
      fputc('(', fp);
      XWORD len = objsize(x);
      
      for(int i = 1; i < len; ++i) {
	if(i > 1) 
	  fputs(", ", fp);

	basic_write_term(fp, debug, limit, quote, deref(slot_ref(x, i)));
      }

      fputc(')', fp);
      break;
    }

    case DBREFERENCE_TYPE:
      fprintf(fp, "<dbreference>(%p)", (void *)slot_ref(x, 0));
      break;

    case STRING_TYPE:
      fprintf(fp, "<internal string \"%s\">", (char *)objdata(x));
      break;

    default:
      fprintf(fp, "<object of unknown type %p:" XWORD_OUTPUT_FORMAT ">", (void *)x, objtype(x));
    }
  }
}


/// symbol-table management

static XWORD hash_name(XCHAR *name, int len)
{
  XWORD key = 0;
  
  if(len > HASH_LENGTH_CUTOFF) len = HASH_LENGTH_CUTOFF;

  while(len--)
    key = (key ^ ((key << 6) + (key >> 2) + *(name++))) & HASH_MASK;

  return key;
}


static X intern(X name)
{
  XWORD len = string_length(name);
  XWORD hash = hash_name((XCHAR *)objdata(name), len);
  XWORD key = hash % SYMBOL_TABLE_SIZE;

  for(X sym = symbol_table[ key ]; sym != END_OF_LIST_VAL; sym = slot_ref(sym, 1)) {
    X name2 = slot_ref(sym, 0);

    if(string_length(name2) == len &&
       !strncmp((XCHAR *)objdata(name), (XCHAR *)objdata(name2), len)) //UUU
      return sym;
  }

  X oldsym = symbol_table[ key ];
  X sym = SYMBOL(name, oldsym, word_to_fixnum(hash));
  symbol_table[ key ] = sym;
  return sym;
}


static void intern_static_symbols(X sym1)
{
  while(sym1 != END_OF_LIST_VAL) {
    X name = slot_ref(sym1, 0);
    XWORD len = string_length(name);
    XWORD hash = fixnum_to_word(slot_ref(sym1, 2));
    XWORD key = hash % SYMBOL_TABLE_SIZE;
    X sym = symbol_table[ key ];
    X nextsym = slot_ref(sym1, 1);
    SLOT_SET(sym1, 1, sym);
    symbol_table[ key ] = sym1;
    sym1 = nextsym;
  }

  dot_atom = intern(CSTRING("."));
  system_error_atom = intern(CSTRING("system_error"));
  type_error_atom = intern(CSTRING("type_error"));
  evaluation_error_atom = intern(CSTRING("evaluation_error"));
  instantiation_error_atom = intern(CSTRING("instantiation_error"));
  user_interrupt_atom = intern(CSTRING("user_interrupt"));
  end_of_file_atom = intern(CSTRING("end_of_file"));
  user_input_atom = intern(CSTRING("user_input"));
  user_output_atom = intern(CSTRING("user_output"));
  user_error_atom = intern(CSTRING("user_error"));
  current_input_atom = intern(CSTRING("current_input"));
  current_output_atom = intern(CSTRING("current_output"));
}


/// Hash-table lookup used for clause-indexing

static void *lookup_symbol_in_table(X sym, SYMBOL_DISPATCH *table, void *deflabel, int len)
{
  XWORD key = fixnum_to_word(slot_ref(sym, 2)) % len;

  for(;;) {
    X tsym = table[ key ].symbol;

    if(tsym == sym) return table[ key ].label;
    
    if(tsym == NULL) return deflabel;

    key = (key + 1) % len;
  }
}


static void *lookup_structure_in_table(X s, STRUCTURE_DISPATCH *table, void *deflabel, int len)
{
  X sym = slot_ref(s, 0);
  int arity = objsize(s) - 1;
  XWORD key = (fixnum_to_word(slot_ref(sym, 2)) + arity) % len;

  for(;;) {
    X tsym = table[ key ].name;

    if(tsym == sym && table[ key ].arity == arity)
      return table[ key ].label;
    
    if(tsym == NULL) return deflabel;

    key = (key + 1) % len;
  }
}


/// Exception handling

static void throw_exception(X ball)
{
  if(catch_top == catch_stack) {
#ifdef EMBEDDED
    saved_state.result = ball;
    longjmp(exception_handler, 2);
#else
    fflush(stdout);
    fputs("\nUnhandled exception:\n", stderr);
    basic_write_term(stderr, 1, 10, 1, ball);
    fputc('\n', stderr);
    crash_hook();
    exit(EXIT_FAILURE);
#endif
  }

  --catch_top;
  arg_top = catch_top->arg_top;
  catch_top->ball = ball;
  longjmp(exception_handler, 1);
}


static void signal_handler(int sig)
{
  signal(SIGINT, signal_handler);
  throw_exception(user_interrupt_atom);
}


static void system_error(XCHAR *msg)
{
  X str = intern(CSTRING(msg));
  X exn = STRUCTURE(system_error_atom, 1);
  SLOT_INIT(exn, 1, str);
  throw_exception(exn);
}


static void type_error(XCHAR *msg, X culprit)
{
  X str = intern(CSTRING(msg));
  X exn = STRUCTURE(type_error_atom, 2);
  SLOT_INIT(exn, 1, str);
  SLOT_INIT(exn, 2, culprit);
  throw_exception(exn);
}


static void evaluation_error(char *msg)
{
  X str = intern(CSTRING(msg));
  X exn = STRUCTURE(type_error_atom, 1);
  SLOT_INIT(exn, 1, str);
  throw_exception(exn);
}


/// type checking

static void check_type_failed(XWORD t, X x)
{
  if(is_FIXNUM(x) || !is_VAR(x))
    type_error(type_name(t), x);
  
  throw_exception(instantiation_error_atom);
}


static inline X check_type(XWORD t, X x)
{
#ifndef UNSAFE
  if(is_FIXNUM(x) || objtype(x) != t)
    check_type_failed(t, x);
#endif

  return x;
}


static inline X check_fixnum(X x)
{
#ifndef UNSAFE
  if(!is_FIXNUM(x))
    check_type_failed(FIXNUM_TYPE, x);
#endif

  return x;
}


static void check_number_failed(X x)
{
  if(is_FIXNUM(x) || !is_VAR(x)) 
    type_error("number", x);

  throw_exception(instantiation_error_atom);
}


static inline X check_number(X x)
{
#ifndef UNSAFE
  if(!is_FIXNUM(x) && objtype(x) != FLONUM_TYPE)
    check_number_failed(x);
#endif
  
  return x;
}


static void check_integer_failed(X x)
{
  if(is_FIXNUM(x) || !is_VAR(x))
    type_error("integer", x);

  throw_exception(instantiation_error_atom);
}


static inline X check_integer(X x)
{
#ifdef UNSAFE
  return x;
#else

  if(is_FIXNUM(x))
    return x;

  if(objtype(x) == FLONUM_TYPE) {
    XFLOAT n;

    if(modf(flonum_to_float(x), &n) == 0)
      return x;
  }

  check_integer_failed(x);
  return x;			/* never executed */
#endif
}


static void check_atomic_failed(X x)
{
  if(is_FIXNUM(x) || is_VAR(x))
    type_error("atomic", x);

  throw_exception(instantiation_error_atom);
}


static inline X check_atomic(X x)
{
#ifndef UNSAFE
  if(!is_FIXNUM(x) && !is_atomic(x))
    check_atomic_failed(x);
#endif
  
  return x;
}


#define check_type_PAIR(x)  check_type(PAIR_TYPE, (x))
#define check_type_STRING(x)  check_type(STRING_TYPE, (x))
#define check_type_SYMBOL(x)  check_type(SYMBOL_TYPE, (x))
#define check_type_FLONUM(x)  check_type(FLONUM_TYPE, (x))
#define check_type_SYMBOL(x)  check_type(SYMBOL_TYPE, (x))
#define check_type_PORT(x)  check_type(PORT_TYPE, (x))
#define check_type_STRUCTURE(x)  check_type(STRUCTURE_TYPE, (x))
#define check_type_DBREFERENCE(x)  check_type(DBREFERENCE_TYPE, (x))
#define check_type_POINTER(x)  check_type(POINTER_TYPE, (x))


static inline int is_port_and_direction(X x, X d)
{
  return !is_FIXNUM(x) && is_PORT(x) && slot_ref(x, 1) == d;
}


static inline X check_input_port(X x)
{
#ifndef UNSAFE
  check_type_PORT(x);
  
  if(slot_ref(x, 1) == ZERO)
    type_error("input-port", x);
#endif

  return x;
}


static inline X check_output_port(X x)
{
#ifndef UNSAFE
  check_type_PORT(x);

  if(slot_ref(x, 1) != ZERO)
    type_error("output-port", x);
#endif

  return x;
}


static void check_nonvar_failed(XCHAR *name, int arity, int arg) 
{
  sprintf(string_buffer, "%s/%d: mode declaration violated for argument %d", name, 
	  arity, arg + 1);
  system_error(string_buffer);
}


#ifdef UNSAFE
# define CHECK_NONVAR(i)
#else
# define CHECK_NONVAR(i)						\
  { X x_ = deref(A[ i ]);						\
    if(!is_FIXNUM(x_) && is_VAR(x_))					\
      check_nonvar_failed(CURRENT_NAME, CURRENT_ARITY, i); }
#endif


/// Management of the "shared term table"

static void clear_shared_term_table()
{
  for(int i = 0; i < shared_term_counter; ++i) {
    XWORD p = shared_term_table_positions[ i ];
    shared_term_table[ p ] = shared_term_table[ p + 1 ] = NULL;
  }

  shared_term_counter = 0;
}


static X *lookup_shared_term(X x, int addnew)
{
  XWORD key = (XUWORD)x % shared_term_table_size;
  int f = 0;
  key *= 2;

  while(shared_term_table[ key ] != x) { /* found? */
    if(shared_term_table[ key ] == NULL) { /* unused entry? */
      if(!addnew) return NULL;

      shared_term_table_positions[ shared_term_counter++ ] = key;
      break;
    }

    key += 2;			// try next
    
    if(key >= (shared_term_table_size * 2)) { /* beyond table size? */
      ASSERT(!f, "shared term table full");
      f = 1;
      key = 0;			/* start over from the beginning */
    }
  }

  return &(shared_term_table[ key ]); /* entry found */
} 


/// Variable + trail handling

static inline X make_var()
{
  ALLOCATE_BLOCK(BLOCK *v, VAR_TYPE, VAR_SIZE);
  v->d[ 0 ] = (X)v;
  v->d[ 1 ] = word_to_fixnum(variable_counter++);
  v->d[ 2 ] = word_to_fixnum(clock_ticks++);
#ifdef USE_DELAY
  v->d[ 3 ] = END_OF_LIST_VAL;
#endif
  return v;
}


static void unwind_trail(X *tp)
{
  ASSERT(tp >= trail_stack && tp <= trail_top, "trail-unwind to invalid pointer");

  while(trail_top != tp) {
    BLOCK *var = (BLOCK *)(*(--trail_top));
#ifdef USE_DELAY
    X al = *(--trail_top);

    // invalidate any delayed goals by destructively modifying the ptr/args pair
    for(X old = slot_ref(var, 3); old != END_OF_LIST_VAL; old = slot_ref(old, 1)) {
      X a = slot_ref(old, 0);
      SLOT_SET(a, 0, ZERO);
    }
#endif

    DBG_DRIBBLE("[detrail: _" XWORD_OUTPUT_FORMAT "]\n", fixnum_to_word(slot_ref((X)var, 1)));
    SLOT_SET(var, 0, var);
#ifdef USE_DELAY
    SLOT_SET(var, 3, al);
#endif
  }
}


static inline void force_gc()
{
  alloc_top = fromspace_limit + 1; /* trigger GC on next check */
}


static inline void push_trail(CHOICE_POINT *C0, X var)
{
  // trail-check
  if(fixnum_to_word(slot_ref(var, 2)) < C0->timestamp) {
#ifndef UNSAFE
# ifdef USE_DELAY
    int slots = 2;
# else
    int slots = 1;
#endif

    if((XWORD)trail_top + sizeof(X) * slots >= (XWORD)trail_stack + trail_stack_size)
      CRASH("trail-stack overflow");
#endif

    if(trail_top >= trail_stack_limit) {
      gc_caused_by_trailing = 1;
      force_gc();
    }

#ifdef USE_DELAY
    *(trail_top++) = slot_ref(var, 3); /* frozen goals */
#endif
    *(trail_top++) = var;
  }
}


// deref recursively, effectively copying term
static X deref_recursive(X val, int limit, int dup, int *failed)
{
  *failed = 0;

  if((!dup && limit <= 0) || is_FIXNUM(val)) return val;

  if(is_VAR(val)) {
    val = deref1(val);

    if(is_variable(val)) {
      X *tp = lookup_shared_term(val, 1);

      if(*tp != NULL) return tp[ 1 ];

      *tp = val;

      if(alloc_top + 4 > fromspace_limit) {
	*failed = 1;
	return val;
      }

      ALLOCATE_BLOCK(BLOCK *newvar, VAR_TYPE, VAR_SIZE);
      newvar->d[ 0 ] = (X)newvar;
      newvar->d[ 1 ] = word_to_fixnum(variable_counter++);
      newvar->d[ 2 ] = word_to_fixnum(clock_ticks++);
#ifdef USE_DELAY
      newvar->d[ 3 ] = deref_recursive(slot_ref(val, 3), limit, dup, failed);
#endif

      if(*failed) return val;

      return tp[ 1 ] = (X)newvar;
    }
  }

  if(is_FIXNUM(val) || val == END_OF_LIST_VAL || is_byteblock(val) || is_SYMBOL(val))
    return val;

  SEARCH_IN_CYCLE_STACK(val)
  XWORD s = objsize(val);

  if(alloc_top + s + 1 > fromspace_limit) {
    *failed = 1;
    return val;
  }

  CHECK_CYCLE_STACK;
  PUSH_ON_CYCLE_STACK(val);
  X *oldtop = alloc_top;	/* for restoration in case term is ground */
  ALLOCATE_BLOCK(BLOCK *p, objtype(val), s);
  PUSH_ON_CYCLE_STACK((X)p);
  --limit;
  int i = 0;

  if(is_specialblock(val)) {
    p->d[ 0 ] = slot_ref(val, 0);
    i = 1;
  }

  int anynew = 0;

  while(i < s) {
    X old = slot_ref(val, i);
    p->d[ i ] = deref_recursive(old, limit, dup, failed);

    if(*failed) return val;

    if(p->d[ i ] != old) anynew = 1;

    ++i;
  }

  POP_CYCLE_STACK;

  if(!dup && !anynew) {
    // keep old value, as it is ground
    alloc_top = oldtop;
    return val;
  }

  return (X)p;
}


static X deref_all(X val, int limit, int dup, int *failed)
{
#ifdef PROFILE_MEMORY
  XWORD oldheap = where->heap.total;
#endif
  INIT_CYCLE_STACK;
  X y = deref_recursive(val, limit, dup, failed);
  clear_shared_term_table();

#ifdef PROFILE_MEMORY
  if(*failed) where->heap.total = oldheap;
#endif

  return y;
}


/// Databases

// evict into malloc'd memory
static void freeze_term_recursive(X x, X *dest)
{
 restart:
  x = deref(x);

  if(is_FIXNUM(x) || x == END_OF_LIST_VAL) {
    *dest = x;
    return;
  }

  if(is_VAR(x)) {
    X *tp = lookup_shared_term(x, 1);

    if(*tp != NULL) {
      *dest = tp[ 1 ];
      return;
    }

    *tp = x;
    BLOCK *newvar = (BLOCK *)malloc(sizeof(XWORD) * (VAR_SIZE + 1));
    ASSERT(newvar, "out of memory - can not freeze variable");
    newvar->h = VAR_TAG | VAR_SIZE;
    newvar->d[ 0 ] = (X)newvar;
    newvar->d[ 1 ] = word_to_fixnum(variable_counter++);
    newvar->d[ 2 ] = word_to_fixnum(0); /* timestamp */
    tp[ 1 ] = (X)newvar;
    *dest = tp[ 1 ];
#ifdef USE_DELAY
    x = slot_ref(x, 3);
    dest = &(newvar->d[ 3 ]);
    goto restart;
#endif
    return;
  }

  ASSERT(!is_DBREFERENCE(x), "db-references can not be frozen");

  if(is_byteblock(x)) {
    XWORD size = objsize(x);
    BYTEBLOCK *b = (BYTEBLOCK *)malloc(sizeof(XWORD) + size);
    ASSERT(b, "out of memory - can not freeze byteblock");
    b->h = objbits(x);
    memcpy(b->d, objdata(x), size);
    *dest = (X)b;
    return;
  }

  if(is_SYMBOL(x)) {
    // just freeze string - it will be interned when thawed
    X str = slot_ref(x, 0);
    X *tp = lookup_shared_term(str, 1);

    if(*tp != NULL) {
      *dest = tp[ 1 ];
      return;
    }

    *tp = x;
    freeze_term_recursive(str, &(tp[ 1 ]));
    *dest = tp[ 1 ];
    return;
  }

#ifndef NO_CHECK_CYCLES
  for(X *ptr = cycle_stack; ptr < cycle_stack_top; ptr += 2) {	
    if(*ptr == x) {
      *dest = ptr[ 1 ];
      return;
    }
  }			
#endif

  CHECK_CYCLE_STACK;
  PUSH_ON_CYCLE_STACK(x);
  XWORD size = objsize(x);
  XWORD i = 0;
  BLOCK *b = (BLOCK *)malloc(sizeof(XWORD) * (size + 1));
  ASSERT(b, "out of memory - can not freeze block");
  b->h = objbits(x);
  PUSH_ON_CYCLE_STACK((X)b);

  if(is_specialblock(x)) {
    ++i;
    b->d[ 0 ] = slot_ref(x, 0);
  }

  *dest = (X)b;

#ifdef NO_CHECK_CYCLES
  if(i >= size) return;

  --size;
#endif

  while(i < size) {
    freeze_term_recursive(slot_ref(x, i), &(b->d[ i ]));
    ++i;
  }

#ifdef NO_CHECK_CYCLES
  x = slot_ref(x, i);
  dest = &(b->d[ i ]);
  goto restart;
#else
  POP_CYCLE_STACK;
#endif
}


static X freeze_term(X x)
{
  INIT_CYCLE_STACK;
  X y;
  freeze_term_recursive(x, &y);
  clear_shared_term_table();
  return y;
}


// copy object back into GC'd memory - this will return 0, if heap-space is insufficient
// note: also re-interns symbols (atoms)
// another note: previously stored compound items will not be identical, with the exceptions of atoms
static int thaw_term_recursive(X *xp)
{
  X x;

 restart:
  x = *xp;

  if(is_FIXNUM(x) || x == END_OF_LIST_VAL) return 1;

  if(is_VAR(x)) {
    X *tp = lookup_shared_term(x, 1);

    if(*tp != NULL) {
      *xp = tp[ 1 ];
      return 1;
    }

    *tp = x;

    if(alloc_top + objsize(x) + 1 > fromspace_limit) return 0;
    
    tp[ 1 ] = *xp = make_var();
#ifdef USE_DELAY
    X al = slot_ref(x, 3);
    
    if(!thaw_term_recursive(&al)) return 0;

    SLOT_SET(*xp, 3, al);
#endif
    return 1;
  }

  if(is_byteblock(x)) {
    X *tp;

    if(is_STRING(x)) {
      tp = lookup_shared_term(x, 1);

      if(*tp != NULL) {
	*xp = tp[ 1 ];
	return 1;
      }

      *tp = x;
    }

    XWORD size = objsize(x);

    if(alloc_top + bytes_to_words(size + 1) > fromspace_limit)
      return 0;

    ALLOCATE_BYTEBLOCK(BYTEBLOCK *b, objtype(x), size);
    memcpy(b->d, objdata(x), size);

    if(is_STRING(x)) tp[ 1 ] = *xp = intern((X)b);
    else *xp = (X)b;

    return 1;
  }

#ifndef NO_CHECK_CYCLES
  for(X *ptr = cycle_stack; ptr < cycle_stack_top; ptr += 2) {	
    if(*ptr == x) {						
      *xp = ptr[ 1 ];						
      return 1; 
    }
  }
#endif

  CHECK_CYCLE_STACK;
  PUSH_ON_CYCLE_STACK(x);
  XWORD i = 0;
  XWORD size = objsize(x);

  if(alloc_top + size + 1 > fromspace_limit) return 0;

  ALLOCATE_BLOCK(BLOCK *b, objtype(x), size);
  PUSH_ON_CYCLE_STACK((X)b);
  
  // initialize, in case thawing elements fails
  for(i = 0; i < size; ++i)
    b->d[ i ] = ZERO;
  
  if(is_specialblock(x)) {
    i = 1;
    b->d[ 0 ] = slot_ref(x, 0);
  }
  else i = 0;

  *xp = (X)b;
#ifdef NO_CHECK_CYCLES
  if(i >= size) return 1;

  --size;
#endif

  while(i < size) {
    b->d[ i ] = slot_ref(x, i);
    
    if(!thaw_term_recursive(&(b->d[ i ]))) {
      b->d[ i ] = ZERO;
      return 0;
    }

    ++i;
  }

#ifdef NO_CHECK_CYCLES
  b->d[ i ] = slot_ref(x, i);
  xp = &(b->d[ i ]);
  goto restart;
#else
  POP_CYCLE_STACK;
#endif
  return 1;
}


static X thaw_term(X x, int *failed)
{
#ifdef PROFILE_MEMORY
  XWORD oldheap = where->heap.total;
#endif
  X y = x;
  INIT_CYCLE_STACK;
  *failed = !thaw_term_recursive(&y);
  clear_shared_term_table();

#ifdef PROFILE_MEMORY
  if(*failed) where->heap.total = oldheap;
#endif

  return y;
}


// delete frozen term, recursively
static void delete_term_recursive(X x)
{
  if(is_FIXNUM(x) || x == END_OF_LIST_VAL) return;

  // must be lookup up first, as it's header may already be destroyed
  X *tp = lookup_shared_term(x, 0);

  if(tp) return;
  
  if(is_VAR(x)) {
    tp = lookup_shared_term(x, 1);
    *tp = x;			/* just add val to mark it */
    XFREE(x, objsize(x) * sizeof(X) + sizeof(XWORD));
    return;
  }

  XWORD size = objsize(x);

  if(is_byteblock(x)) {
    if(is_STRING(x)) {
      tp = lookup_shared_term(x, 1);
      *tp = x;			/* s.a. */
    }
      
    XFREE(x, size + sizeof(XWORD));
    return;
  }

  XWORD i = 0;

  if(is_specialblock(x)) i = 1;

  while(i < size) {
    delete_term_recursive(slot_ref(x, i));
    ++i;
  }

  XFREE(x, size * sizeof(X) + sizeof(XWORD));
}


static void delete_term(X x)
{
  delete_term_recursive(x);
  clear_shared_term_table();
}


static DB *create_db(char *name, int namelen, XWORD tablesize)
{
  DB *db = malloc(sizeof(DB));
  ASSERT(db, "out of memory - can not create database");
  db->name = xstrndup(name, namelen);
  db->tablesize = tablesize;
  db->table = malloc(sizeof(DB_BUCKET *) * tablesize);
  ASSERT(db->table, "out of memory - can not allocate database table");

  for(XWORD i = 0; i < tablesize; ++i)
    db->table[ i ] = NULL;

  return db;
}


static DB_ITEM *db_insert_item(DB *db, char *key, int keylen, X val, int atend)
{
  XWORD hash = hash_name(key, keylen) % db->tablesize;
  DB_ITEM *item = malloc(sizeof(DB_ITEM));
  ASSERT(item, "out of memory - can not allocate db-item");
  item->val = freeze_term(val);
  item->erased = 0;
  item->next_deleted = NULL;

  for(DB_BUCKET *bucket = db->table[ hash ]; bucket != NULL; bucket = bucket->next) {
    if(bucket->keylen == keylen && !strncmp(key, bucket->key, keylen)) {
      item->bucket = bucket;

      if(atend) {
	item->previous = bucket->lastitem;
	item->next = NULL;
	bucket->lastitem->next = item;
	bucket->lastitem = item;
      }
      else {
	item->previous = NULL;
	item->next = bucket->firstitem;
	bucket->firstitem->previous = item;
	bucket->firstitem = item;
      }

      return item;
    }
  }

  DB_BUCKET *bucket = malloc(sizeof(DB_BUCKET));
  ASSERT(bucket, "out of memory - can not allocate db-bucket");
  bucket->db = db;
  bucket->index = hash;
  bucket->key = xstrndup(key, keylen);
  bucket->keylen = keylen;
  bucket->firstitem = item;
  item->next = NULL;
  item->previous = NULL;
  item->bucket = bucket;
  bucket->lastitem = item;
  bucket->previous = NULL;
  bucket->next = db->table[ hash ];
  db->table[ hash ] = bucket;
  return item;
}


static DB_ITEM *db_find_first_item(DB *db, char *key, int keylen)
{
  XWORD hash = hash_name(key, keylen) % db->tablesize;

  for(DB_BUCKET *bucket = db->table[ hash ]; bucket != NULL; bucket = bucket->next) {
    if(bucket->keylen == keylen && !strncmp(key, bucket->key, keylen)) {
      DB_ITEM *item = bucket->firstitem; 

      while(item && item->erased) item = item->next;

      return item;
    }
  }

  return NULL;
}


static DB_BUCKET *db_enumerate_buckets(DB *db, DB_BUCKET *prev)
{
  int i;

  if(prev != NULL) {
    if(prev->next != NULL) return prev->next;

    i = prev->index + 1;
  }
  else i = 0;
  
  while(i < db->tablesize) {
    if(db->table[ i ] == NULL) ++i;
    else return db->table[ i ];
  }
  
  return NULL;
}


static void db_mark_item_as_erased(DB_ITEM *item)
{
  if(item->erased) return;

  item->erased= 1;
  item->next_deleted = deleted_db_items;
  deleted_db_items = item;
}


static void db_mark_bucket_as_erased(DB_ITEM *item)
{
  DB_BUCKET *bucket = item->bucket;
  item = bucket->firstitem;	/* any item will do */

  // mark all items as erased
  while(item != NULL) {
    db_mark_item_as_erased(item);
    item = item->next;
  }
}


static void db_erase_item(DB_ITEM *item)
{
  DB_BUCKET *bucket = item->bucket;
  ASSERT(item->erased, "attempt to delete db-item that is not marked as erased");
  delete_term(item->val);

  if(bucket->firstitem == item) {
    // erase bucket, if this is the last item in it
    if(bucket->lastitem == item) {
      if(bucket->previous) bucket->previous->next = bucket->next;
    
      if(bucket->next) bucket->next->previous = bucket->previous;

      bucket->db->table[ bucket->index ] = NULL;
      bucket->previous = bucket->next = NULL;
      XFREE(bucket->key, bucket->keylen);
      XFREE(bucket, sizeof(DB_BUCKET));
    }
    else {
      // first item in bucket
      if(item->next) item->next->previous = NULL; 

      bucket->firstitem = item->next;
    }
  }
  else {
    // otherwise remove from item-chain
    item->previous->next = item->next; /* it's not the first item */

    if(bucket->lastitem == item)
      bucket->lastitem = item->previous;
  
    if(item->next) 
      item->next->previous = item->previous;
  }

  item->next = item->previous = NULL;
  item->bucket = NULL;
  XFREE(item, sizeof(DB_ITEM));
}


/// cycle checking

static int check_cycles_recursive(X x)
{
  if(is_FIXNUM(x) || x == END_OF_LIST_VAL) return 0;

  if(is_byteblock(x)) return 0;

  for(X *ptr = cycle_stack; ptr < cycle_stack_top; ++ptr) {
    if(*ptr == x) return 1;	/* value was already traversed */
  }

  if(is_VAR(x)) {
    X y = slot_ref(x, 0);

    if(x == y) return 0;

    //XXX just set x to y and fall through?
    ASSERT(cycle_stack_top + 1 < cycle_stack + CYCLE_STACK_SIZE,
	   "cycle-stack overflow");
    *(cycle_stack_top++) = y;
    int r = check_cycles_recursive(y);
    --cycle_stack_top;
    return r;
  }

  ASSERT(cycle_stack_top + 1 < cycle_stack + CYCLE_STACK_SIZE,
	 "cycle-stack overflow");
  *(cycle_stack_top++) = x;
  XWORD size = objsize(x);
  XWORD i = 0;

  if(is_specialblock(x)) i = 1;

  while(i < size) {
    if(check_cycles_recursive(slot_ref(x, i))) 
      return 1;

    ++i;
  }  

  --cycle_stack_top;
  return 0;
}


static int check_cycles(X term)
{
  cycle_stack_top = cycle_stack;
  return check_cycles_recursive(term);
}


/// groundness checking

static int check_ground_recursive(X x)
{
  if(is_FIXNUM(x) || x == END_OF_LIST_VAL) return 1;

  if(is_byteblock(x)) return 1;

#ifndef NO_CHECK_CYCLES
  for(X *ptr = cycle_stack; ptr < cycle_stack_top; ++ptr) {
    if(*ptr == x) return 1;	/* value was already traversed */
  }
#endif

  if(is_VAR(x)) {
    X y = slot_ref(x, 0);

    if(x == y) return 0;
   
    return check_ground_recursive(y);
  }

  CHECK_CYCLE_STACK;
  PUSH_ON_CYCLE_STACK(x);
  PUSH_ON_CYCLE_STACK(NULL);	/* not used */
  XWORD size = objsize(x);
  XWORD i = 0;

  if(is_specialblock(x)) i = 1;

  while(i < size) {
    if(!check_ground_recursive(slot_ref(x, i))) 
      return 0;

    ++i;
  }  

  POP_CYCLE_STACK;
  return 1;
}


static int check_ground(X term)
{
  INIT_CYCLE_STACK;
  return check_ground_recursive(term);
}


/// garbage collection

static void mark(X *addr) 
{
  XWORD h = ((BLOCK *)(*addr))->h;

  if((h & GC_MARK_BIT) != 0) {
    *addr = fptr_to_ptr(h);
    return;
  }

  XWORD size = h & HEADER_SIZE_MASK;

  if((h & BYTEBLOCK_MARK_BIT) == 0) 
    size *= sizeof(XWORD);
  else
    size = ALIGN(size);

  XWORD t = TAG_TO_TYPE(h);

#ifndef SIXTYFOUR
  if(t == FLONUM_TYPE && !FPALIGNED(tospace_top))
    *(tospace_top++) = ALIGNMENT_HOLE_MARKER;
#endif

  if(t == DBREFERENCE_TYPE && slot_ref(*addr, 1) == ONE) {
    // increase refcount if DB-reference, to detect unreferenced ones that can be completely deleted
    DB_ITEM *item = (DB_ITEM *)slot_ref(*addr, 0);
    ++item->refcount;
  }

  X nptr = tospace_top;
  XWORD fptr = ptr_to_fptr(tospace_top);
  memcpy(tospace_top, *addr, size + sizeof(XWORD));
  tospace_top = (X)((XWORD)tospace_top + size + sizeof(XWORD));
  ((BLOCK *)(*addr))->h = fptr;
  *addr = nptr;
}


static inline void mark1(X *addr)  
{ 
  XWORD p = (XWORD)(*addr);

  if(IS_TAINTED_PTR(p))
    CRASH("access of tainted pointer");
  
  if((p & FIXNUM_MARK_BIT) == 0 && IS_IN_HEAP(*addr))
    mark(addr); 
}


static void collect_garbage(CHOICE_POINT *C)
{
  DRIBBLE(gc_caused_by_trailing ? "[GC (due to trailing) ... " : "[GC ... ");
  gc_caused_by_trailing = 0;
  tospace_top = tospace; 
  scan_ptr = tospace_top;				

  // clear ref-counts of all deleted DB items
  for(DB_ITEM *item = deleted_db_items; item != NULL; item = item->next_deleted)
    item->refcount = 0;

  // mark local environments
  for(X *p = environment_stack; p < env_top; ++p)
    mark1(p);

  // mark argument stack
  for(X *p = argument_stack; p < arg_top; ++p)
    mark1(p);

  // mark global variables
  for(int i = 0; i < global_variable_counter; ++i)
    mark1(&(global_variables[ i ]));

  // mark topmost exception value in catcher stack
  if(catch_top > catch_stack)
    mark1(&((catch_top - 1)->ball));

#ifdef USE_DELAY
  // mark triggered frozen goals list
  mark1(&triggered_frozen_goals);

  // mark frozen-goals slots in trail stack
  for(X *tp = trail_stack; tp < trail_top; tp += 2) {
    if(tp[ 0 ] != END_OF_LIST_VAL) mark1(&(tp[ 0 ]));
  }
#endif

  // mark special symbols
  mark1(&dot_atom);
  mark1(&system_error_atom);
  mark1(&type_error_atom);
  mark1(&evaluation_error_atom);
  mark1(&instantiation_error_atom);
  mark1(&user_interrupt_atom);
  mark1(&end_of_file_atom);
  mark1(&user_input_atom);
  mark1(&user_output_atom);
  mark1(&user_error_atom);
  mark1(&current_input_atom);
  mark1(&current_output_atom);

  // mark standard ports
  mark1(&standard_input_port);
  mark1(&standard_output_port);
  mark1(&standard_error_port);

  while(scan_ptr < tospace_top) {						
#ifndef SIXTYFOUR
    if(((XWORD)scan_ptr & 7) == 0 && *scan_ptr == ALIGNMENT_HOLE_MARKER) 
      ++scan_ptr; 
#endif

    XWORD h = (XWORD)(*scan_ptr);						
    XWORD size = h & HEADER_SIZE_MASK;					

    // DRIBBLE("H: %08lx\n", h);

    if((h & BYTEBLOCK_MARK_BIT) != 0) 
      scan_ptr += bytes_to_words(size) + 1;	
    else {
      ++scan_ptr;		/* skip header */

      if((h & SPECIALBLOCK_MARK_BIT) != 0) {
	++scan_ptr;		/* skip ptr */
	--size;
      }

      while(size-- > 0)
	mark1(scan_ptr++);

      ASSERT(scan_ptr < tospace_end, "scan_ptr exceeded fromspace");
    }
  }

  // drop trail-stack items that refer to unreferenced variables
  TRAIL_STACK_GAP *gp = trail_stack_gap_buffer;
  int gap_size = 0;
  X *tsp = trail_stack;
#ifdef USE_DELAY
  int slots = 2;
#else
  int slots = 1;
#endif

  // first collect "gaps" of unref'd variables in trail-stack
  for(X *tp = trail_stack; tp < trail_top; tp += slots) {
#ifdef USE_DELAY
    X var = tp[ 1 ];
#else
    X var = *tp;
#endif
    int offset = gp - trail_stack_gap_buffer;

    if(offset >= trail_stack_gap_buffer_size) {
      trail_stack_gap_buffer_size *= 2;
      trail_stack_gap_buffer = realloc(trail_stack_gap_buffer, 
				       trail_stack_gap_buffer_size * sizeof(TRAIL_STACK_GAP));
      ASSERT(trail_stack_gap_buffer, 
	     "out of memory - can not re-allocate trail-stack gap-buffer");
      gp = trail_stack_gap_buffer + offset;
    }

    if(is_forwarded(var)) {
      if(gap_size) {		// forwarded var? close existing gap
	gp->size = gap_size;
	++gp;
	gap_size = 0;
      }

#ifdef USE_DELAY
      *(tsp++) = tp[ 0 ];	// frozen goals
#endif
      *(tsp++) = fptr_to_ptr(objbits(var));
    }
    else if(gap_size) {		// collected var? add to gap or...
      gap_size += slots;
    }
    else {			// ...create new gap
      gp->position = tp;
      gap_size = slots;
    }
  }

  if(gap_size) {		// close gap, if one is still open
    gp->size = gap_size;
    ++gp;
  }

  // now adjust trail-ptrs in choice-point stack
  int ts_shift = 0;
  TRAIL_STACK_GAP *gp2 = trail_stack_gap_buffer;

  for(CHOICE_POINT *cp = choice_point_stack; cp < C; ++cp) {
    while(gp2 < gp && cp->T >= gp2->position + gp2->size) {
      // add gap-size to shift, if cp->T is above gap
      ts_shift += gp2->size;
      ++gp2;
    }

    X *cpt = cp->T;		// remember
    cp->T -= ts_shift;
    
    // if cp->T is inside gap, only reduce by offset into it
    if(gp2 < gp && cpt > gp2->position && cpt < gp2->position + gp2->size)
      cp->T -= cpt - gp2->position;

    ASSERT(cp->T >= trail_stack, "trail-pointer underflow after shift");
    ASSERT(cp == choice_point_stack || cp->T >= (cp - 1)->T, 
	   "trail-pointer in CP below expected value");
  }

  // do the same for trail-ptrs in catcher-stack
  ts_shift = 0;
  gp2 = trail_stack_gap_buffer;

  for(CATCHER *cp = catch_stack; cp < catch_top; ++cp) {
    while(gp2 < gp && cp->T > gp2->position) {
      ts_shift += gp2->size;
      ++gp2;
    }

    cp->T -= ts_shift;
  }

  if(tsp < trail_top)
    DRIBBLE("trail-stack reduced by " XWORD_OUTPUT_FORMAT " ... ", (XWORD)(trail_top - tsp));

  trail_top = tsp;

  // remove unforwarded symbols from symbol-table
  int gcdsyms = 0;

  for(int i = 0; i < SYMBOL_TABLE_SIZE; ++i) {
    X prevsym = END_OF_LIST_VAL;
    X sym = symbol_table[ i ];

    while(sym != END_OF_LIST_VAL) {
      if(!IS_IN_HEAP(sym)) break;	// all further symbols in this chain will be static

      if(is_forwarded(sym)) {
	sym = fptr_to_ptr(objbits(sym));
	X next = slot_ref(sym, 1);
	SLOT_SET(sym, 1, END_OF_LIST_VAL);

	if(prevsym != END_OF_LIST_VAL) 
	  SLOT_SET(prevsym, 1, sym);
	else
	  symbol_table[ i ] = sym;

	prevsym = sym;
	sym = next;
      }
      else {
	// DRIBBLE("reclaimed symbol: \'%s\'\n", (XCHAR *)objdata(slot_ref(sym, 0)));
	++gcdsyms;
	sym = slot_ref(sym, 1);
      }
    }

    if(prevsym != END_OF_LIST_VAL)
      SLOT_SET(prevsym, 1, sym);
    else
      symbol_table[ i ] = sym;
  }

  if(gcdsyms > 0)
    DRIBBLE("%d symbol(s) reclaimed ... ", gcdsyms);

  void *tmp = fromspace; 
  fromspace = tospace; 
  tospace = tmp;		
  tmp = fromspace_end; 
  fromspace_end = tospace_end;
  tospace_end = tmp;	
  fromspace_limit = (X)((char *)fromspace_end - heap_reserve);	
  alloc_top = tospace_top;
  XWORD total = (XWORD)fromspace_end - (XWORD)fromspace;
  XWORD used = (XWORD)alloc_top - (XWORD)fromspace;
  DRIBBLE("finished (" XWORD_OUTPUT_FORMAT "%% in use, T: " XWORD_OUTPUT_FORMAT
	  ", C: " XWORD_OUTPUT_FORMAT ", A: " XWORD_OUTPUT_FORMAT ", E: " 
	  XWORD_OUTPUT_FORMAT ")]\n",
	  (XWORD)((double)used / total * 100),
	  (XWORD)trail_top - (XWORD)trail_stack,
	  (XWORD)C - (XWORD)choice_point_stack,
	  (XWORD)arg_top - (XWORD)argument_stack,
	  (XWORD)env_top - (XWORD)environment_stack);
  ++gc_count;
  
  if(alloc_top >= fromspace_limit) 
    CRASH("heap exhausted");				

  // check finalizers
  FINALIZER *prev = NULL;
  FINALIZER *fp = active_finalizers; 

  while(fp != NULL) {
    XWORD h = objbits(fp->object);

    if((h & GC_MARK_BIT) != 0) {
      // was marked, update object ptr
      fp->object = fptr_to_ptr(h);
      prev = fp;
      fp = fp->next;
    }
    else {
      // was reclaimed, remove from list, run and add to free_finalizers
      DRIBBLE("[running finalizer on %s %p]\n", type_name(TAG_TO_TYPE(h)), fp->object);

      if(prev != NULL)
	prev->next = fp->next;
      else
	active_finalizers = fp->next;

      FINALIZER *fp2 = fp->next;
      fp->next = free_finalizers;
      free_finalizers = fp;
      fp->finalizer(fp->object);
      fp = fp2;
    }
  }

  // really erase DB items that are not referenced
  DB_ITEM *previtem = NULL;
  int deleted = 0;
  DB_ITEM *item = deleted_db_items; 

  while(item != NULL) {
    DB_ITEM *next = item->next_deleted;
    ASSERT(item->erased, "unerased db-item in deleted items list");

    if(item->refcount == 0) {
      if(previtem) 
	previtem->next_deleted = next;
      else
	deleted_db_items = next;

      db_erase_item(item);
      ++deleted;
    }
    else previtem = item;

    item = next;
  }

  if(deleted > 0)
    DRIBBLE("[%d db-items deleted]\n", deleted);

#ifdef DEBUG_GC
  memset(tospace, TAINTED_PTR_B & 0xff, (XWORD)tospace_end - (XWORD)tospace);
  memset(alloc_top, TAINTED_PTR_A & 0xff, (XWORD)fromspace_end - (XWORD)alloc_top);
#endif
}


static void set_finalizer(X x, void (*fn)(X))
{
  FINALIZER *fp = free_finalizers;

  if(!fp) fp = malloc(sizeof(FINALIZER));
  else free_finalizers = fp->next;

  fp->next = active_finalizers;
  fp->finalizer = fn;
  fp->object = x;
  active_finalizers = fp;
}


/// Time

static inline XFLOAT get_time()
{
#if defined(__linux__)
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return (XFLOAT)ts.tv_sec + (XFLOAT)ts.tv_nsec / 1000000000.0;
#elif defined(__OpenBSD__) || defined(__FreeBSD__) || defined(__NetBSD__) || defined(__DragonFly__)
  struct timespec ts;
  clock_gettime(CLOCK_REALTIME, &ts);
  return (XFLOAT)ts.tv_sec + (XFLOAT)ts.tv_nsec / 1000000000.0;
#elif defined(_WIN32)
  LARGE_INTEGER ts;
  QueryPerformanceCounter(&ts);
  return (XFLOAT)ts.QuadPart / (XFLOAT)win32_perf_freq.QuadPart;
#elif defined(__APPLE__) && defined(__MACH__)
  XWORD tm = mach_absolute_time();
  mach_timebase_info_data_t tr;
  mach_timebase_info(&tr);
  return (XFLOAT)tm * (XFLOAT)tr.numer / (XFLOAT)tr.denom / 1000000000.0;
#else
  system_error("clock not available for this platform");
  return 0;
#endif
}


/// runtime initialization and abnormal exit


static XWORD numeric_arg(char *arg)
{
  int len = strlen(arg);
  XWORD m = 1L;

  switch(toupper(arg[ len - 1 ])) {
  case 'K': m = 1024L; break;
  case 'M': m = 1024L * 1024L; break;
  case 'G': m = 1024L * 1024L * 1024L; break;
#ifdef SIXTYFOUR
  case 'T': m = (XWORD)1024L * 1024L * 1024L * 1024L; break;
#endif
  default: return atol(arg);
  }

  arg[ len - 1 ] = '\0';
  return m * atol(arg);
}


static void initialize(int argc, char *argv[])
{
#ifdef _WIN32
  QueryPerformanceFrequency(&win32_perf_freq);
#endif

  heap_size = HEAP_SIZE;
  environment_stack_size = ENVIRONMENT_STACK_SIZE;
  ifthen_stack_size = IFTHEN_STACK_SIZE;
  choice_point_stack_size = CHOICE_POINT_STACK_SIZE;
  trail_stack_size = TRAIL_STACK_SIZE;
  argument_stack_size = ARGUMENT_STACK_SIZE;
  shared_term_table_size = SHARED_TERM_TABLE_SIZE / sizeof(X);
  global_argc = argc;
  global_argv = argv;
  debugging = 0;
  verbose = 0;
  srand((unsigned int)time(NULL));

#if defined(PROFILE) || defined(PROFILE_MEMORY)
  where = &system_pinfo;
#endif

  // scan argv for runtime-parameters
  for(int i = argc - 1; i > 0; --i) {
    char *arg = argv[ i ];

    if(*arg == '-' && arg[ 1 ] == ':') {
      switch(arg[ 2 ]) {
      case 'h':
	heap_size = numeric_arg(arg + 3);
	break;

      case 'v':
	verbose = 1;
	break;

      case 'm':
	mmapped_heap = arg + 3;
	break;

      case 'd':
	debugging = 1;
	break;

      case 'A':
	argument_stack_size = numeric_arg(arg + 3);
	break;

      case 'E':
	environment_stack_size = numeric_arg(arg + 3);
	break;

      case 'C':
	choice_point_stack_size = numeric_arg(arg + 3);
	break;

      case 'T':
	trail_stack_size = numeric_arg(arg + 3);
	break;

      case 'S':
	shared_term_table_size = numeric_arg(arg + 3);
	break;

	// no option for ifthen-stack, in the moment

      default:
	COUTPUT("WARNING: invalid runtime option \"%s\" (ignored)\n", arg);
      }
    }
  }

  DRIBBLE("[allocating heap of size 2 x " XWORD_OUTPUT_FORMAT "]\n", heap_size / 2);
  heap_reserve = heap_size * HEAP_RESERVE / 100.0;

#ifndef _WIN32
  if(mmapped_heap) {
    mmapped_fd = open(mmapped_heap, O_RDWR | O_CREAT, S_IRUSR | S_IWUSR);

    if(mmapped_fd == -1) 
      CRASH("unable to open heap file: %s", strerror(errno));

    if(ftruncate(mmapped_fd, heap_size) == -1)
      CRASH("unable to create heap file: %s", strerror(errno));

    mmapped_buffer = mmap(NULL, heap_size, PROT_READ | PROT_WRITE, MAP_SHARED, mmapped_fd, 0);

    if(mmapped_buffer == MAP_FAILED) 
      CRASH("unable to map heap file into memory: %s", strerror(errno));

    fromspace = mmapped_buffer;
    tospace = (X *)((XWORD)fromspace + heap_size / 2);
  }
  else
#endif
  {
    fromspace = malloc(heap_size / 2);
    tospace = malloc(heap_size / 2);
    ASSERT(fromspace && tospace, "can not allocate heap");
  }

  fromspace_end = (X *)((XWORD)fromspace + heap_size / 2);
  tospace_end = (X *)((XWORD)tospace + heap_size / 2);
  fromspace_limit = (X *)((XWORD)fromspace_end - heap_reserve);
  alloc_top = fromspace;
  default_input_port.fp = stdin;
  default_output_port.fp = stdout;
  default_error_port.fp = stderr;
  environment_stack = malloc(environment_stack_size);
  ASSERT(environment_stack, "out of memory - can not allocate environment stack");
  trail_stack = malloc(trail_stack_size);
  ASSERT(trail_stack, "out of memory - can not allocate environment stack");
  trail_stack_reserve = trail_stack_size * TRAIL_STACK_RESERVE / 100.0;
  trail_stack_limit = trail_stack + (trail_stack_size - trail_stack_reserve) / sizeof(X *);
  trail_top = trail_stack;
  ifthen_stack = malloc(ifthen_stack_size);
  ASSERT(ifthen_stack, "out of memory - can not allocate if-then stack");
  choice_point_stack = malloc(choice_point_stack_size);
  ASSERT(choice_point_stack, "out of memory - can not allocate choice-point stack");
  argument_stack = malloc(argument_stack_size);
  ASSERT(argument_stack, "out of memory - can not allocate argument stack");
  ifthen_top = ifthen_stack;
  env_top = environment_stack;
  arg_top = argument_stack;
  string_buffer = malloc(string_buffer_length = STRING_BUFFER_SIZE);
  ASSERT(argument_stack, "out of memory - can not allocate string buffer");
  shared_term_table = malloc(shared_term_table_size * 2 * sizeof(X));
  ASSERT(shared_term_table, "out of memory - can not allocate shared term table");
  memset(shared_term_table, 0, shared_term_table_size * 2 * sizeof(X));
  shared_term_table_positions = malloc(shared_term_table_size * sizeof(XWORD));
  ASSERT(shared_term_table, "out of memory - can not allocate shared term positions table");
  trail_stack_gap_buffer = malloc(TRAIL_STACK_GAP_BUFFER_SIZE * sizeof(TRAIL_STACK_GAP));
  ASSERT(trail_stack_gap_buffer, "out of memory - can not allocate initial trail-stack gap buffer");
  cycle_stack = malloc(CYCLE_STACK_SIZE * sizeof(X));
  ASSERT(cycle_stack, "out of memory - can not allocate cycle-stack");

  for(int i = 0; i < SYMBOL_TABLE_SIZE; ++i)
    symbol_table[ i ] = END_OF_LIST_VAL;

  for(int i = 0; i < MAX_GLOBAL_VARIABLES; ++i)
    global_variables[ i ] = ZERO;

  initial_global_variable_count = global_variable_counter;
  standard_input_port = (X)(&default_input_port);
  standard_output_port = (X)(&default_output_port);
  standard_error_port = (X)(&default_error_port);
  catch_top = catch_stack;
  deleted_db_items = NULL;
  gc_count = 0;
  active_finalizers = free_finalizers = NULL;
  clock_ticks = 0;
  shared_term_counter = 0;
  variable_counter = 0;
  triggered_frozen_goals = END_OF_LIST_VAL;
  first_distinct_variable = NULL;
  signal(SIGINT, signal_handler);

#ifdef DEBUG_GC
  memset(tospace, TAINTED_PTR_B & 0xff, (XWORD)tospace_end - (XWORD)tospace);
  memset(fromspace, TAINTED_PTR_A & 0xff, (XWORD)fromspace_end - (XWORD)fromspace);
#endif
}


#ifdef PROFILE
static void *profile_thread_start(void *arg)
{
  struct timespec wt = { 0, PROFILE_PERIOD }; /* XXX ??? */
  struct timespec rt, rt2;

  while(!finish_profiling) {
    //XXX needs windows variant
    //XXX does mac os have nanosleep(2)? probably not...
#ifdef __linux__
    clock_gettime(CLOCK_MONOTONIC, &rt);
    rt.tv_nsec += wt.tv_nsec;

    while(clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME, &rt, NULL) != 0) {
      if(errno != EINTR) break;
    }
#else
    if(nanosleep(&wt, &rt) != 0) {
      while(nanosleep(&rt, &rt2) != 0) {
	rt = rt2;
      } 
    }
#endif

    ++where->count;
    ++total_counts;
  }

  return NULL;
}
#endif


#if defined(PROFILE) || defined(PROFILE_MEMORY)
static void profile_init(PINFO *list)
{
  pinfo_list = list;

#ifdef PROFILE  
  start_time = get_time();
  total_counts = 0;
  finish_profiling = 0;

  if(pthread_create(&profile_thread, NULL, profile_thread_start, NULL) != 0)
    CRASH("can not create profiling thread");
#endif
}


#ifdef PROFILE_MEMORY
static XCHAR *poutput_polish(XWORD val)
{
  static XCHAR buf[ 32 ];
  
  if(val < 1000) sprintf(buf, "%6" XWORD_OUTPUT_FORMAT_LENGTH "d ", val);
  else if(val < 1000000) sprintf(buf, "%6" XWORD_OUTPUT_FORMAT_LENGTH "dK", val / 1024);
  else sprintf(buf, "%6.2gM", (XFLOAT)val / (1024 * 1024));

  return buf;
}


static XCHAR *poutput_avg_polish(volatile PINFO_AVG_COUNT *c, int count)
{
  if(count == 0) return "     0 ";

  return poutput_polish(c->total / count);
}
#endif


static void profile_emit_data()
{
#ifdef PROFILE
  finish_profiling = 1;

  if(pthread_join(profile_thread, NULL) != 0)
    CRASH("waiting for profiling thread failed");

  XFLOAT total_time = get_time() - start_time;
#endif
  int pid = getpid();
  sprintf(string_buffer, "PROFILE.%d", pid);

#ifdef PROFILE
  DRIBBLE("[generating %s - total time: %.2g seconds, " XWORD_OUTPUT_FORMAT " counts]\n", 
	  string_buffer, total_time, total_counts);
#elif defined(PROFILE_MEMORY)
  DRIBBLE("[generating memory profile %s]\n", string_buffer);
#endif
  FILE *fp = fopen(string_buffer, "w");

  if(fp == NULL)
    CRASH("can not open file with profile data");
  
  //XXX move pinfos into table and sort
  for(PINFO *pinfo = pinfo_list; pinfo != NULL; pinfo = pinfo->next) {
#ifdef PROFILE_MEMORY
    if(pinfo->count > 0 || pinfo->heap.total > 0) {
#else
    if(pinfo->count > 0) {
#endif
      XCHAR *name = pinfo->name;
      int len = strlen(name);
      fputs(name, fp);

      while(len < 40) {
	fputc(' ', fp);
	++len;
      }

#ifdef PROFILE    
      XFLOAT tm = (XFLOAT)pinfo->count / total_counts * total_time;
      fprintf(fp, " %5d  %6.2g  %%%d\n", (int)pinfo->count, 
	      tm < 0.009 ? 0 : tm,
	      (int)((XFLOAT)pinfo->count / total_counts * 100));
#else
      fprintf(fp, " C:%s", poutput_avg_polish(&pinfo->cp, pinfo->count));
      fprintf(fp, "  T:%s", poutput_avg_polish(&pinfo->tp, pinfo->count));
      fprintf(fp, "  E:%s", poutput_avg_polish(&pinfo->ep, pinfo->count));
      fprintf(fp, "  H:%s\n", poutput_polish(pinfo->heap.total));
#endif
    }
  }

  fclose(fp);
}
#endif


static void cleanup()
{
#if defined(PROFILE) || defined(PROFILE_MEMORY)
  profile_emit_data();
#endif
#ifndef _WIN32
  if(mmapped_heap) {
    munmap(mmapped_buffer, heap_size);
    close(mmapped_fd);
  }
  else 
#endif
  {
    free(fromspace);
    free(tospace);
  }

  free(ifthen_stack);
  free(choice_point_stack);
  free(trail_stack);
  free(argument_stack);
  free(environment_stack);
  free(string_buffer);
  free(shared_term_table);
  free(shared_term_table_positions);
}


static void terminate(CHOICE_POINT *C, int code)
{
  if(active_finalizers) {
    DRIBBLE("[running finalizers ...]\n");
    
    for(FINALIZER *fp = active_finalizers; fp != NULL; fp = fp->next)
      fp->finalizer(fp->object);
  }

  DRIBBLE("[terminating - T: " XWORD_OUTPUT_FORMAT ", C: " XWORD_OUTPUT_FORMAT
	  ", A: " XWORD_OUTPUT_FORMAT ", E: " XWORD_OUTPUT_FORMAT "]\n",
	  (XWORD)trail_top - (XWORD)trail_stack,
	  (XWORD)C - (XWORD)choice_point_stack,
	  (XWORD)arg_top - (XWORD)argument_stack,
	  (XWORD)env_top - (XWORD)environment_stack);
  cleanup();
  exit(code);
}


// debugging and tracing support

static void write_hook(X x)
{
  basic_write_term(stderr, 1, TRACE_DEBUG_WRITE_LIMIT, 1, deref(x));
  fputc('\n', stderr);
}


static void dump_symbol_table()
{
  for(int i = 0; i < SYMBOL_TABLE_SIZE; ++i) {
    int f = 0;

    for(X sym = symbol_table[ i ]; sym != END_OF_LIST_VAL; sym = slot_ref(sym, 1)) {
      X name2 = slot_ref(sym, 0);

      if(!f) {
	fprintf(stderr, "\n%d: ", i);
	f = 1;
      }

      fprintf(stderr, "'%s' ", (XCHAR *)objdata(name2));
    }
  }

  fputc('\n', stderr);
}


static void trace_write(char *title, char *name, int arity, X *A, CHOICE_POINT *C)
{
  FILE *fp = port_file(standard_error_port);

  fflush(port_file(standard_output_port));
  fprintf(fp, "[(%d/%d/%d) %s: %s", (int)(C - choice_point_stack), 
	  (int)(arg_top - argument_stack), (int)(trail_top - trail_stack),
	  title, name);

  if(arity > 0) {
    fputc('(', fp);

    for(int i = 0; i < arity; ++i) {
      if(i > 0)
	fputs(", ", fp);

      basic_write_term(fp, 1, TRACE_DEBUG_WRITE_LIMIT, 1, deref(A[ i ]));
    }
    
    fputc(')', fp);
  }

  fputs("]\n", fp);
}


#ifdef TRACE
# define TRACE_ENTER(name, arity)  { if(debugging) trace_write("CALL", name, arity, C0->A, C); }
# define TRACE_REDO(name, arity)   { if(debugging) trace_write("REDO", name, arity, C0->A, C); }
# define TRACE_EXIT(name, arity)   { if(debugging) trace_write("EXIT", name, arity, C0->A, C); }
# define TRACE_FAIL(name, arity)   { if(debugging && C0->P == NULL) trace_write("FAIL", name, arity, C0->A, C); }
# define TRACE_TAIL_CALL(name, arity)  { if(debugging) trace_write("TAIL", name, arity, C0->A, C); }
#else
# define TRACE_ENTER(name, arity)
# define TRACE_REDO(name, arity)
# define TRACE_EXIT(name, arity)
# define TRACE_FAIL(name, arity)
# define TRACE_TAIL_CALL(name, arity)
#endif


/// unification

// trigger frozen goal on freshly bound variable
static inline void trigger_frozen_goal(X var)
{
  X frozen = slot_ref(var, 3);
  ASSERT(frozen != END_OF_LIST_VAL, "no frozen goals in triggered variable");
  X prev;

  // find end of triggered goal list
  for(prev = triggered_frozen_goals; prev != END_OF_LIST_VAL; prev = slot_ref(prev, 1)) {
    if(slot_ref(prev, 1) == END_OF_LIST_VAL) break;
  }

  int count = 0;

  // copy list of frozen goals
  while(frozen != END_OF_LIST_VAL) {
    X p = PAIR(slot_ref(frozen, 0), END_OF_LIST_VAL);
    ++count;

    if(prev != END_OF_LIST_VAL) 
      SLOT_SET(prev, 1, p);
    else
      triggered_frozen_goals = p;

    prev = p;
    frozen = slot_ref(frozen, 1);
  }

  SLOT_SET(var, 3, END_OF_LIST_VAL);

  DBG_DRIBBLE("[%d frozen goal(s) triggered on _" XWORD_OUTPUT_FORMAT "]\n", 
	      count, fixnum_to_word(slot_ref(var, 1)));
}


static void *next_frozen_goal(X *arglist)
{
  ASSERT(triggered_frozen_goals != END_OF_LIST_VAL, "no triggered frozen_goals");
  X frozen = triggered_frozen_goals;
  triggered_frozen_goals = slot_ref(frozen, 1);
  X a1 = slot_ref(frozen, 0);
  X prio = slot_ref(a1, 0);
  X a2 = slot_ref(a1, 1);

  if(prio == ZERO) return NULL;	/* was invalidated by detrailing */

  *arglist = slot_ref(a2, 1);
  return (void *)(slot_ref(slot_ref(a2, 0), 0));
}


#define unify(x, y)   ({ X _x = (x), _y = (y); _x == _y || unify1(C0, _x, _y); })
#define unify3(x, y)   ({ X _x = (x), _y = (y); _x == _y || unify2(C0, _x, _y); })

static int unify2(CHOICE_POINT *C0, X x, X y)
{
  x = deref(x);
  y = deref(y);

  if(is_FIXNUM(x) && is_FIXNUM(y)) return x == y;

  XWORD xt = is_FIXNUM(x) ? FIXNUM_TYPE : objtype(x);
  XWORD yt = is_FIXNUM(y) ? FIXNUM_TYPE : objtype(y);

  if(xt == VAR_TYPE) {
#ifdef DEBUGGING
    if(verbose) {
      DRIBBLE("[binding _" XWORD_OUTPUT_FORMAT " <- ", fixnum_to_word(slot_ref(x, 1)));
      basic_write_term(stderr, 1, 9999, 1, y);
      fputs("]\n", stderr);
    }
#endif

#ifdef USE_DELAY
    if(!first_distinct_variable)
      first_distinct_variable = x;

    if(slot_ref(x, 3) != END_OF_LIST_VAL) {
      SLOT_SET(x, 0, y);
      push_trail(C0, x);
      trigger_frozen_goal(x);
      return 1;
    }
#endif

    /*
      //XXX Careful! order of binding will influence bagof/setof - see comment
      //    in compile.pl (compile_argument_unification/8)

      if(yt == VAR_TYPE) {
        // bind younger variable in the hope that it doesn't need to be trailed
        if(fixnum_to_word(slot_ref(x, 2)) > fixnum_to_word(slot_ref(y, 2))) {
  	  SLOT_SET(x, 0, y);
	  push_trail(C0, x);
        }
        else {
	  SLOT_SET(y, 0, x);
	  push_trail(C0, y);
        }
      }

      // dropping the 2 lines below
    */

    SLOT_SET(x, 0, y);
    push_trail(C0, x);
    return 1;
  }

  if(yt == VAR_TYPE) {
#ifdef DEBUGGING
    if(verbose) {
      DRIBBLE("[binding _" XWORD_OUTPUT_FORMAT " <- ", fixnum_to_word(slot_ref(y, 1)));
      basic_write_term(stderr, 1, 9999, 1, x);
      fputs("]\n", stderr);
    }
#endif

#ifdef USE_DELAY
    if(!first_distinct_variable)
      first_distinct_variable = y;

    if(slot_ref(y, 3) != END_OF_LIST_VAL) {
      SLOT_SET(y, 0, x);
      push_trail(C0, y);
      trigger_frozen_goal(y);
      return 1;
    }
#endif

    SLOT_SET(y, 0, x);
    push_trail(C0, y);
    return 1;
  }

  if(xt != yt) return 0;
  
  if(xt == FIXNUM_TYPE) return 1;

  XWORD s = objsize(x);

  if(s != objsize(y)) return 0;

  if(is_byteblock(x))
    return !memcmp(objdata(x), objdata(y), s);

  int i = 0;

  if(is_specialblock(x)) ++i;

#ifndef NO_CHECK_CYCLES
  for(X *ptr = cycle_stack; ptr < cycle_stack_top; ptr += 2) {
    if(*ptr == x) return ptr[ 1 ] == y;
    else if(*ptr == y) return ptr[ 1 ] == y;
  }
#endif

  CHECK_CYCLE_STACK;
  PUSH_ON_CYCLE_STACK(x);
  PUSH_ON_CYCLE_STACK(y);

  while(i < s) {
    if(!unify3(slot_ref(x, i), slot_ref(y, i)))
      return 0;

    ++i;
  }

  POP_CYCLE_STACK;
  return 1;
}


static inline int unify1(CHOICE_POINT *C0, X x, X y)
{
  INIT_CYCLE_STACK;
  return unify2(C0, x, y);
}


static int unify_args(CHOICE_POINT *C0, X *A, X *args)
{
  while(*args != NULL) {
    if(!unify(*A, *args)) return 0;

    ++A;
    ++args;
  }

  return 1;
}


static int unify_block(CHOICE_POINT *C0, X *A, int arity, int *typemap, 
		       int ilen, BLOCK_INTEGER_DISPATCH *itable, 
		       int alen, BLOCK_SYMBOL_DISPATCH *atable, 
		       int slen, BLOCK_STRUCTURE_DISPATCH *stable)
{
  X *args = A[ arity ];
  X *tp = trail_top;

  /* if this is the first entry, then dispatch on type, if first arg
     is not a variable (assumes A[0] is already derefd) */
  if(args == A[ arity + 1 ]) {
    X A0 = A[ 0 ];

    // typemap: { INT1, ATOM1, NULL1, PAIR1, STRUCTURE1 }
    // fixnum first
    if(is_FIXNUM(A0)) {
      if(typemap[ 0 ]) {
	if(itable) {
	  // secondary clause-indexing for integer case
	  XWORD num = fixnum_to_word(A0);
	  XWORD key = (num < 0 ? -num : num) % ilen;

	  for(;;) {
	    XWORD tnum = itable[ key ].num;
	    
	    if(tnum == num) {
	      args += arity * (itable[ key ].index - 1);
	      goto enter;
	    }
    
	    if(itable[ key ].index == 0) break;

	    key = (key + 1) % ilen;
	  }
	}

	args += arity * (typemap[ 0 ] - 1);
      }
    }
    else if(!is_VAR(A0)) {	/* var? start with first clause */
      if(A0 == END_OF_LIST_VAL) {
	if(typemap[ 2 ])
	  args += arity * (typemap[ 2 ] - 1);
      }
      else if(is_SYMBOL(A0)) {
	if(typemap[ 1 ]) {
	  if(atable) {
	    // secondary clause-indexing for atom case
	    XWORD key = fixnum_to_word(slot_ref(A0, 2)) % alen;

	    for(;;) {
	      X tsym = atable[ key ].symbol;

	      if(tsym == A0) {
		args += arity * (atable[ key ].index - 1);
		goto enter;
	      }
    
	      if(tsym == NULL) break;

	      key = (key + 1) % alen;
	    }
	  }
	  
	  args += arity * (typemap[ 1 ] - 1);
	}
      }
      else if(is_PAIR(A0)) {
	if(typemap[ 3 ]) 
	  args += arity * (typemap[ 3 ] - 1);
      }
      else if(is_STRUCTURE(A0)) {
	if(typemap[ 4 ]) {
	  if(stable) {
	    // secondary clause-indexing for structure case
	    X sym = slot_ref(A0, 0);
	    int sarity = objsize(A0) - 1;
	    XWORD key = (fixnum_to_word(slot_ref(sym, 2)) + sarity) % slen;

	    for(;;) {
	      X tsym = stable[ key ].name;
	      
	      if(tsym == sym && stable[ key ].arity == sarity) {
		args += arity * (stable[ key ].index - 1);
		goto enter;
	      }
	      
	      if(tsym == NULL) break;
	      
	      key = (key + 1) % slen;
	    }
	  }
	  
	  args += arity * (typemap[ 4 ] - 1);
	}
      }
    }
  }

 enter:
  while(*args != NULL) {	/* loop over facts */
    int i;

    for(i = 0; i < arity; ++i) { /* loop over arguments */
      if(!unify(A[ i ], *(args++))) {
	unwind_trail(tp);
	args += arity - (i + 1);	/* skip args in this fact */
	break;
      }
    }

    if(i == arity) {
      A[ arity ] = args;
      return 1;
    }
  }

  return 0;
}


/// term-construction

static X make_term_from_list(int arity, X functor, X args)
{
  if(arity == 0) return functor;

  check_type(SYMBOL_TYPE, functor);

  if(arity == 2 && functor == dot_atom) {
    check_type(PAIR_TYPE, args);
    X car = slot_ref(args, 0);
    args = deref(slot_ref(args, 1));
    check_type(PAIR_TYPE, args);
    return PAIR(car, slot_ref(args, 0));
  }

  X s = STRUCTURE(functor, arity);

  for(int i = 1; i <= arity; ++i) {
    check_type(PAIR_TYPE, args);
    SLOT_SET(s, i, slot_ref(args, 0));
    args = deref(slot_ref(args, 1));
  }

  return (X)s;
}


#define make_pair    PAIR


/// numerical operations


static inline XFLOAT to_float(X x)
{
  if(is_FIXNUM(x)) return fixnum_to_float(x);
  else if(is_FLONUM(x)) return flonum_to_float(x);

  check_number_failed(x);
  return 0;
}


#define NUMERIC_BINARY_CMP(name, op)		\
  static inline int name(X x, X y)		\
  { x = deref(x); \
    y = deref(y); \
    if(is_FIXNUM(x)) {					\
      if(is_FIXNUM(y))					\
	return fixnum_to_word(x) op fixnum_to_word(y);	\
      if(is_FLONUM(y))					\
	return fixnum_to_float(x) op flonum_to_float(y);	\
      check_number_failed(y); }					\
    if(is_FLONUM(x)) {						\
      if(is_FIXNUM(y))						\
	return flonum_to_float(x) op fixnum_to_float(y);	\
      if(is_FLONUM(y))						\
	return flonum_to_float(x) op flonum_to_float(y);	\
      check_number_failed(y); }					\
    check_number_failed(x);					\
    return 0; }

NUMERIC_BINARY_CMP(is_num_eq, ==)
NUMERIC_BINARY_CMP(is_num_gt, >)
NUMERIC_BINARY_CMP(is_num_lt, <)

#define NUMERIC_BINARY_OP(name, op)		\
  static inline X name(X x, X y)		\
  { x = deref(x); \
    y = deref(y); \
    if(is_FIXNUM(x)) {					\
      if(is_FIXNUM(y))					\
	return word_to_fixnum(fixnum_to_word(x) op fixnum_to_word(y));	\
      if(is_FLONUM(y))					\
	return FLONUM(fixnum_to_float(x) op flonum_to_float(y));	\
      check_number_failed(y); }					\
    if(is_FLONUM(x)) {						\
      if(is_FIXNUM(y))						\
	return FLONUM(flonum_to_float(x) op fixnum_to_float(y));	\
      if(is_FLONUM(y))						\
	return FLONUM(flonum_to_float(x) op flonum_to_float(y));	\
      check_number_failed(y); }						\
    check_number_failed(x);						\
    return 0; }

NUMERIC_BINARY_OP(num_add, +)
NUMERIC_BINARY_OP(num_sub, -)
NUMERIC_BINARY_OP(num_mul, *)

#define INTEGER_BINARY_OP(name, op)  \
  static inline X name(X x, X y)		\
  { x = deref(x); \
    y = deref(y); \
    check_fixnum(x); \
    check_fixnum(y); \
    return word_to_fixnum(fixnum_to_word(x) op fixnum_to_word(y)); }

INTEGER_BINARY_OP(num_and, &)
INTEGER_BINARY_OP(num_or, |)
INTEGER_BINARY_OP(num_xor, ^)
INTEGER_BINARY_OP(num_shl, <<)
INTEGER_BINARY_OP(num_shr, >>)


static inline X num_div(X x, X y)
{
  x = deref(x);
  y = deref(y);

  if(is_FIXNUM(x)) {
    if(is_FIXNUM(y)) {
#ifndef UNSAFE
      if(y == word_to_fixnum(0))
	evaluation_error("zero_divisor");
#endif

      return FLONUM(fixnum_to_float(x) / fixnum_to_float(y));	
    }

    if(is_FLONUM(y))						
      return FLONUM(fixnum_to_float(x) / flonum_to_float(y));	
  
    check_number_failed(y);
  }					

  if(is_FLONUM(x)) {						
    if(is_FIXNUM(y)) {
#ifndef UNSAFE
      if(y == word_to_fixnum(0))
	evaluation_error("zero_divisor");
#endif

      return FLONUM(flonum_to_float(x) / fixnum_to_float(y));  
    }

    if(is_FLONUM(y))							
      return FLONUM(flonum_to_float(x) / flonum_to_float(y));		

    check_number_failed(y);
  }						
  
  check_number_failed(x); 
  return x;			/* never executed */
}


static inline X num_quo(X x, X y) 
{
  x = deref(x);
  y = deref(y);
  check_fixnum(x);
  check_fixnum(y);

#ifndef UNSAFE
  if(y == word_to_fixnum(0))
    evaluation_error("zero_divisor");
#endif

  return word_to_fixnum(fixnum_to_word(x) / fixnum_to_word(y));
}


static inline X num_rem(X x, X y) 
{
  x = deref(x);
  y = deref(y);
  check_fixnum(x);
  check_fixnum(y);

#ifndef UNSAFE
  if(y == word_to_fixnum(0))
    evaluation_error("zero_divisor");
#endif

  return word_to_fixnum(fixnum_to_word(x) % fixnum_to_word(y));
}


static inline X num_mod(X x, X y)
{
  XWORD x2 = fixnum_to_word(check_fixnum(deref(x)));
  XWORD y2 = fixnum_to_word(check_fixnum(deref(y)));

#ifndef UNSAFE
  if(y2 == 0)
    evaluation_error("zero_divisor");
#endif

  XWORD z = x2 % y2;
    
  if(y2 < 0)
    return z <= 0 ? word_to_fixnum(z) : word_to_fixnum(z + y2);
  else 
    return z >= 0 ? word_to_fixnum(z) : word_to_fixnum(z + y2);
}


static inline X num_pow(X x, X y)
{
  x = deref(x);
  y = deref(y);

  if(is_FIXNUM(x)) {
    if(is_FIXNUM(y))
      return FLONUM(pow(fixnum_to_float(x), fixnum_to_float(y)));	

    if(is_FLONUM(y))						
      return FLONUM(pow(fixnum_to_float(x), flonum_to_float(y)));	
  
    check_number_failed(y);
  }					

  if(is_FLONUM(x)) {						
    if(is_FIXNUM(y))
      return FLONUM(pow(flonum_to_float(x), fixnum_to_float(y)));  

    if(is_FLONUM(y))							
      return FLONUM(pow(flonum_to_float(x), flonum_to_float(y)));		

    check_number_failed(y);
  }						
  
  check_number_failed(x); 
  return x;			/* never executed */
}


static inline X num_max(X x, X y)
{
  x = deref(x);
  y = deref(y);

  if(is_FIXNUM(x)) {
    if(is_FIXNUM(y))
      return fixnum_to_word(x) > fixnum_to_word(y) ? x : y;

    if(is_FLONUM(y))						
      return fixnum_to_word(x) > flonum_to_float(y) ? x : y;
  
    check_number_failed(y);
  }					

  if(is_FLONUM(x)) {						
    if(is_FIXNUM(y))
      return flonum_to_float(x) > fixnum_to_word(y) ? x : y;

    if(is_FLONUM(y))							
      return flonum_to_float(x) > flonum_to_float(y) ? x : y;

    check_number_failed(y);
  }						
  
  check_number_failed(x); 
  return x;			/* never executed */
}


static inline X num_min(X x, X y)
{
  x = deref(x);
  y = deref(y);

  if(is_FIXNUM(x)) {
    if(is_FIXNUM(y))
      return fixnum_to_word(x) < fixnum_to_word(y) ? x : y;

    if(is_FLONUM(y))						
      return fixnum_to_word(x) < flonum_to_float(y) ? x : y;
  
    check_number_failed(y);
  }					

  if(is_FLONUM(x)) {						
    if(is_FIXNUM(y))
      return flonum_to_float(x) < fixnum_to_word(y) ? x : y;

    if(is_FLONUM(y))							
      return flonum_to_float(x) < flonum_to_float(y) ? x : y;

    check_number_failed(y);
  }						
  
  check_number_failed(x); 
  return x;			/* never executed */
}


static inline X num_float(X x)
{
  x = deref(x);

  if(is_FIXNUM(x)) return FLONUM(fixnum_to_word(x));

  check_type_FLONUM(x);
  return x;
}


static inline X num_frac(X x)
{
  XFLOAT i;
  return FLONUM(modf(to_float(deref(x)), &i));
}


static inline X num_int(X x)
{
  XFLOAT i;
  modf(to_float(deref(x)), &i);
  return FLONUM(i);
}


static inline X num_floor(X x) { return FLONUM(floor(to_float(deref(x)))); }
static inline X num_ceiling(X x) { return FLONUM(ceil(to_float(deref(x)))); }
static inline X num_round(X x) { return FLONUM(round(to_float(deref(x)))); }
static inline X num_truncate(X x) { return FLONUM(trunc(to_float(deref(x)))); }
static inline X num_sin(X x) { return FLONUM(sin(to_float(deref(x)))); }
static inline X num_cos(X x) { return FLONUM(cos(to_float(deref(x)))); }
static inline X num_tan(X x) { return FLONUM(tan(to_float(deref(x)))); }
static inline X num_atan(X x) { return FLONUM(atan(to_float(deref(x)))); }
static inline X num_exp(X x) { return FLONUM(exp(to_float(deref(x)))); }
static inline X num_log(X x) { return FLONUM(log(to_float(deref(x)))); }
static inline X num_sqrt(X x) { return FLONUM(sqrt(to_float(deref(x)))); }


static inline X num_abs(X x)
{ 
  x = deref(x);

  if(is_FIXNUM(x))
    return fixnum_to_word(x) < 0 ? word_to_fixnum(-fixnum_to_word(x)) : x;

  if(is_FLONUM(x)) {
    XFLOAT n = flonum_to_float(x);

    if(n < 0) return FLONUM(-n);
    
    return x;
  }

  check_number_failed(x);
  return x;			/* never executed */
}


static inline X num_sign(X x)
{ 
  x = deref(x);

  if(is_FIXNUM(x)) {
    if(fixnum_to_word(x) < 0)
      return word_to_fixnum(-1);

    if(x == ZERO) 
      return word_to_fixnum(0);
      
    return word_to_fixnum(1);
  }

  if(is_FLONUM(x)) {
    XFLOAT n = flonum_to_float(x);

    if(n < 0.0) return FLONUM(-1);
    
    if(n > 0.0) return FLONUM(1);

    return FLONUM(0);
  }

  check_number_failed(x);
  return x;			/* never executed */
}


static inline X num_negate(X x)
{ 
  x = deref(x);

  if(is_FIXNUM(x)) 
    return word_to_fixnum(-fixnum_to_word(x));

  if(is_FLONUM(x))
    return FLONUM(-flonum_to_float(x));

  check_number_failed(x);
  return x;			/* never executed */
}


static inline X num_not(X x) 
{ 
  return word_to_fixnum(~fixnum_to_word(check_fixnum(deref(x))));
}


static inline X num_random(X x)
{
  return word_to_fixnum(rand() % fixnum_to_word(check_fixnum(deref(x))));
}


static inline X num_clock() { return FLONUM(get_time()); }


/// Term comparison

static int is_recursively_identical(X x, X y)
{
  x = deref(x);
  y = deref(y);

  if(x == y) return 1;

  if(is_FIXNUM(x) || is_FIXNUM(y)) return 0;

  XWORD t = objtype(x);

  if(t != objtype(y)) return 0;

  if(t == SYMBOL_TYPE) return 0;

  if(t == VAR_TYPE) 
    return slot_ref(x, 1) == slot_ref(y, 1);

  XWORD s = objsize(x);

  if(s != objsize(y)) return 0;

  if(is_byteblock(x))
    return !memcmp(objdata(x), objdata(y), s);

  XWORD i = 0;

  if(is_specialblock(x)) {
    if(slot_ref(x, 0) != slot_ref(y, 0)) return 0;

    i = 1;
  }

  while(i < s) {
    X x2 = slot_ref(x, i);
    X y2 = slot_ref(y, i);

    if(x2 != y2 && !is_recursively_identical(x2, y2)) 
      return 0;

    ++i;
  }

  return 1;
}


static inline int is_identical(X x, X y)
{
  if(x == y) return 1;
  
  return is_recursively_identical(x, y);
}


static inline int compare_strings(XCHAR *str1, XWORD len1, XCHAR *str2, XWORD len2)
{
  XWORD d = strncmp(str1, str2, len1 < len2 ? len1 : len2);
  return d == 0 ? len2 - len1 : -d;
}


static int compare_terms(X x, X y)
{
  x = deref(x);
  y = deref(y);

  if(x == y) return 0;

  XWORD xt = is_FIXNUM(x) ? FIXNUM_TYPE : objtype(x);
  XWORD yt = is_FIXNUM(y) ? FIXNUM_TYPE : objtype(y);
  // {-, fixnum, eol, symbol, flonum, port, var, string, structure, pair, -, dbref, ptr}
  // gives order: var < float < integer < atom(eol, port) < structure < dbref(ptr)
  static int type_order[] = { 0, 3, 4, 4, 2, 4, 1, 0, 5, 5, 0, 6, 6 };
  XWORD d = type_order[ yt & 0x1f ] - type_order[ xt & 0x1f ];

  if(d != 0) return d;

  switch(xt) {
  case FIXNUM_TYPE: return fixnum_to_word(y) - fixnum_to_word(x);

  case END_OF_LIST_TYPE: 
    switch(yt) {
    case END_OF_LIST_TYPE: return 0;

    case SYMBOL_TYPE:
      { X str = slot_ref(y, 0);
	return compare_strings("[]", 2, (XCHAR *)objdata(str), string_length(str)); }

    case PORT_TYPE: return '<' - '[';
    }

  case SYMBOL_TYPE:
    switch(yt) {
    case SYMBOL_TYPE:
      { X str1 = slot_ref(x, 0);
	X str2 = slot_ref(y, 0);
	XWORD len1 = string_length(str1);
	XWORD len2 = string_length(str2);
	return compare_strings((XCHAR *)objdata(str1), len1, (XCHAR *)objdata(str2), len2); }

    case END_OF_LIST_TYPE:
      { X str = slot_ref(x, 0);
	return compare_strings((XCHAR *)objdata(str), string_length(str), "[]", 2); }
      
    case PORT_TYPE:
      { XCHAR *buf = port_name(y);
	X str = slot_ref(x, 0);
	return compare_strings((XCHAR *)objdata(str), string_length(str), buf, strlen(buf)); }
    }

  case PORT_TYPE:
    switch(yt) {
    case END_OF_LIST_TYPE:
      { XCHAR *buf = port_name(x);
	return compare_strings(buf, strlen(buf), "[]", 2); }
      
    case SYMBOL_TYPE:
      { XCHAR *buf = port_name(x);
	X str = slot_ref(y, 0);
	return compare_strings(buf, strlen(buf), (XCHAR *)objdata(str), string_length(str)); }
      
    case PORT_TYPE:
      return (XWORD)(((PORT_BLOCK *)y)->fp - ((PORT_BLOCK *)x)->fp);
    }

  case FLONUM_TYPE: 
    { XFLOAT n = flonum_to_float(y) - flonum_to_float(x);
      return n > 0 ? 1 : (n < 0 ? -1 : 0);
    }
    
  case VAR_TYPE:
    return (XWORD)slot_ref(y, 1) - (XWORD)slot_ref(x, 1);

  case STRUCTURE_TYPE:
    switch(yt) {
    case STRUCTURE_TYPE:
      { XWORD s = objsize(y);
	d = s - objsize(x);

	if(d != 0) return d;
      
	d = compare_terms(slot_ref(x, 0), slot_ref(y, 0));
    
	if(d != 0) return d;

	for(int i = 1; i < s; ++i) {
	  d = compare_terms(slot_ref(x, i), slot_ref(y, i));

	  if(d != 0) return d;
	}

	return 0; }

    case PAIR_TYPE:
      d = 3 - objsize(x);

      if(d != 0) return d;

      d = compare_terms(slot_ref(x, 0), dot_atom);

      if(d != 0) return d;

      d = compare_terms(slot_ref(x, 1), slot_ref(y, 0));

      if(d != 0) return d;

      return compare_terms(slot_ref(x, 2), slot_ref(y, 1));

    }

  case PAIR_TYPE:
    switch(yt) {
    case STRUCTURE_TYPE:
      d = objsize(y) - 3;

      if(d != 0) return d;

      d = compare_terms(dot_atom, slot_ref(y, 0));

      if(d != 0) return d;

      d = compare_terms(slot_ref(x, 0), slot_ref(x, 1));

      if(d != 0) return d;

      return compare_terms(slot_ref(x, 1), slot_ref(y, 2));

    case PAIR_TYPE:
      d = compare_terms(slot_ref(x, 0), slot_ref(y, 0));

      if(d != 0) return d;

      return compare_terms(slot_ref(x, 1), slot_ref(y, 1));

    }

  case DBREFERENCE_TYPE:
  case POINTER_TYPE:
    return (XWORD)slot_ref(y, 0) - (XWORD)slot_ref(x, 0);
  }

  CRASH("unable to compare terms");
}


static int ensure_string_buffer(int len, XCHAR **ptr)
{
  if(len >= string_buffer_length - 1) {
    XWORD offset;

    if(ptr) offset = *ptr - string_buffer;

    string_buffer = realloc(string_buffer, string_buffer_length *= 2);
    ASSERT(string_buffer, 
	   "out of memory - can not increase size of string-buffer to " XWORD_OUTPUT_FORMAT, 
	   (XWORD)string_buffer_length);

    if(ptr) *ptr = (XCHAR *)(string_buffer + offset);

    return 1;
  }

  return 0;
}


// assumes x is deref'd, returns pointer to char that should not be modified
static XCHAR *to_string(X x, int *size)
{
  if(is_FIXNUM(x))
    type_error("atom_or_string", x);

  if(x == END_OF_LIST_VAL) {
    *size = 0;
    return "";
  }

  switch(objtype(x)) {
  case SYMBOL_TYPE:
    { X str = slot_ref(x, 0);
      *size = string_length(str);
      return (XCHAR *)objdata(str); }

  case PAIR_TYPE:
    { int len = 0;
      XCHAR *ptr = string_buffer;
      
      while(!is_FIXNUM(x) && objtype(x) == PAIR_TYPE) {
	ensure_string_buffer(len, &ptr);
	X c = deref(slot_ref(x, 0));
	check_fixnum(c);
	*(ptr++) = fixnum_to_word(c);
	++len;
	x = deref(slot_ref(x, 1));
      }

      if(x != END_OF_LIST_VAL)
	type_error("proper_list", x);

      *ptr = '\0';
      *size = len;
      return string_buffer; }

  default:
    if(is_VAR(x))
      throw_exception(instantiation_error_atom);
    else type_error("atom_or_string", x);

    return NULL;
  }
}


// triggers GC if space is insufficient
static X string_to_list(XCHAR *str, int len)
{
  XWORD size = len * 3 * sizeof(XWORD); /* N pairs */

  if((XWORD)alloc_top + size  > (XWORD)fromspace_limit) {
    force_gc();
    return ZERO;
  }

  X lst = END_OF_LIST_VAL;

  while(len > 0)
    lst = PAIR(word_to_fixnum(str[ --len ]), lst);

  return lst;
}


static X string_to_number(XCHAR *ptr)
{
  // catch "nan" and "inf" (user at least should add "+"/"-")
  if(isalpha(*ptr)) return END_OF_LIST_VAL;

  XCHAR *endptr;
  XWORD n = strtol(ptr, &endptr, 10);

  if(*endptr != '\0' || ((n == LONG_MIN || n == LONG_MAX) && errno == ERANGE) || !is_in_fixnum_range(n)) {
    XFLOAT f = strtod(ptr, &endptr);

    if(*endptr != '\0' || ((f == 0 || f == HUGE_VAL || f == -HUGE_VAL) && errno == ERANGE)) 
      return END_OF_LIST_VAL;

    return FLONUM(f);
  }

  return word_to_fixnum(n);
}


static void push_argument_list(X lst)
{
  lst = deref(lst);

  while(!is_FIXNUM(lst) && is_PAIR(lst)) {
    *(arg_top++) = slot_ref(lst, 0);
    lst = deref(slot_ref(lst, 1));
  }
}


/// Port helpers

static X get_stream(X s) 
{
  if(s == user_input_atom) return &default_input_port;
  else if(s == user_output_atom) return standard_output_port;
  else if(s == user_error_atom) return &default_error_port;
  else if(s == current_input_atom) return standard_input_port;
  else if(s == current_output_atom) return standard_output_port;
  
  return check_type_PORT(s);
}


static X get_input_stream(X s) 
{
  if(s == user_input_atom) return &default_input_port;
  else if(s == current_input_atom) return standard_input_port;
  
  return check_input_port(s);
}


static X get_output_stream(X s) 
{
  if(s == user_output_atom) return &default_output_port;
  else if(s == user_error_atom) return &default_error_port;
  else if(s == current_output_atom) return standard_output_port;
  
  return check_output_port(s);
}


static inline FILE *get_input_port(X s) { return port_file(get_input_stream(s)); }
static inline FILE *get_output_port(X s) { return port_file(get_output_stream(s)); }


/// VM operations

#define CURRENT_NAME
#define CURRENT_ARITY
#define CURRENT_ENVIRONMENT_SIZE

#define ENTER(lbl)							\
  { CHECK_LIMIT;							\
    PUSH_CHOICE_POINT(NULL);						\
    SET_WHERE(PREVIOUS_PINFO);						\
    CALL_TRIGGERED(lbl);						\
    TRACE_ENTER(CURRENT_NAME, CURRENT_ARITY); }

#define PUSH_CHOICE_POINT(lbl)					\
  { C->T = trail_top;						\
    C->R = R;							\
    C->E = E;							\
    C->A = A;							\
    C->timestamp = clock_ticks++;				\
    C->arg_top = arg_top;					\
    C->env_top = env_top;					\
    C->catch_top = catch_top;					\
    C->C0 = C0;							\
    C->P = lbl;							\
    SET_PINFO(C->where, PREVIOUS_PINFO);			\
    C0 = C++;							\
    ASSERT((XWORD)C < (XWORD)choice_point_stack + choice_point_stack_size, "choice-point stack overflow"); }

#define COPY_CHOICE_POINT(lbl)						\
  { C->T = trail_top;							\
    C->R = C0->R;							\
    C->E = E;								\
    C->A = C0->A;							\
    C->timestamp = clock_ticks++;					\
    C->arg_top = arg_top;						\
    C->env_top = env_top;						\
    C->catch_top = catch_top;						\
    C->C0 = C0;								\
    C->P = lbl;								\
    SET_PINFO(C->where, PREVIOUS_PINFO);				\
    ++C;								\
    ASSERT((XWORD)C < (XWORD)choice_point_stack + choice_point_stack_size, "choice-point stack overflow"); }

#define ADJUST_CHOICE_POINT(lbl)					\
  { C0->T = trail_top;							\
    C0->arg_top = arg_top;						\
    C0->env_top = env_top;						\
    C0->P = lbl; }

#define SAVE_CHOICE_POINTS						\
  { *(ifthen_top++) = arg_top;						\
    *(ifthen_top++) = env_top;						\
    *(ifthen_top++) = E;						\
    *(ifthen_top++) = C0->P;						\
    *(ifthen_top++) = C0;						\
    *(ifthen_top++) = C;						\
    ASSERT((XWORD)ifthen_top < (XWORD)ifthen_stack + ifthen_stack_size, "if-then stack overflow"); }

#define RESTORE_CHOICE_POINTS	 \
  { C = *(--ifthen_top);	 \
    C0 = *(--ifthen_top);	 \
    C0->P = *(--ifthen_top);	 \
    E = *(--ifthen_top);	 \
    env_top = *(--ifthen_top);	 \
    arg_top = *(--ifthen_top);						\
    ASSERT(ifthen_top >= ifthen_stack, "if-then stack underflow"); }

#define INVOKE_CHOICE_POINT			\
  { for(C0 = C - 1; C0->P == NULL; --C0) {	\
      SET_WHERE(C0->where);			\
      PROFILE_COUNTS;				\
      E = C0->E;				\
      env_top = C0->env_top; }			\
    C = C0 + 1;					\
    unwind_trail(C0->T);			\
    A = C0->A;					\
    arg_top = C0->arg_top;			\
    catch_top = C0->catch_top;			\
    SET_WHERE(C0->where);			\
    goto *(C0->P); }

#define POP_CHOICE_POINT  \
  { E = C0->E;		  \
    C0 = C0->C0; }

#ifdef PROFILE_MEMORY
# define PROFILE_COUNTS							\
  { COUNT_TOTAL(cp, (XWORD)(C - C0) * sizeof(CHOICE_POINT));		\
    COUNT_TOTAL(tp, (XWORD)(trail_top - C0->T) * sizeof(XWORD) * 2);	\
    COUNT_TOTAL(ep, (XWORD)(env_top - C0->env_top) * sizeof(XWORD));	\
    COUNT_INC; }								
#else
# define PROFILE_COUNTS
#endif

#define EXIT(lbl)			     \
  { CALL_TRIGGERED(lbl);		     \
    TRACE_EXIT(CURRENT_NAME, CURRENT_ARITY); \
    PROFILE_COUNTS;			     \
    R = C0->R;				     \
    E = C0->E;				     \
    C0 = C0->C0;			     \
    goto *R; }

#define DETERMINATE_EXIT		     \
  { TRACE_EXIT(CURRENT_NAME, CURRENT_ARITY); \
    PROFILE_COUNTS;			     \
    R = C0->R;				     \
    E = C0->E;				     \
    env_top = C0->env_top;		     \
    arg_top = C0->arg_top - CURRENT_ARITY;   \
    C = C0;				     \
    C0 = C0->C0;			     \
    goto *R; }

#define CALL(lbl, ret)   { R = ret; goto lbl; }

#ifdef USE_DELAY
# define CALL_TRIGGERED(lbl)   \
  { lbl: if(triggered_frozen_goals != END_OF_LIST_VAL) {	\
    R = &&lbl; goto triggered; }				\
    SET_WHERE(PREVIOUS_PINFO);					\
    A = C0->A; }
#else
# define CALL_TRIGGERED(lbl)
#endif

#define FAILURE             { TRACE_FAIL(CURRENT_NAME, CURRENT_ARITY); goto fail; }
#define QUASI_FAILURE(lbl)  goto lbl

#define FAIL  FAILURE
#define REDO  TRACE_REDO(CURRENT_NAME, CURRENT_ARITY)

#define POP_ARGUMENTS   arg_top = C0->arg_top - CURRENT_ARITY

#define TAIL_CALL(lbl)					 \
  { TRACE_TAIL_CALL(CURRENT_NAME, CURRENT_ARITY);	 \
    PROFILE_COUNTS;					 \
    R = C0->R;						 \
    E = C0->E;						 \
    env_top = C0->env_top;				 \
    C = C0;						 \
    C0 = C0->C0;					 \
    goto lbl; }

#define FINAL_CALL(lbl, ret)				 \
  { if(C == C0 + 1) {					 \
      TRACE_TAIL_CALL(CURRENT_NAME, CURRENT_ARITY);	 \
      PROFILE_COUNTS;					 \
      R = C0->R;					 \
      E = C0->E;					 \
      env_top = C0->env_top;				 \
      C = C0;						 \
      C0 = C0->C0; }					 \
    else R = ret;					 \
    goto lbl; }

#define CHECK_LIMIT					\
  { if(alloc_top >= fromspace_limit)			\
      collect_garbage(C);				\
    ASSERT((char *)arg_top < (char *)argument_stack + argument_stack_size, \
	   "argument-stack overflow"); } 

#define ENVIRONMENT(len)  \
  { E = env_top;							\
    for(int _i = 0; _i < (len); ++_i) *(env_top++) = ZERO;		\
    ASSERT((XWORD)env_top < (XWORD)environment_stack + environment_stack_size, \
	   "environment-stack overflow"); } 

#define SET_REDO(lbl)   C0->P = (lbl)

#define CUT(lbl)							\
  { CALL_TRIGGERED(lbl);						\
    C = C0 + 1;								\
    arg_top = C0->arg_top;						\
    env_top = E + CURRENT_ENVIRONMENT_SIZE;				\
    SET_REDO(NULL); }

#define PUSH_CATCHER(lbl)			\
  { catch_top->C0 = C0;				\
    catch_top->E = E;				\
    catch_top->T = trail_top;			\
    catch_top->ifthen_top = ifthen_top;		\
    catch_top->env_top = env_top;		\
    catch_top->arg_top = arg_top;		\
    catch_top->P = lbl;				\
    catch_top->ball = ZERO;			\
    ++catch_top; }

#define POP_CATCHER  \
  { ASSERT(catch_top > catch_stack, "catch-stack underflow"); \
    --catch_top; }

#define RETHROW       throw_exception(catch_top->ball)

#define UNIFY_BLOCK(dl, rl, xl, tl, ilen, it, alen, at, slen, st)	\
  { SET_REDO(&&rl);							\
    *(arg_top++) = (X *)dl;						\
    *(arg_top++) = (X *)dl;						\
    C0->arg_top += 2;							\
  rl:									\
    if(!unify_block(C0, A, CURRENT_ARITY, tl, ilen, it, alen, at, slen, st)) { \
      SET_REDO(NULL);							\
      FAIL; }								\
    BLOCK_EXIT(xl); }

#define BLOCK_EXIT(lbl)						\
  { CALL_TRIGGERED(lbl);					\
    TRACE_EXIT(CURRENT_NAME, CURRENT_ARITY);			\
    PROFILE_COUNTS;						\
    R = C0->R;							\
    E = C0->E;							\
    if(A[ CURRENT_ARITY ] == NULL) {				\
      env_top = C0->env_top;					\
      arg_top = C0->arg_top - CURRENT_ARITY - 2;		\
      C = C0; }							\
    C0 = C0->C0;						\
    goto *R; }


/// Profiling 

#if defined(PROFILE) || defined(PROFILE_MEMORY)
# define PREVIOUS_PINFO  &system_pinfo

# define DECLARE_PINFO(n, a, lbl)				\
  static PINFO lbl = { .name = n "/" #a, .next = PREVIOUS_PINFO }

# define STARTUP                 start: profile_init(PREVIOUS_PINFO); goto INIT_GOAL
# define SET_WHERE(pinfo)        where = pinfo
# define SET_PINFO(dest, pinfo)  dest = pinfo
#else
# define DECLARE_PINFO(n, a, l)
# define STARTUP    start: goto INIT_GOAL
# define SET_WHERE(pinfo)
# define SET_PINFO(dest, pinfo)
#endif


/// Boilerplate code

#ifdef EMBEDDED

# define TERMINATE(cp, code)   \
  { if(exit_code) *exit_code = code;		\
    cleanup();					\
    return NULL; }

# define DECLARE_RESULT
# define RETURN_RESULT            return saved_state.result

# define RETURN_EXCEPTION  \
  { if(exit_code) *exit_code = EXIT_EXCEPTION;	\
    cleanup();					\
    return NULL; }

#else
# define TERMINATE(cp, code)      terminate(cp, code)
# define DECLARE_RESULT           X result = ZERO
# define RETURN_RESULT
# define RETURN_EXCEPTION
#endif

#define BOILERPLATE					\
  X *A, *E;						\
  CHOICE_POINT *C, *C0;					\
  void *R, *R0;						\
  DECLARE_RESULT;					\
  if(argc == 0) {					\
    A = saved_state.A;					\
    R = saved_state.R;					\
    E = saved_state.E;					\
    C0 = saved_state.C0;				\
    C = saved_state.C;					\
    saved_state.result = result;			\
  } else {						\
    initialize(argc, argv);				\
    intern_static_symbols(PREVIOUS_SYMBOL);		\
    A = NULL;						\
    R = &&success_exit;					\
    E = env_top;					\
    C0 = C = choice_point_stack;			\
    C->T = trail_top;					\
    C->timestamp = clock_ticks++;			\
    C->R = NULL;					\
    C->C0 = NULL;					\
    C->P = &&fail_exit;					\
    SET_PINFO(C->where, &system_pinfo);			\
    C++; }						\
  int lj = setjmp(exception_handler);			\
  if(lj == 1) {						\
    PROFILE_COUNTS;					\
    unwind_trail(catch_top->T);				\
    C0 = catch_top->C0;					\
    C = C0 + 1;						\
    A = arg_top = catch_top->arg_top;			\
    env_top = catch_top->env_top;			\
    ifthen_top = catch_top->ifthen_top;			\
    E = catch_top->E;					\
    goto *(catch_top->P); }				\
  else if(lj == 2) { RETURN_EXCEPTION };		\
  if(argc == 0) goto *saved_state.P;			\
  else goto start;							\
fail: INVOKE_CHOICE_POINT;						\
fail_exit:								\
 fputs("no.\n", stderr);						\
 TERMINATE(C, EXIT_FAILURE);						\
success_exit:								\
 CALL_TRIGGERED(exit_r);						\
 ASSERT(ifthen_stack == ifthen_top, "unbalanced if-then stack");	\
 ASSERT(catch_stack == catch_top, "unbalanced catcher stack");		\
 TERMINATE(C, EXIT_SUCCESS);					       \
triggered:							       \
{ A = arg_top;							       \
   X arglst;							       \
   void *gp = next_frozen_goal(&arglst);			       \
   if(gp) {							       \
     push_argument_list(arglst);				       \
     goto *gp; }						       \
   goto *R; }							       \
suspend:							       \
 saved_state.A = A;						       \
 saved_state.E = E;						       \
 saved_state.R = R;						       \
 saved_state.C0 = C0;						       \
 saved_state.C = C;						       \
 RETURN_RESULT;


#define DISPATCH_ON_SYMBOL(table, lbl, len)  goto *(lookup_symbol_in_table(A[0], table, &&lbl, len))

#define DISPATCH_ON_STRUCTURE(table, lbl, len)  goto *(lookup_structure_in_table(A[0], table, &&lbl, len))


/// Entry-points for embedding

#ifdef EMBEDDED
# ifndef ENTRY_POINT_NAME
#  define ENTRY_POINT_NAME   prolog
# endif
# define ENTRY_POINT   X ENTRY_POINT_NAME(int argc, char *argv[], X result, int *exit_code)

# ifndef VARIABLE_ACCESS_NAME
#  define VARIABLE_ACCESS_NAME  prolog_variable
# endif

X VARIABLE_ACCESS_NAME(int index)
{
  int i = initial_global_variable_count + index;
  ASSERT(i < MAX_GLOBAL_VARIABLES, "global-variable index out of range: %d", index);

  if(i > global_variable_counter)
    global_variable_counter = i + 1;

  return &(global_variables[ i ]);
}

#else
# define ENTRY_POINT   int main(int argc, char *argv[])
#endif


////////////////////////////////////////////////////////////////////////////////

/// PRIMITIVES (all expect their arguments to be deref'd)


#define PRIMITIVE(name, ...)	   static int name(CHOICE_POINT *C0, __VA_ARGS__)


static int debug_hook(CHOICE_POINT *C0) { return 1; }


PRIMITIVE(put_byte, X s, X c) 
{
  int code;

  if(is_FIXNUM(c)) code = fixnum_to_word(c);
  else if(is_VAR(c)) throw_exception(instantiation_error_atom);
  else type_error("integer", c);
  
  fputc(code, get_output_port(s));
  return 1;
}

PRIMITIVE(put_string, X s, X str)
{
  int len;
  XCHAR *ptr = to_string(str, &len);
  fwrite(ptr, sizeof(XCHAR), len, get_output_port(s));
  return 1;
}

PRIMITIVE(basic_write, X s, X x) 
{ 
  basic_write_term(get_output_port(s), 0, 99999, 0, x); 
  return 1; 
}

PRIMITIVE(basic_writeq, X s, X x) 
{ 
  basic_write_term(get_output_port(s), 0, 99999, 1, x); 
  return 1; 
}

// has no args, avoid ugly dummy parameter
static int gc(CHOICE_POINT *C0) { force_gc(); return 1; }

PRIMITIVE(halt, X code) 
{ 
  check_fixnum(code);
  terminate(C0, fixnum_to_word(code));
  return 1;			/* never executed */
}

PRIMITIVE(command_line_arguments, X var)
{
  X lst = END_OF_LIST_VAL;

  // build argument-list for "command-line-arguments"
  for(int i = global_argc - 1; i > 0; --i) {
    // ignore runtime options
    if(global_argv[ i ][ 0 ] != '-' || global_argv[ i ][ 1 ] != ':') {
      X str = CSTRING(global_argv[ i ]);
      X sym = intern(str);
      X pr = PAIR(sym, lst);
      lst = pr;
    }
  }

  return unify(lst, var);
}

PRIMITIVE(db_create, X name, X size, X result) 
{
  check_type_SYMBOL(name);
  check_fixnum(size);
  X str = slot_ref(name, 0);
  DB *db = create_db((XCHAR *)objdata(str), string_length(str), fixnum_to_word(size));
  // this is a fake db-reference, not usable for lookup
  ALLOCATE_BLOCK(BLOCK *dbr, DBREFERENCE_TYPE, 2);
  dbr->d[ 0 ] = (X)db;
  dbr->d[ 1 ] = ZERO;	/* marks dbref as ptr to db (not item) */
  return unify(result, (X)dbr);
}

PRIMITIVE(db_find, X dbr, X key, X ref)
{
  check_type_DBREFERENCE(dbr);
  check_type_SYMBOL(key);
  X str = slot_ref(key, 0);
  DB *db = (DB *)slot_ref(dbr, 0);
  DB_ITEM *item = db_find_first_item(db, (XCHAR *)objdata(str), string_length(str));

  if(item) {
    ALLOCATE_BLOCK(BLOCK *b, DBREFERENCE_TYPE, 2);
    b->d[ 0 ] = (X)item;
    b->d[ 1 ] = ONE;
    return unify(ref, (X)b);
  }

  return 0;
}

PRIMITIVE(db_next, X ref, X result)
{
  check_type_DBREFERENCE(ref);
  DB_ITEM *item = ((DB_ITEM *)slot_ref(ref, 0))->next;

  while(item && item->erased) item = item->next;

  if(item != NULL) {
    ALLOCATE_BLOCK(BLOCK *b, DBREFERENCE_TYPE, 2);
    b->d[ 0 ] = (X)item;
    b->d[ 1 ] = ONE;
    return unify(result, (X)b);
  }

  return 0;
}

PRIMITIVE(db_find_bucket, X dbr, X prev, X key, X ref)
{
  check_type_DBREFERENCE(dbr);
  DB *db = (DB *)slot_ref(dbr, 0);
  DB_BUCKET *bucket = NULL;

  if(is_DBREFERENCE(prev)) {
    DB_ITEM *item = (DB_ITEM *)slot_ref(prev, 0);
    ASSERT(!item->erased, "attempt to obtain bucket from erased db-item");
    bucket = item->bucket;
  }

  bucket = db_enumerate_buckets(db, bucket);

  if(bucket) {
    X str = STRING(bucket->keylen);
    memcpy(objdata(str), bucket->key, bucket->keylen + 1); /* including 0-terminator */
    X bkey = intern(str);
    DB_ITEM *item = bucket->firstitem;
    ASSERT(!item->erased, "first db-item in bucket is marked as erased");
    ALLOCATE_BLOCK(BLOCK *b, DBREFERENCE_TYPE, 2);
    b->d[ 0 ] = (X)item;
    b->d[ 1 ] = ONE;
    return unify(key, bkey) && unify(ref, (X)b);
  }

  return 0;
}

PRIMITIVE(db_ref, X ref, X result)
{
  check_type_DBREFERENCE(ref);
  DB_ITEM *item = (DB_ITEM *)slot_ref(ref, 0);
  int failed;
  ASSERT(slot_ref(ref, 1) == ONE, "attempting to reference database pointer");
  ASSERT(!item->erased, "attempting to reference erased db-item");
  X x = thaw_term(item->val, &failed);
  return !failed && unify(result, x);
}

PRIMITIVE(db_erase, X ref)
{
  check_type_DBREFERENCE(ref);
  DB_ITEM *item = (DB_ITEM *)slot_ref(ref, 0);
  db_mark_item_as_erased(item);
  return 1;
}

PRIMITIVE(db_erase_all, X ref)
{
  check_type_DBREFERENCE(ref);
  DB_ITEM *item = (DB_ITEM *)slot_ref(ref, 0);
  db_mark_bucket_as_erased(item);
  return 1;
}

PRIMITIVE(db_record, X dbr, X atend, X key, X val, X result)
{
  check_type_DBREFERENCE(dbr);
  check_type_SYMBOL(key);
  X str = slot_ref(key, 0);
  DB *db = (DB *)slot_ref(dbr, 0);
  DB_ITEM *item = db_insert_item(db, (XCHAR *)objdata(str), string_length(str), val, atend != ZERO);
  ALLOCATE_BLOCK(BLOCK *b, DBREFERENCE_TYPE, 2);
  b->d[ 0 ] = (X)item;
  b->d[ 1 ] = ONE;
  return unify(result, (X)b);
}

PRIMITIVE(file_exists, X name) 
{
  struct stat info;
  int len;
  XCHAR *fname = to_string(name, &len);
  return !stat(fname, &info) && S_ISREG(info.st_mode);
}

PRIMITIVE(dir_exists, X name) 
{
  struct stat info;
  int len;
  XCHAR *fname = to_string(name, &len);
  return !stat(fname, &info) && S_ISDIR(info.st_mode);
}

PRIMITIVE(get_byte, X s, X c)
{
  int g = fgetc(get_input_port(s));
  return unify(word_to_fixnum(g), c);
}

PRIMITIVE(peek_byte, X s, X c)
{
  FILE *fp = get_input_port(s);
  int g = fgetc(fp);

  if(g != EOF) ungetc(g, fp);

  return unify(word_to_fixnum(g), c);
}

PRIMITIVE(at_eof, X s) { return feof(port_file(get_input_port(s))); }

PRIMITIVE(open_stream, X name, X input, X mode, X data, X result)
{
  int len;
  XCHAR *str = to_string(name, &len);
  XCHAR *m = to_string(mode, &len);
  FILE *fp = fopen(str, m);

  if(fp == NULL) {
    if(errno == ENOENT) {
      X str = intern(CSTRING("existence_error"));
      X exn = STRUCTURE(str, 2);
      SLOT_INIT(exn, 1, intern(CSTRING("open")));
      SLOT_INIT(exn, 2, name);
      throw_exception(exn);
    }
    else {
      X str = intern(CSTRING("permission_error"));
      X exn = STRUCTURE(str, 2);
      SLOT_INIT(exn, 1, intern(CSTRING("open")));
      SLOT_INIT(exn, 2, name);
      throw_exception(exn);
    }
  }

  X port = PORT(fp, input, ONE, data);
  return unify(port, result);
}

PRIMITIVE(close_stream, X stream)
{
  check_type_PORT(stream);

  if(stream == &default_input_port || stream == &default_output_port || stream == &default_error_port)
    return 1;

  if(slot_ref(stream, 2) != ZERO) {
    fclose(port_file(stream));
    SLOT_SET(stream, 2, ZERO);
  }

  if(stream == standard_input_port) standard_input_port = &default_input_port;
  else if(stream == standard_output_port) standard_output_port = &default_output_port;
  else if(stream == standard_error_port) standard_error_port = &default_error_port;

  return 1;
}

PRIMITIVE(shell_command, X cmd, X status)
{
  int len;
  XCHAR *ptr = to_string(cmd, &len);
  int s = system(ptr);
  return unify(word_to_fixnum(s), status);
}

PRIMITIVE(get_environment_variable, X name, X result)
{
  int len;
  XCHAR *ptr = to_string(name, &len);
  XCHAR *val = getenv(ptr);

  if(!val) return 0;

  int slen = strlen(val);
  X str = STRING(slen);
  memcpy(objdata(str), val, slen);
  return val && unify(intern(str), result);
}

PRIMITIVE(current_input_stream, X stream) { return unify(standard_input_port, stream); }
PRIMITIVE(current_output_stream, X stream) { return unify(standard_output_port, stream); }
PRIMITIVE(current_error_stream, X stream) { return unify(standard_error_port, stream); }

PRIMITIVE(set_current_input_stream, X stream)
{
  standard_input_port = get_input_stream(stream);
  return 1;
}

PRIMITIVE(set_current_output_stream, X stream)
{
  standard_output_port = get_output_stream(stream);
  return 1;
}

PRIMITIVE(set_current_error_stream, X stream)
{
  standard_error_port = get_output_stream(stream);
  return 1;
}

PRIMITIVE(atom_codes, X atom, X lst)
{
  if(is_variable(atom)) {
    int len;
    XCHAR *ptr = to_string(lst, &len);

    if(!strcmp(ptr, "[]")) 
      return unify(atom, END_OF_LIST_VAL);

    X str = STRING(len);
    memcpy(objdata(str), ptr, len);
    return unify(atom, intern(str));
  }

  int len;
  XCHAR *ptr;

  if(atom == END_OF_LIST_VAL) {
    len = 2;
    ptr = "[]";
  }
  else {
    check_type_SYMBOL(atom);
    X str = slot_ref(atom, 0);
    len = string_length(str);
    ptr = (XCHAR *)objdata(str);
  }

  return unify(lst, string_to_list(ptr, len));
}

PRIMITIVE(number_codes, X num, X lst)
{
  if(is_variable(num)) {
    int len;
    XCHAR *ptr = to_string(lst, &len);
    X n = string_to_number(ptr);
    return n != END_OF_LIST_VAL && unify(num, n);
  }

  check_number(num);

  if(is_FIXNUM(num))
    sprintf(string_buffer, XWORD_OUTPUT_FORMAT, fixnum_to_word(num));
  else
    sprintf(string_buffer, XFLOAT_OUTPUT_FORMAT, flonum_to_float(num));

  int len = strlen(string_buffer);
  return unify(lst, string_to_list(string_buffer, len));
}

PRIMITIVE(functor, X term, X name, X arity)
{
  if(is_FIXNUM(term))
    return unify(arity, word_to_fixnum(0)) && unify(term, name);

  if(is_VAR(term)) {
    check_fixnum(arity);
    int n = fixnum_to_word(arity);
    X x;

    if(n == 0) {
      check_atomic(name);
      x = name;
    }
    else if(name == dot_atom && n == 2)
      x = PAIR(make_var(), make_var());
    else {
      check_type_SYMBOL(name);
      x = STRUCTURE(name, n);
    
      for(int i = 1; i <= n; ++i) 
	SLOT_INIT(x, i, make_var());
    }

    return unify(term, x);
  }

  if(is_PAIR(term))
    return unify(arity, word_to_fixnum(2)) && unify(dot_atom, name);
  
  if(is_atomic(term))
    return unify(arity, word_to_fixnum(0)) && unify(term, name);

  check_type_STRUCTURE(term);
  return unify(word_to_fixnum(objsize(term) - 1), arity) && unify(name, slot_ref(term, 0));
}

PRIMITIVE(term_arg, X index, X term, X arg)
{
  check_fixnum(index);
  XWORD i = fixnum_to_word(index);

  if(is_PAIR(term))
    return i > 0 && i < 3 && unify(arg, slot_ref(term, i - 1));

  check_type_STRUCTURE(term);
  return i > 0 && i < objsize(term) && unify(arg, slot_ref(term, i));
}

PRIMITIVE(deref_term, X in, X limit, X dup, X out)
{
  check_fixnum(limit);
  int failed;
  X x = deref_all(in, fixnum_to_word(limit), fixnum_to_word(dup), &failed);
  return !failed && unify(x, out);
}

PRIMITIVE(enable_trace, X flag)
{
  check_fixnum(flag);
  debugging = fixnum_to_word(flag);
  return 1;
}

PRIMITIVE(get_process_id, X pid) { return unify(pid, word_to_fixnum(getpid())); }

PRIMITIVE(sleep_for_seconds, X secs) {
  check_fixnum(secs); 
#ifdef _WIN32
  Sleep(fixnum_to_word(secs) * 1000); 
#else
  sleep(fixnum_to_word(secs)); 
#endif
  return 1; 
}

PRIMITIVE(set_random_seed, X seed) 
{
  check_fixnum(seed);
  srand(fixnum_to_word(seed));
  return 1;
}

PRIMITIVE(flush_output, X stream) { 
  fflush(get_output_port(stream));
  return 1; 
}

PRIMITIVE(do_throw, X ball) { throw_exception(ball); return 0; }

PRIMITIVE(do_make_term, X arity, X functor, X args, X term) {
  check_fixnum(arity);
  return unify(term, make_term_from_list(fixnum_to_word(arity), functor, args));
}

PRIMITIVE(atom_hash, X sym, X hash) {
  check_type_SYMBOL(sym);
  return(unify(slot_ref(sym, 2), hash));
}

PRIMITIVE(read_string, X s, X len, X lst) {
  FILE *fp = get_input_port(s);
  XCHAR *ptr = string_buffer;
  int c, f = 1;
  int count;

  if(!is_FIXNUM(len)) f = 0;
  else count = fixnum_to_word(len);

  while(!f || count) {
    if((c = fgetc(fp)) == EOF) break;

    if(ptr >= string_buffer + string_buffer_length + 1) {
      ensure_string_buffer(string_buffer_length + 1, &ptr); /* force */
    }

    *(ptr++) = c;
    --count;
  }

  string_buffer_top = ptr;
  XWORD slen = ptr - string_buffer;
  return unify(lst, string_to_list(string_buffer, slen));
}

PRIMITIVE(read_line, X s, X lst) {
  FILE *fp = get_input_port(s);
  XCHAR *ptr = string_buffer;
  int p = 0;
  int space = string_buffer_length;

  while(space > 0) {
    if(fgets(string_buffer + p, space, fp) == NULL) {
      if(p > 0) break;
      else return unify(lst, end_of_file_atom);
    }

    int n = strlen(string_buffer + p);

    if(string_buffer[ n - 1 ] == '\n') break;

    p += n;
    space -= n;

    if(space <= 1) {
      ensure_string_buffer(string_buffer_length + 1, NULL); /* force */
      space = string_buffer_length - p;
    }
  }

  if(p == 0) p = strlen(string_buffer);

  string_buffer_top = string_buffer + p;

  if(string_buffer[ strlen(string_buffer) - 1 ] == '\n') --p;

  return unify(lst, string_to_list(string_buffer, p));
}

PRIMITIVE(retry_string_to_list, X lst) {
  XWORD len = string_buffer_top - string_buffer;
  X str = string_to_list(string_buffer, len);

  if(str == ZERO) 
    CRASH("out of memory - can not allocate string of length " XWORD_OUTPUT_FORMAT, len);

  return unify(lst, str);
}

PRIMITIVE(delay_goal, X var, X prio, X ptr, X args) {
  ASSERT(is_variable(var), "delay_goal: not a variable");
  check_fixnum(prio);
  ASSERT(!is_FIXNUM(ptr) && is_POINTER(ptr), "delay_goal: not a pointer");
  // insert into front of list - if multiple delayed goals are triggered
  // on a variable, the order will automatically be reversed because
  // the entry of a delayed goal will invoke the next on the list (and so on)
  // ...
  X a2 = PAIR(ptr, args);
  X a1 = PAIR(prio, a2);
  X al = slot_ref(var, 3);
  X prev = NULL;

  // ... but: take priority into account
  while(al != END_OF_LIST_VAL) {
    X a = slot_ref(al, 0);
    
    if(slot_ref(a, 0) >= prio) {
      if(prev) SLOT_SET(prev, 1, PAIR(a1, al));
      else SLOT_SET(var, 3, PAIR(a1, al));

      break;
    }

    al = slot_ref(al, 1);
  }

  SLOT_SET(var, 3, PAIR(a1, al));
  return 1;
}

// used for dif/2
PRIMITIVE(special_id, X x, X y, X var) {
  first_distinct_variable = NULL;
  X *tt = trail_top;

  if(!unify(x, y)) return 0;

  unwind_trail(tt); /* detrail or unification will be with bound val */
  return unify(var, first_distinct_variable ? first_distinct_variable : ZERO);
}

// compute cdb-key from term or name/arity - for assert/clause/abolish
PRIMITIVE(cdb_key2, X name, X arity, X key) {
  ASSERT(is_atom(name) && is_FIXNUM(arity), "cdb_key2: bad PI");
  X str = slot_ref(name, 0);
  XWORD len = string_length(str);
  memcpy(string_buffer, objdata(str), len);
  sprintf(string_buffer + len, "\001" XWORD_OUTPUT_FORMAT, fixnum_to_word(arity));
  len += strlen(string_buffer + len);
  X str2 = STRING(len);
  memcpy(objdata(str2), string_buffer, len);
  return unify(intern(str2), key);
}

PRIMITIVE(cdb_key, X term, X key) {
  X str2;

  if(is_atom(term)) {
    X str = slot_ref(term, 0);
    XWORD len = string_length(str);
    memcpy(string_buffer, objdata(str), len);
    memcpy(string_buffer + len, "\0010", 3); /* + terminator */
    str2 = STRING(len + 2);
    memcpy(objdata(str2), string_buffer, len + 4); /* s.a. */
  }
  else {
    ASSERT(is_compound(term), "cdb_key: bad term");
    X str = slot_ref(slot_ref(term, 0), 0);
    XWORD len = string_length(str);
    memcpy(string_buffer, objdata(str), len);
    sprintf(string_buffer + len, "\001" XWORD_OUTPUT_FORMAT, objsize(term) - 1);
    len += strlen(string_buffer + len);
    str2 = STRING(len);
    memcpy(objdata(str2), string_buffer, len);
  }

  return unify(intern(str2), key);
}

PRIMITIVE(fixnum_bounds, X minimum, X maximum) {
  XWORD mi, ma;
#ifdef SIXTYFOUR
  ma = 0x3fffffffffffffff;
  mi = -0x3fffffffffffffff - 1;
#else
  ma = 0x3fffffff;
  mi = -0x3fffffff - 1;
#endif
  return unify(minimum, word_to_fixnum(mi)) &&
    unify(maximum, word_to_fixnum(ma));
}

PRIMITIVE(acyclic_term, X term) { return !check_cycles(term); }

PRIMITIVE(atom_length, X a, X len) { 
  return unify(len, word_to_fixnum(string_length(slot_ref(check_type_SYMBOL(a), 0))));
}

PRIMITIVE(ground, X term) { return check_ground(term); }

PRIMITIVE(rename_file, X old, X new) {
  int len;
  XCHAR *on = strdup(to_string(old, &len));
  XCHAR *nn = to_string(new, &len);
  
  if(rename(on, nn) != 0) {
    free(on);
    system_error(strerror(errno));
  }

  free(on);
  return 1;
}

PRIMITIVE(delete_file, X fn) {
  int len;
  XCHAR *name = to_string(fn, &len);

  if(unlink(name) != 0)
    system_error(strerror(errno));

  return 1;
}

PRIMITIVE(tty_stream, X s) { return isatty(fileno(port_file(get_stream(s)))); }

PRIMITIVE(os_type, X type) {
  switch(fixnum_to_word(type)) {
  case 0:			/* unix */
#ifdef __unix__
    return 1;
#else
    return 0;
#endif
  case 1:			/* windows */
#ifdef WIN32
    return 1;
#else
    return 0;
#endif
  case 2:			/* apple */
#ifdef __APPLE__
    return 1;
#else
    return 0;
#endif
  }

  return 0;
}

PRIMITIVE(sub_atom, X atom, X pos, X len, X result) {
  //XXX check string-buffer length
  check_type_SYMBOL(atom);
  check_fixnum(pos);
  check_fixnum(len);
  X str = slot_ref(atom, 0);
  int alen = string_length(str);
  int offset = fixnum_to_word(pos);
  int clen = fixnum_to_word(len);
  
  if(offset + clen > alen)
    system_error("sub-atom out of range");

  X res = STRING(clen);
  memcpy(objdata(res), (XCHAR *)objdata(str) + offset, clen);
  return unify(intern(res), result);
}

PRIMITIVE(stream_position, X s, X result) {
  long pos = ftell(port_file(get_stream(s)));

  if(pos == -1) return 0;

  return unify(result, word_to_fixnum(pos));
}

PRIMITIVE(set_stream_position, X s, X pos) {
  check_fixnum(pos);
  long p = fixnum_to_word(pos);

  if(fseek(port_file(get_stream(s)), p < 0 ? 0 : p, p < 0 ? SEEK_END : SEEK_SET) < 0)
    system_error(strerror(errno));

  return 1;
}

PRIMITIVE(stream_data, X s, X result) { 
  return unify(result, slot_ref(get_stream(s), 3)); 
}

PRIMITIVE(stream_fileno, X s, X result) { 
  return unify(result, word_to_fixnum(fileno(port_file(get_stream(s)))));
}

PRIMITIVE(get_working_dir, X result) {
  if(getcwd(string_buffer, string_buffer_length) == NULL)
    system_error(strerror(errno));

  return unify(result, intern(CSTRING(string_buffer)));
} 

PRIMITIVE(set_working_dir, X s) {
  int len;

  if(chdir(to_string(s, &len)) != 0)
    system_error(strerror(errno));

  return 1;
}

PRIMITIVE(word_size, X n) { 
#ifdef SIXTYFOUR
  return unify(n, word_to_fixnum(64));
#else
  return unify(n, word_to_fixnum(32));
#endif
}

PRIMITIVE(memberchk, X e, X lst) {
  X *tt = trail_top;

  while(lst != END_OF_LIST_VAL) {
    check_type_PAIR(lst);

    if(unify(e, slot_ref(lst, 0))) return 1;

    unwind_trail(tt);
    lst = deref(slot_ref(lst, 1));
  }

  return 0;
}

PRIMITIVE(fast_assq, X key, X lst, X arg, X result) {
  int i = fixnum_to_word(arg);

  while(lst != END_OF_LIST_VAL) {
    X item = deref(slot_ref(lst, 0));

    if(slot_ref(item, i) == key)
      return unify(result, lst);

    lst = deref(slot_ref(lst, 1));
  }

  return 0;
}

PRIMITIVE(num_to_atom, X num, X atom) {
  if(is_FIXNUM(num))
    sprintf(string_buffer, XWORD_OUTPUT_FORMAT, fixnum_to_word(num));
  else if(objtype(num) == FLONUM_TYPE)
    sprintf(string_buffer, XFLOAT_OUTPUT_FORMAT, flonum_to_float(num));
  else check_number_failed(num);

  return unify(atom, CSYMBOL(string_buffer));
}

PRIMITIVE(atom_to_num, X atom, X num) {
  XCHAR *ptr = (XCHAR *)objdata(slot_ref(check_type_SYMBOL(atom), 0));
  X n = string_to_number(ptr);
  return n != END_OF_LIST_VAL && unify(num, n);
}


#endif

#endif
