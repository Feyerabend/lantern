/* array.c — arrays beyond simple vectors (M13).
 *
 * A simple vector (HT_VECTOR) or string (HT_STRING) is already a
 * one-dimensional array, and stays the fast common case. Everything else —
 * more (or no) dimensions, a fill pointer, an adjustable or displaced array —
 * is an HT_ARRAY header around such a vector or string:
 *
 *   AR_DIMS    the dimensions, a list of fixnums
 *   AR_DATA    the elements, in row-major order: a vector, or a string
 *   AR_FILL    the fill pointer (fixnum), or NIL
 *   AR_OFFSET  where in AR_DATA element 0 is (displaced arrays share data)
 *   AR_FLAGS   1 = adjustable; 2 = a bit array (M18): AR_DATA is a narrow
 *              string of bytes 0 and 1, and the elements are the fixnums 0 and 1
 *
 * AREF, ASET and LENGTH take arrays here; world/cl2.lisp builds MAKE-ARRAY,
 * VECTOR-PUSH-EXTEND, ADJUST-ARRAY and the rest on these primitives.
 */
#include "lisp.h"

#define NEED(cond, msg, x) do { if (!(cond)) return lisp_error(msg, x); } while (0)

static int data_len(obj d) { return STRINGP(d) ? STR_LEN(d) : VEC_LEN(d); }

static obj data_ref(obj d, int i)
{
    return STRINGP(d) ? MAKE_CHAR(str_ref(d, i)) : VEC_REF(d, i);
}

static int data_set(obj d, int i, obj v)
{
    if (STRINGP(d)) {
        if (!CHARP(v)) { lisp_error("Not a character:", v); return 0; }
        if (!str_set(d, i, CHAR_CODE(v))) { lisp_error("Too wide a character for a narrow string:", v); return 0; }
    } else {
        VEC_REF(d, i) = v;
    }
    return 1;
}

int array_rank(obj a)
{
    if (!ARRAYP(a)) return 1;
    return (int)list_length(FIELD(a, AR_DIMS));
}

/* the length of a one-dimensional array: its fill pointer if it has one */
int array_length(obj a)
{
    if (FIELD(a, AR_FILL) != NIL) return (int)FIX(FIELD(a, AR_FILL));
    return (int)FIX(CAR(FIELD(a, AR_DIMS)));
}

/* the index into AR_DATA of the element at the NSUB subscripts SUBS, or -1 (error pending) */
static int row_major(obj a, obj *subs, int nsub)
{
    obj dims = FIELD(a, AR_DIMS);
    fixnum_t index = 0;
    int k = 0;
    for (; CONSP(dims) && k < nsub; dims = CDR(dims), k++) {
        fixnum_t d = FIX(CAR(dims));
        if (!FIXNUMP(subs[k]) || FIX(subs[k]) < 0 || FIX(subs[k]) >= d) {
            lisp_error("Index out of range:", subs[k]);
            return -1;
        }
        index = index * d + FIX(subs[k]);
    }
    if (CONSP(dims) || k != nsub) {
        lisp_error("Wrong number of subscripts for an array of rank", MAKE_FIX(array_rank(a)));
        return -1;
    }
    index += FIX(FIELD(a, AR_OFFSET));
    if (index >= data_len(FIELD(a, AR_DATA))) {
        lisp_error("Index beyond the data of a displaced array:", MAKE_FIX(index));
        return -1;
    }
    return (int)index;
}

int bit_array_p(obj a) { return ARRAYP(a) && (FIX(FIELD(a, AR_FLAGS)) & AR_BITS); }

obj array_ref(obj a, obj *subs, int nsub)
{
    int i = row_major(a, subs, nsub);
    if (i < 0) return NIL;
    if (bit_array_p(a)) return MAKE_FIX(STR_DATA(FIELD(a, AR_DATA))[i]);
    return data_ref(FIELD(a, AR_DATA), i);
}

obj array_set(obj a, obj *subs, int nsub, obj v)
{
    int i = row_major(a, subs, nsub);
    if (i < 0) return NIL;
    if (bit_array_p(a)) {
        if (v != MAKE_FIX(0) && v != MAKE_FIX(1)) return lisp_error("Not a bit (0 or 1):", v);
        STR_DATA(FIELD(a, AR_DATA))[i] = (char)FIX(v);
        return v;
    }
    return data_set(FIELD(a, AR_DATA), i, v) ? v : NIL;
}

/* #*1011: a bit vector of the N bits (bytes 0 and 1) at BITS */
obj make_bit_vector(const char *bits, int n)
{
    obj data = make_string(bits, n), dims = NIL;
    PROTECT2(data, dims);
    dims = cons(MAKE_FIX(n), NIL);
    obj a = alloc_object(HT_ARRAY, AR_NFIELDS, TAG_OBJECT);
    FIELD(a, AR_DIMS) = dims;
    FIELD(a, AR_DATA) = data;
    FIELD(a, AR_FILL) = NIL;
    FIELD(a, AR_OFFSET) = MAKE_FIX(0);
    FIELD(a, AR_FLAGS) = MAKE_FIX(AR_BITS);
    UNPROTECT(2);
    return a;
}

/* ---- primitives -------------------------------------------------------------- */

/* (%make-array dims data fill-pointer offset adjustable) */
obj ap_make_array(obj *a, int n)
{
    (void)n;
    NEED(LISTP(a[0]), "Bad array dimensions:", a[0]);
    NEED(VECTORP(a[1]) || STRINGP(a[1]), "Array data must be a vector or a string:", a[1]);
    NEED(a[2] == NIL || FIXNUMP(a[2]), "Bad fill pointer:", a[2]);
    NEED(FIXNUMP(a[3]), "Bad offset:", a[3]);
    obj x = alloc_object(HT_ARRAY, AR_NFIELDS, TAG_OBJECT);
    FIELD(x, AR_DIMS) = a[0];
    FIELD(x, AR_DATA) = a[1];
    FIELD(x, AR_FILL) = a[2];
    FIELD(x, AR_OFFSET) = a[3];
    FIELD(x, AR_FLAGS) = FIXNUMP(a[4]) ? a[4] : MAKE_FIX(a[4] != NIL ? 1 : 0);   /* flags, or adjustable-p */
    return x;
}

obj ap_array_header_p(obj *a, int n) { (void)n; return ARRAYP(a[0]) ? S_T : NIL; }

/* (%array-field array i) and (%set-array-field array i value), i = AR_* */
obj ap_array_field(obj *a, int n)
{
    (void)n;
    NEED(ARRAYP(a[0]), "Not an array header:", a[0]);
    NEED(FIXNUMP(a[1]) && FIX(a[1]) >= 0 && FIX(a[1]) < AR_NFIELDS, "Bad array field:", a[1]);
    return FIELD(a[0], FIX(a[1]));
}

obj ap_set_array_field(obj *a, int n)
{
    (void)n;
    NEED(ARRAYP(a[0]), "Not an array header:", a[0]);
    NEED(FIXNUMP(a[1]) && FIX(a[1]) >= 0 && FIX(a[1]) < AR_NFIELDS, "Bad array field:", a[1]);
    FIELD(a[0], FIX(a[1])) = a[2];
    return a[2];
}

/* #nA(...): an array of rank N from nested lists (the reader). => NIL with an
 * error pending if the contents are not a rectangle. */
static int count_into(obj x, int rank, obj vec, int *k)
{
    if (rank == 0) {
        if (vec != NIL) VEC_REF(vec, *k) = x;
        (*k)++;
        return 1;
    }
    for (; CONSP(x); x = CDR(x))
        if (!count_into(CAR(x), rank - 1, vec, k)) return 0;
    return 1;
}

obj array_from_contents(int rank, obj contents)
{
    obj dims = NIL, x = contents, v = NIL;
    PROTECT4(contents, dims, v, x);           /* x walks CONTENTS across allocations */
    for (int r = 0; r < rank; r++) {           /* the dimensions, from the first element at each level */
        fixnum_t len = list_length(x);
        if (len < 0) { UNPROTECT(4); return lisp_error("Bad contents for #A:", contents); }
        dims = cons(MAKE_FIX(len), dims);
        x = CONSP(x) ? CAR(x) : NIL;
    }
    dims = nreverse(dims);
    fixnum_t total = 1;
    for (obj d = dims; CONSP(d); d = CDR(d)) total *= FIX(CAR(d));
    v = make_vector((int)total, NIL);
    int k = 0;
    count_into(contents, rank, v, &k);
    if (k != total) { UNPROTECT(4); return lisp_error("The contents of #A are not rectangular:", contents); }
    if (rank == 1) { UNPROTECT(4); return v; }
    obj a = alloc_object(HT_ARRAY, AR_NFIELDS, TAG_OBJECT);
    FIELD(a, AR_DIMS) = dims;
    FIELD(a, AR_DATA) = v;
    FIELD(a, AR_FILL) = NIL;
    FIELD(a, AR_OFFSET) = MAKE_FIX(0);
    FIELD(a, AR_FLAGS) = MAKE_FIX(0);
    UNPROTECT(4);
    return a;
}
