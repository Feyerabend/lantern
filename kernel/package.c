/* package.c — packages: separate namespaces of symbols (M12).
 *
 * A package is a Lisp vector, #(:package name nicknames table use-list),
 * like the other Lisp-level objects; world/packages.lisp builds the rest of
 * the package system on the primitives here. The table is a vector of
 * buckets (lists of symbols), hashed by name as the obarray is.
 *
 * COMMON-LISP is the system's own package, and its table is the obarray: the
 * whole world, as before packages. All of its symbols count as external, so
 * a package that uses it sees every one. Keywords stay in the obarray with
 * the colon in their names (:FOO is named ":FOO"); their home package is
 * KEYWORD, which has no table of its own. Every symbol records its home
 * package (NIL for uninterned ones), and an exported symbol has
 * SYMF_EXTERNAL set.
 *
 * The reader interns in *PACKAGE* and reads pkg:name and pkg::name; the
 * printer writes the prefix when a symbol is not accessible in *PACKAGE*.
 * Neither allocates more than the symbol it creates.
 */
#include "lisp.h"

obj lisp_package = NIL, keyword_package = NIL, user_package = NIL, package_list = NIL;
obj S_STAR_PACKAGE = NIL;

#define TABLE_SIZE 211

/* names in C are UTF-8, and hash by their characters (text.c) */
#define name_hash utf8_hash

static obj K_PACKAGE = NIL;    /* :PACKAGE, the tag */

int packagep(obj x)
{
    return K_PACKAGE != NIL && VECTORP(x) && VEC_LEN(x) == PK_NFIELDS && VEC_REF(x, 0) == K_PACKAGE;
}

obj current_package(void)
{
    obj p = SYM(S_STAR_PACKAGE, SYM_VALUE);
    return packagep(p) ? p : lisp_package;
}

#define str_eq str_eq_utf8

obj find_package(const char *s, int n)
{
    for (obj l = package_list; l != NIL; l = CDR(l)) {
        obj p = CAR(l);
        if (str_eq(VEC_REF(p, PK_NAME), s, n)) return p;
        for (obj k = VEC_REF(p, PK_NICKNAMES); CONSP(k); k = CDR(k))
            if (str_eq(CAR(k), s, n)) return p;
    }
    return NIL;
}

static obj table_find(obj table, const char *s, int n)
{
    if (!VECTORP(table)) return UNBOUND;
    uint32_t h = name_hash(s, n) % (uint32_t)VEC_LEN(table);
    for (obj b = VEC_REF(table, h); b != NIL; b = CDR(b))
        if (str_eq(SYM(CAR(b), SYM_NAME), s, n)) return CAR(b);
    return UNBOUND;
}

/* put SYM into TABLE (no check for an existing one) */
void table_add(obj table, obj sym)
{
    obj name = SYM(sym, SYM_NAME);
    uint32_t h = str_hash(name) % (uint32_t)VEC_LEN(table);
    PROTECT1(table);
    obj cell = cons(sym, VEC_REF(table, h));
    UNPROTECT(1);
    VEC_REF(table, h) = cell;
}

int table_remove(obj table, obj sym)
{
    if (!VECTORP(table)) return 0;
    obj name = SYM(sym, SYM_NAME);
    uint32_t h = str_hash(name) % (uint32_t)VEC_LEN(table);
    obj prev = NIL;
    for (obj b = VEC_REF(table, h); b != NIL; prev = b, b = CDR(b))
        if (CAR(b) == sym) {
            if (prev == NIL) VEC_REF(table, h) = CDR(b);
            else CDR(prev) = CDR(b);
            return 1;
        }
    return 0;
}

/* Is SYM external in PKG? Exported from its home package (the flag), or
 * exported from PKG though it lives elsewhere (PK_EXTERNALS). Every symbol
 * of COMMON-LISP is external. */
static int externalp(obj sym, obj pkg)
{
    if (pkg == lisp_package) return 1;
    if ((SYM_FLAGS_OF(sym) & SYMF_EXTERNAL) && SYM(sym, SYM_PACKAGE) == pkg) return 1;
    for (obj l = VEC_REF(pkg, PK_EXTERNALS); CONSP(l); l = CDR(l))
        if (CAR(l) == sym) return 1;
    return 0;
}

/* the symbol named S accessible in PKG, or UNBOUND; *STATUS is PS_INTERNAL,
 * PS_EXTERNAL or PS_INHERITED. Keywords are looked up with their colon. */
obj pkg_find_symbol(obj pkg, const char *s, int n, int *status)
{
    *status = 0;
    if (pkg == keyword_package) {
        static char kbuf[1024];
        if (n + 1 > (int)sizeof kbuf) return UNBOUND;
        kbuf[0] = ':';
        memcpy(kbuf + 1, s, (__SIZE_TYPE__)n);
        obj k = table_find(obarray, kbuf, n + 1);
        if (k != UNBOUND) *status = PS_EXTERNAL;
        return k;
    }
    if (n == 3 && k_memeq(s, "NIL", 3) && pkg == lisp_package) { *status = PS_EXTERNAL; return NIL; }
    obj sym = table_find(VEC_REF(pkg, PK_TABLE), s, n);
    if (sym != UNBOUND) {
        *status = externalp(sym, pkg) ? PS_EXTERNAL : PS_INTERNAL;
        return sym;
    }
    for (obj u = VEC_REF(pkg, PK_USE); CONSP(u); u = CDR(u)) {
        obj used = CAR(u);
        if (used == lisp_package && n == 3 && k_memeq(s, "NIL", 3)) { *status = PS_INHERITED; return NIL; }
        obj x = table_find(VEC_REF(used, PK_TABLE), s, n);
        if (x != UNBOUND && (used == lisp_package ? (n == 0 || s[0] != ':') : externalp(x, used))) {
            *status = PS_INHERITED;
            return x;
        }
    }
    return UNBOUND;
}

/* the symbol named S in PKG, made there if there is none. S must not point into the heap. */
obj pkg_intern(obj pkg, const char *s, int n)
{
    if (pkg == lisp_package || (n > 0 && s[0] == ':')) return intern(s, n);
    if (pkg == keyword_package) {
        static char kbuf[1024];
        if (n + 1 > (int)sizeof kbuf) return lisp_error("Symbol name too long", UNBOUND);
        kbuf[0] = ':';
        memcpy(kbuf + 1, s, (__SIZE_TYPE__)n);
        return intern(kbuf, n + 1);
    }
    int status;
    obj found = pkg_find_symbol(pkg, s, n, &status);
    if (found != UNBOUND) return found;
    PROTECT1(pkg);
    obj sym = make_symbol(make_string_utf8(s, n));
    SYM(sym, SYM_PACKAGE) = pkg;
    PROTECT1(sym);
    table_add(VEC_REF(pkg, PK_TABLE), sym);
    UNPROTECT(2);
    return sym;
}

/* Does SYM print without a prefix in the current package? */
int symbol_accessible(obj sym)
{
    obj home = SYM(sym, SYM_PACKAGE), cur = current_package();
    if (home == cur && (cur == lisp_package)) return 1;
    static char buf[1024];
    int n = str_utf8(SYM(sym, SYM_NAME), buf, (int)sizeof buf), status;
    if (n > (int)sizeof buf) return 0;
    return pkg_find_symbol(cur, buf, n, &status) == sym;
}

int symbol_external(obj sym)
{
    obj home = SYM(sym, SYM_PACKAGE);
    return home == lisp_package || (SYM_FLAGS_OF(sym) & SYMF_EXTERNAL);
}

obj make_package(obj name, obj nicknames, obj use, int with_table)
{
    PROTECT3(name, nicknames, use);
    obj table = with_table ? make_vector(TABLE_SIZE, NIL) : NIL;
    PROTECT1(table);
    obj p = make_vector(PK_NFIELDS, NIL);
    UNPROTECT(1);
    VEC_REF(p, 0) = K_PACKAGE;
    VEC_REF(p, PK_NAME) = name;
    VEC_REF(p, PK_NICKNAMES) = nicknames;
    VEC_REF(p, PK_TABLE) = table;
    VEC_REF(p, PK_USE) = use;
    PROTECT1(p);
    package_list = cons(p, package_list);
    UNPROTECT(4);
    return p;
}

static obj strings(const char **names)
{
    obj r = NIL;
    PROTECT1(r);
    for (int i = 0; names[i]; i++) {
        int n = 0;
        while (names[i][n]) n++;
        obj s = make_string(names[i], n);
        r = cons(s, r);
    }
    UNPROTECT(1);
    return nreverse(r);
}

static obj cstring(const char *s)
{
    int n = 0;
    while (s[n]) n++;
    return make_string(s, n);
}

/* after the first symbols exist: the three standard packages */
void packages_init(void)
{
    static obj *const roots[] = { &K_PACKAGE, &lisp_package, &keyword_package, &user_package, &package_list, &S_STAR_PACKAGE };
    for (unsigned i = 0; i < sizeof roots / sizeof roots[0]; i++) gc_global(roots[i]);

    K_PACKAGE = intern_c(":PACKAGE");
    static const char *cl_nicks[] = { "CL", "LISP", "SYSTEM", "SYS", 0 };
    static const char *user_nicks[] = { "CL-USER", "USER", 0 };
    lisp_package = make_package(cstring("COMMON-LISP"), strings(cl_nicks), NIL, 0);
    VEC_REF(lisp_package, PK_TABLE) = obarray;
    keyword_package = make_package(cstring("KEYWORD"), NIL, NIL, 0);
    obj use = cons(lisp_package, NIL);
    user_package = make_package(cstring("COMMON-LISP-USER"), strings(user_nicks), use, 1);

    /* the symbols made so far */
    for (int h = 0; h < VEC_LEN(obarray); h++)
        for (obj b = VEC_REF(obarray, h); b != NIL; b = CDR(b)) {
            obj s = CAR(b), name = SYM(s, SYM_NAME);
            SYM(s, SYM_PACKAGE) = STR_LEN(name) > 0 && str_ref(name, 0) == ':' ? keyword_package : lisp_package;
        }

    S_STAR_PACKAGE = intern_c("*PACKAGE*");
    SYM(S_STAR_PACKAGE, SYM_VALUE) = lisp_package;
    SYM(S_STAR_PACKAGE, SYM_FLAGS) = MAKE_FIX(SYM_FLAGS_OF(S_STAR_PACKAGE) | SYMF_SPECIAL);
}

/* ---- primitives ------------------------------------------------------------- */

#define NEED(cond, msg, x) do { if (!(cond)) return lisp_error(msg, x); } while (0)
#define NEED_PKG(x)  NEED(packagep(x), "Not a package:", x)
#define NEED_STR(x)  NEED(STRINGP(x), "Not a string:", x)
#define NEED_SYMB(x) NEED(TAG(x) == TAG_SYMBOL, "Not a symbol:", x)

static char namebuf[4096];

/* S as UTF-8 in namebuf */
static int copy_name(obj s)
{
    int n = str_utf8(s, namebuf, (int)sizeof namebuf - 1);
    return n < (int)sizeof namebuf ? n : (int)sizeof namebuf - 1;
}

obj pk_package_list(obj *a, int n) { (void)a; (void)n; return package_list; }

obj pk_find_package(obj *a, int n)
{
    (void)n;
    NEED_STR(a[0]);
    int len = copy_name(a[0]);
    return find_package(namebuf, len);
}

/* (%make-package name nicknames use-list) */
obj pk_make_package(obj *a, int n)
{
    (void)n;
    NEED_STR(a[0]);
    int len = copy_name(a[0]);
    NEED(find_package(namebuf, len) == NIL, "A package with this name exists already:", a[0]);
    return make_package(a[0], a[1], a[2], 1);
}

/* (%find-symbol name package) => (symbol status) or NIL */
obj pk_find_symbol(obj *a, int n)
{
    (void)n;
    NEED_STR(a[0]);
    NEED_PKG(a[1]);
    int len = copy_name(a[0]), status;
    obj s = pkg_find_symbol(a[1], namebuf, len, &status);
    if (s == UNBOUND) return NIL;
    const char *st = status == PS_EXTERNAL ? ":EXTERNAL" : status == PS_INHERITED ? ":INHERITED" : ":INTERNAL";
    PROTECT1(s);
    obj k = intern_c(st);
    UNPROTECT(1);
    return list2(s, k);
}

obj pk_intern(obj *a, int n)
{
    NEED_STR(a[0]);
    obj pkg = n > 1 ? a[1] : current_package();
    NEED_PKG(pkg);
    int len = copy_name(a[0]);
    return pkg_intern(pkg, namebuf, len);
}

obj pk_symbol_package(obj *a, int n)
{
    (void)n;
    if (a[0] == NIL) return lisp_package;
    NEED_SYMB(a[0]);
    return SYM(a[0], SYM_PACKAGE);
}

/* (%package-add symbol package): make SYMBOL present in PACKAGE (import);
 * an uninterned symbol gets PACKAGE as its home */
obj pk_package_add(obj *a, int n)
{
    (void)n;
    NEED_SYMB(a[0]);
    NEED_PKG(a[1]);
    NEED(VECTORP(VEC_REF(a[1], PK_TABLE)) && a[1] != lisp_package, "Cannot add symbols to this package:", a[1]);
    if (SYM(a[0], SYM_PACKAGE) == NIL) SYM(a[0], SYM_PACKAGE) = a[1];
    table_add(VEC_REF(a[1], PK_TABLE), a[0]);
    return a[0];
}

obj pk_package_remove(obj *a, int n)
{
    (void)n;
    NEED_SYMB(a[0]);
    NEED_PKG(a[1]);
    int removed = table_remove(VEC_REF(a[1], PK_TABLE), a[0]);
    if (removed && SYM(a[0], SYM_PACKAGE) == a[1]) SYM(a[0], SYM_PACKAGE) = NIL;
    return removed ? S_T : NIL;
}

obj pk_set_external(obj *a, int n)
{
    (void)n;
    NEED_SYMB(a[0]);
    fixnum_t f = SYM_FLAGS_OF(a[0]);
    SYM(a[0], SYM_FLAGS) = MAKE_FIX(a[1] != NIL ? (f | SYMF_EXTERNAL) : (f & ~SYMF_EXTERNAL));
    return a[1];
}

obj pk_externalp(obj *a, int n)
{
    (void)n;
    if (a[0] == NIL) return S_T;
    NEED_SYMB(a[0]);
    return symbol_external(a[0]) ? S_T : NIL;
}

obj pk_delete_package(obj *a, int n)
{
    (void)n;
    NEED_PKG(a[0]);
    NEED(a[0] != lisp_package && a[0] != keyword_package, "Cannot delete this package:", a[0]);
    obj prev = NIL;
    for (obj l = package_list; l != NIL; prev = l, l = CDR(l))
        if (CAR(l) == a[0]) {
            if (prev == NIL) package_list = CDR(l);
            else CDR(prev) = CDR(l);
            return S_T;
        }
    return NIL;
}
