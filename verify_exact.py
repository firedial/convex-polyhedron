#!/usr/bin/env python3
"""Verify johnson_min.json *exactly*: no step of the proof relies on floating
point.

Input format
    each coordinate is [root_index, c0, c1, ...] = the root_index-th real root
    (ascending, 0-based) of c0 + c1*x + c2*x^2 + ...

How the exactness is achieved
    1. HINT (untrusted, numerical).  A high precision computation plus lattice
       reduction proposes a number field Q(theta) = Q[x]/(m(x)) and, for every
       coordinate alpha, a rational polynomial A with alpha = A(theta).
       Nothing here has to be believed - it is only a guess.
    2. CERTIFICATION (exact).  For each coordinate given as (p, i):
         a)  p(A(x)) = 0  in  Q[x]/(m(x))          [exact rational arithmetic]
             => A(theta) is a root of p
         b)  the interval evaluation of A over a certified isolating interval
             of theta is contained in the isolating interval of the i-th real
             root of p                             [exact rational intervals]
         => A(theta) is that root, i.e. alpha = A(theta) exactly.
       Both steps use only integer/rational arithmetic, so the identification
       is proved, not estimated.
    3. CHECKS (exact).  Every geometric condition is then an identity between
       rational polynomials modulo m:
         S  faces form a connected, consistently oriented closed surface,
            every edge in exactly two faces, V - E + F = 2
         E  every edge has squared length exactly 1
         P  every face is exactly flat
         G  every face is a regular polygon: all its sides are 1 (from E) and
            all its vertices are equidistant from the circumcentre of its
            first three vertices - an equilateral cyclic polygon traversed in
            cyclic order is regular
         C  convex: for every face the remaining vertices give determinants
            that are proved non-zero in Q(theta) and whose sign is decided by
            refining the isolating interval of theta until 0 is excluded
            (this terminates precisely because the value is proved non-zero)

Requires sympy (exact polynomial arithmetic, real root isolation) and mpmath
(only for the untrusted hint).  python-flint is used for lattice reduction if
available.
"""
import json, sys, time
from fractions import Fraction

import sympy as sp
from sympy import ZZ
from sympy.polys.matrices import DomainMatrix
from mpmath import mp, mpf

sys.set_int_max_str_digits(1000000)
try:
    from flint import fmpz_mat
    HAVE_FLINT = True
except ImportError:
    HAVE_FLINT = False

X = sp.Symbol('x')


# ============================ hint phase (numerical, not trusted) =========
def lll_first_row(rows):
    if HAVE_FLINT:
        M = fmpz_mat([[int(a) for a in r] for r in rows]).lll()
        return [int(M[0, j]) for j in range(M.ncols())]
    M = DomainMatrix([[ZZ(int(a)) for a in r] for r in rows],
                     (len(rows), len(rows[0])), ZZ)
    return [int(a) for a in M.lll().to_list()[0]]


def int_relation(vals, dps):
    n = len(vals)
    C = mpf(10) ** int(dps * 0.75)
    rows = []
    for i, v in enumerate(vals):
        r = [0] * (n + 1)
        r[i] = 1
        r[n] = int(mp.floor(v * C + mpf('0.5')))
        rows.append(r)
    return lll_first_row(rows)[:n]


def guess_minpoly(v, dps, maxdeg=64):
    if v == 0:
        return sp.Poly(X, X)
    for d in [d for d in list(range(1, 13)) + [14, 16, 20, 24, 32, 40, 48, 64]
              if d <= maxdeg]:
        digits = int(0.45 * dps / (d + 1))
        if digits < 5:
            break
        pw = [mpf(1)]
        for _ in range(d):
            pw.append(pw[-1] * v)
        rel = int_relation(pw, dps)
        if not rel or all(c == 0 for c in rel):
            continue
        if max(abs(c) for c in rel) > 10 ** digits:
            continue
        p = sp.Poly(sum(int(c) * X ** i for i, c in enumerate(rel)), X)
        for f, _ in sp.factor_list(p.as_expr(), X)[1]:
            f = sp.Poly(f, X)
            co = [mpf(int(c)) for c in f.all_coeffs()]
            if f.degree() >= 1 and abs(mp.polyval(co, v)) < \
                    max(abs(c) for c in co) * mpf(10) ** -int(dps * 0.7):
                g = f.monic()
                return sp.Poly(g.as_expr() * sp.lcm([sp.denom(c)
                               for c in g.all_coeffs()]), X)
    return None


def newton_root(coeffs, index, dps):
    """numerical value of the index-th real root (hint only)"""
    if len(coeffs) == 2:
        return -mpf(coeffs[0]) / mpf(coeffs[1])
    mp.dps = 60
    rough = sorted(r.real for r in mp.polyroots([mpf(c) for c in coeffs[::-1]],
                                                maxsteps=500, extraprec=800)
                   if abs(r.imag) < mpf(10) ** -25)
    x = rough[index]
    mp.dps = dps + 20
    p = [mpf(c) for c in coeffs[::-1]]
    dp = [mpf(c * i) for i, c in enumerate(coeffs)][1:][::-1]
    for _ in range(dps // 10 + 40):
        s = mp.polyval(p, x) / mp.polyval(dp, x)
        x -= s
        if abs(s) < mpf(10) ** -(dps + 10):
            break
    return x


def hint_field(vals, dps, tries=2):
    """propose (m, theta_numeric, representations)"""
    import random
    for t in range(tries + min(len(vals), 10)):
        if t < tries:
            random.seed(t)
            th = sum(random.randint(1, 5) * v for v in vals)
        else:
            th = vals[t - tries]
        m = guess_minpoly(th, dps)
        if m is None:
            continue
        d = m.degree()
        pw = [mpf(1)]
        for _ in range(d):
            pw.append(pw[-1] * th)
        reps, ok = [], True
        for v in vals:
            rel = int_relation([v] + pw[:d], dps)
            if not rel or rel[0] == 0:
                ok = False
                break
            reps.append(tuple(Fraction(-rel[i + 1], rel[0]) for i in range(d)))
        if not ok:
            continue
        # a spurious lattice relation only matches to about 0.75*dps digits,
        # a genuine representation to the full working precision
        good = True
        for rp, v in zip(reps, vals):
            val = mpf(0)
            for c in reversed(rp):
                val = val * th + mpf(c.numerator) / mpf(c.denominator)
            if abs(val - v) > mpf(10) ** -int(dps * 0.9):
                good = False
                break
        if good:
            return m, th, reps
    return None, None, None


# ============================ exact arithmetic ===========================
class Field:
    """Q[x]/(m), elements are tuples of Fractions (m irreducible)"""

    def __init__(self, m):
        self.m = m
        self.d = m.degree()
        c = [Fraction(int(a)) for a in m.all_coeffs()]
        self.red = [-a / c[0] for a in c[1:]]

    def reduce(self, a):
        a = list(a)
        while len(a) > self.d:
            top = a.pop()
            k = len(a)
            if top:
                for i, r in enumerate(self.red):
                    a[k - 1 - i] += top * r
        while len(a) < self.d:
            a.append(Fraction(0))
        return tuple(a)

    zero = property(lambda self: tuple([Fraction(0)] * self.d))

    def const(self, q):
        return self.reduce([Fraction(q)])

    def add(self, a, b):
        return tuple(p + q for p, q in zip(a, b))

    def sub(self, a, b):
        return tuple(p - q for p, q in zip(a, b))

    def mul(self, a, b):
        out = [Fraction(0)] * (2 * self.d - 1)
        for i, p in enumerate(a):
            if p:
                for j, q in enumerate(b):
                    if q:
                        out[i + j] += p * q
        return self.reduce(out)

    def scal(self, q, a):
        return tuple(Fraction(q) * t for t in a)

    def _poly(self, a):
        return sp.Poly([sp.Rational(t.numerator, t.denominator)
                        for t in reversed(a)], X, domain='QQ')

    def inv(self, a):
        """inverse in Q[x]/(m) via the extended Euclidean algorithm"""
        A = self._poly(a)
        M = sp.Poly(self.m, X, domain='QQ')
        B = sp.invert(A, M)
        return self.reduce([Fraction(int(sp.numer(c)), int(sp.denom(c)))
                            for c in reversed(B.all_coeffs())])


class Root:
    """the k-th real root (ascending) of an integer polynomial, with a
    certified isolating interval refinable by exact bisection"""

    def __init__(self, poly, k):
        p = sp.Poly(poly, X)
        self.coeffs = [Fraction(int(c)) for c in p.all_coeffs()]
        if p.degree() == 1:
            r = -self.coeffs[1] / self.coeffs[0]
            self.lo = self.hi = r
            return
        ivs = sp.Poly(p, X).intervals(sqf=True)     # exact root isolation
        if k >= len(ivs):
            raise ValueError('root index out of range')
        (a, b) = ivs[k][0] if isinstance(ivs[k][0], tuple) else ivs[k]
        self.lo = Fraction(int(sp.numer(sp.Rational(a))),
                           int(sp.denom(sp.Rational(a))))
        self.hi = Fraction(int(sp.numer(sp.Rational(b))),
                           int(sp.denom(sp.Rational(b))))
        if self.lo == self.hi:
            return
        while self._val(self.lo) == 0:
            self.lo -= (self.hi - self.lo)
        while self._val(self.hi) == 0:
            self.hi += (self.hi - self.lo)

    def _val(self, t):
        s = Fraction(0)
        for c in self.coeffs:
            s = s * t + c
        return s

    def refine(self):
        if self.lo == self.hi:
            return
        mid = (self.lo + self.hi) / 2
        v = self._val(mid)
        if v == 0:
            self.lo = self.hi = mid
        elif (self._val(self.lo) > 0) == (v > 0):
            self.lo = mid
        else:
            self.hi = mid

    def width(self):
        return self.hi - self.lo


def ival_eval(coeffs, lo, hi):
    """interval value of sum coeffs[i] x^i for x in [lo, hi] (Horner)"""
    a = b = Fraction(0)
    for c in reversed(coeffs):
        prods = [a * lo, a * hi, b * lo, b * hi]
        a, b = min(prods) + c, max(prods) + c
    return a, b


def certify(coeffs, rep, theta):
    """prove that the given (poly, root index) equals rep(theta)"""
    poly, k = coeffs[1:], coeffs[0]
    K = certify.K
    # (a)  p(rep) == 0 in Q[x]/(m)
    acc = K.zero
    for c in reversed(poly):
        acc = K.add(K.mul(acc, rep), K.const(c))
    if acc != K.zero:
        return False
    # (b)  interval of rep(theta) inside the isolating interval of the root
    target = Root(sum(int(c) * X ** i for i, c in enumerate(poly)), k)
    # the isolating interval of the target contains exactly one root of p, so
    # it is enough to squeeze rep(theta) inside it; only theta gets refined
    for _ in range(3000):
        lo, hi = ival_eval(list(rep), theta.lo, theta.hi)
        if target.lo <= lo and hi <= target.hi:
            return True
        if hi < target.lo or lo > target.hi:
            return False
        theta.refine()
    return False


def sign_of(elem, theta, K):
    """exact sign of a non-zero element of Q(theta)"""
    if elem == K.zero:
        return 0
    for _ in range(3000):
        lo, hi = ival_eval(list(elem), theta.lo, theta.hi)
        if lo > 0:
            return 1
        if hi < 0:
            return -1
        theta.refine()
    raise RuntimeError('sign could not be decided')


def det3(M, K):
    return K.sub(K.add(K.mul(M[0][0], K.sub(K.mul(M[1][1], M[2][2]),
                                            K.mul(M[1][2], M[2][1]))),
                       K.mul(M[0][2], K.sub(K.mul(M[1][0], M[2][1]),
                                            K.mul(M[1][1], M[2][0])))),
                 K.mul(M[0][1], K.sub(K.mul(M[1][0], M[2][2]),
                                      K.mul(M[1][2], M[2][0]))))


def solve3(A, b, K):
    """exact 3x3 solve by Cramer's rule over Q(theta)"""
    D = det3(A, K)
    if D == K.zero:
        return None
    Di = K.inv(D)
    out = []
    for c in range(3):
        M = [[b[r] if cc == c else A[r][cc] for cc in range(3)]
             for r in range(3)]
        out.append(K.mul(det3(M, K), Di))
    return out


# ============================ structural check ===========================
def check_structure(nv, faces):
    directed, undirected = {}, set()
    for f in faces:
        for i in range(len(f)):
            a, b = f[i], f[(i + 1) % len(f)]
            directed[(a, b)] = directed.get((a, b), 0) + 1
            undirected.add((min(a, b), max(a, b)))
    if any(c != 1 for c in directed.values()):
        return None, 'faces not consistently oriented'
    if len(directed) != 2 * len(undirected):
        return None, 'an edge is not shared by exactly two faces'
    if nv - len(undirected) + len(faces) != 2:
        return None, 'Euler characteristic is not 2'
    adj = {i: set() for i in range(nv)}
    for a, b in undirected:
        adj[a].add(b)
        adj[b].add(a)
    seen, st = {0}, [0]
    while st:
        for w in adj[st.pop()]:
            if w not in seen:
                seen.add(w)
                st.append(w)
    if len(seen) != nv:
        return None, 'vertex graph not connected'
    return sorted(undirected), None


# ============================ per solid =================================
def verify_solid(sol, dps):
    faces = sol['faces']
    nv = len(sol['vertices'])
    edges, err = check_structure(nv, faces)
    if err:
        return {'ok': False, 'why': err}

    # ---- hint
    mp.dps = dps + 20
    num = [[newton_root(c[1:], c[0], dps) for c in row]
           for row in sol['vertices']]
    mp.dps = dps
    vals, idx = [], {}
    for row in num:
        for v in row:
            if v == 0:
                continue
            key = mp.nstr(abs(v), 25)
            if key not in idx:
                idx[key] = len(vals)
                vals.append(abs(v))
    m, thnum, reps = hint_field(vals, dps)
    if m is None:
        return {'ok': False, 'why': 'no number field found (raise precision)'}
    if len(sp.factor_list(m.as_expr(), X)[1]) != 1:
        return {'ok': False, 'why': 'proposed minimal polynomial is reducible'}
    K = Field(m)
    certify.K = K
    # which real root of m is theta?
    ivs = sp.Poly(m, X).intervals(sqf=True)
    kth = None
    for t, iv in enumerate(ivs):
        a, b = iv[0] if isinstance(iv[0], tuple) else iv
        if sp.Rational(a) <= sp.Float(mp.nstr(thnum, 30), 30) <= sp.Rational(b):
            kth = t
            break
    if kth is None:
        return {'ok': False, 'why': 'theta not located'}
    theta = Root(m, kth)

    # ---- certification
    V = []
    for row, nrow in zip(sol['vertices'], num):
        r = []
        for c, v in zip(row, nrow):
            if v == 0:
                if c[1:] != [0, 1]:
                    return {'ok': False, 'why': 'zero coordinate mismatch'}
                r.append(K.zero)
                continue
            rep = K.reduce(reps[idx[mp.nstr(abs(v), 25)]])
            if v < 0:
                rep = K.scal(-1, rep)
            if not certify(c, rep, theta):
                return {'ok': False, 'why': 'coordinate could not be certified'}
            r.append(rep)
        V.append(r)

    res = {'degree': m.degree()}
    one = K.const(1)

    def sqdist(i, j):
        s = K.zero
        for c in range(3):
            t = K.sub(V[i][c], V[j][c])
            s = K.add(s, K.mul(t, t))
        return s

    res['E'] = all(sqdist(a, b) == one for a, b in edges)

    # one normal per face, then plane checks are single dot products
    normals = []
    for f in faces:
        a = V[f[0]]
        u = [K.sub(V[f[1]][c], a[c]) for c in range(3)]
        v = [K.sub(V[f[2]][c], a[c]) for c in range(3)]
        normals.append([K.sub(K.mul(u[1], v[2]), K.mul(u[2], v[1])),
                        K.sub(K.mul(u[2], v[0]), K.mul(u[0], v[2])),
                        K.sub(K.mul(u[0], v[1]), K.mul(u[1], v[0]))])

    def plane_val(fi, j):
        f, n = faces[fi], normals[fi]
        a = V[f[0]]
        s = K.zero
        for c in range(3):
            s = K.add(s, K.mul(n[c], K.sub(V[j][c], a[c])))
        return s

    okP = all(plane_val(fi, j) == K.zero
              for fi, f in enumerate(faces) for j in f[3:])
    res['P'] = okP

    okG = True
    for f in faces:
        if len(f) == 3:
            continue                       # equilateral triangle is regular
        p0, p1, p2 = V[f[0]], V[f[1]], V[f[2]]
        u = [K.sub(p1[c], p0[c]) for c in range(3)]
        v = [K.sub(p2[c], p0[c]) for c in range(3)]
        n = [K.sub(K.mul(u[1], v[2]), K.mul(u[2], v[1])),
             K.sub(K.mul(u[2], v[0]), K.mul(u[0], v[2])),
             K.sub(K.mul(u[0], v[1]), K.mul(u[1], v[0]))]
        A = [u, v, n]
        half = Fraction(1, 2)
        b = [K.scal(half, K.mul(u[0], u[0])), K.zero, K.zero]
        b[0] = K.scal(half, K.add(K.add(K.mul(u[0], u[0]), K.mul(u[1], u[1])),
                                  K.mul(u[2], u[2])))
        b[1] = K.scal(half, K.add(K.add(K.mul(v[0], v[0]), K.mul(v[1], v[1])),
                                  K.mul(v[2], v[2])))
        b[2] = K.zero
        cen = solve3(A, b, K)
        if cen is None:
            okG = False
            continue
        r0 = K.zero
        for c in range(3):
            r0 = K.add(r0, K.mul(cen[c], cen[c]))
        for j in f[2:]:
            d = K.zero
            for c in range(3):
                t = K.sub(K.sub(V[j][c], p0[c]), cen[c])
                d = K.add(d, K.mul(t, t))
            if d != r0:
                okG = False
    res['G'] = okG

    # Convexity.  A certified interval enclosure of each coordinate (obtained
    # from the isolating interval of theta) already proves both that the
    # determinant is non-zero and what its sign is, whenever the enclosure
    # excludes 0; only in the rare undecided case do we fall back to exact
    # arithmetic in Q(theta).
    def enclosures():
        return [[ival_eval(list(c), theta.lo, theta.hi) for c in row]
                for row in V]

    def isub(a, b):
        return (a[0] - b[1], a[1] - b[0])

    def imul(a, b):
        p = (a[0] * b[0], a[0] * b[1], a[1] * b[0], a[1] * b[1])
        return (min(p), max(p))

    def iadd(a, b):
        return (a[0] + b[0], a[1] + b[1])

    while theta.width() > Fraction(1, 10 ** 50):   # refine once, up front
        theta.refine()
    okC = True
    IV = enclosures()
    for fi, f in enumerate(faces):
        sign = 0
        for j in range(nv):
            if j in f:
                continue
            s = 0
            for attempt in range(60):
                a = IV[f[0]]
                u = [isub(IV[f[1]][c], a[c]) for c in range(3)]
                v = [isub(IV[f[2]][c], a[c]) for c in range(3)]
                w = [isub(IV[j][c], a[c]) for c in range(3)]
                n = [isub(imul(u[1], v[2]), imul(u[2], v[1])),
                     isub(imul(u[2], v[0]), imul(u[0], v[2])),
                     isub(imul(u[0], v[1]), imul(u[1], v[0]))]
                d = iadd(iadd(imul(n[0], w[0]), imul(n[1], w[1])),
                         imul(n[2], w[2]))
                if d[0] > 0:
                    s = 1
                    break
                if d[1] < 0:
                    s = -1
                    break
                theta.refine()
                IV = enclosures()
            if s == 0:                       # undecided: decide exactly
                if plane_val(fi, j) == K.zero:
                    okC = False
                    continue
                s = sign_of(plane_val(fi, j), theta, K)
            if sign == 0:
                sign = s
            elif sign != s:
                okC = False
    res['C'] = okC
    res['ok'] = all(res[t] for t in 'EPGC')
    return res


def main():
    path = sys.argv[1] if len(sys.argv) > 1 else 'johnson_min.json'
    only = None
    if '--only' in sys.argv:
        only = set(sys.argv[sys.argv.index('--only') + 1].split(','))
    data = json.load(open(path))
    if '--selftest' in sys.argv:
        import copy
        for sol0 in data['solids']:
            sol = copy.deepcopy(sol0)
            hit = False
            for row in sol['vertices']:
                for c in row:
                    if len(c) > 3 and not hit:      # several real roots
                        c[0] = 1 - c[0]             # pick a conjugate instead
                        hit = True
            if hit:
                break
        r = verify_solid(sol, 400)
        ok = not r.get('ok')
        print('selftest on %s (one root replaced by its conjugate): %s  %s' %
              (sol['id'], 'FAILS as it must' if ok else 'PASSED - BUG!',
               {k: v for k, v in r.items() if k != 'degree'}))
        return 0 if ok else 1
    t0 = time.time()
    bad, n = [], 0
    for sol in data['solids']:
        if only and sol['id'] not in only:
            continue
        n += 1
        for dps in (400, 1500, 4000, 9000):
            r = verify_solid(sol, dps)
            if r.get('ok'):
                break
        if not r.get('ok'):
            bad.append(sol['id'])
        print('%-5s %s deg=%-3s E=%s P=%s G=%s C=%s %s' %
              (sol['id'], 'OK  ' if r.get('ok') else 'FAIL', r.get('degree', '-'),
               r.get('E'), r.get('P'), r.get('G'), r.get('C'),
               r.get('why', '')), flush=True)
    print('\n%d solids, %d failed%s  (%.0f s)' %
          (n, len(bad), (': ' + ', '.join(bad)) if bad else '', time.time() - t0))
    return 1 if bad else 0


if __name__ == '__main__':
    sys.exit(main())
