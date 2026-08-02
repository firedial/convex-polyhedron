#!/usr/bin/env python3
"""Check that polyhedra_min.json contains no two congruent solids.

Why a combinatorial test is enough
----------------------------------
Every solid in the file is convex and all of its faces are regular polygons of
edge 1 (this is what verify_exact.py proves).  By Cauchy's rigidity theorem a
convex polyhedron is determined up to isometry by its combinatorial structure
together with the shapes of its faces.  Here the face shapes are fixed by the
combinatorics alone - a k-gonal face is *the* regular k-gon of edge 1 - so

    two solids are congruent  <=>  their face structures are isomorphic.

The test therefore uses integers only; nothing depends on the coordinates.

Method
------
1. cheap invariants: (V, E, F), the numbers of faces of each size, and the
   multiset of vertex configurations (the cyclic sequence of face sizes around
   each vertex, canonicalised under rotation and reflection).  Solids that
   differ here are already proved non-congruent.
2. inside each group that survives step 1, an exact isomorphism test on the
   half-edge (flag) structure: a map is fixed by the image of a single flag,
   so all 2E images (times the two orientations) are tried and propagated.

Usage
    python3 check_distinct.py polyhedra_min.json [--verbose]
"""
import json, sys
from collections import Counter


# ------------------------------------------------------------- structure
def half_edges(faces):
    """nxt[(a,b)] = the next directed edge of the same face"""
    nxt, face_of = {}, {}
    for fi, f in enumerate(faces):
        k = len(f)
        for i in range(k):
            h = (f[i], f[(i + 1) % k])
            if h in nxt:
                raise ValueError('directed edge repeated')
            nxt[h] = (f[(i + 1) % k], f[(i + 2) % k])
            face_of[h] = fi
    return nxt, face_of


def vertex_configs(faces, nv):
    """for each vertex the cyclic sequence of face sizes around it"""
    nxt, face_of = half_edges(faces)
    out = []
    for v in range(nv):
        start = next(h for h in nxt if h[0] == v)
        h, cfg = start, []
        while True:
            cfg.append(len(faces[face_of[h]]))
            # rotate around v: next edge of this face, then its twin
            h2 = nxt[h]
            while h2[1] != v:
                h2 = nxt[h2]
            h = (h2[1], h2[0])
            if h == start:
                break
        best = min(min(tuple(cfg[i:] + cfg[:i]) for i in range(len(cfg))),
                   min(tuple(cfg[::-1][i:] + cfg[::-1][:i])
                       for i in range(len(cfg))))
        out.append(best)
    return sorted(out)


def invariant(sol):
    faces = sol['faces']
    nv = len(sol['vertices'])
    ne = len({(min(f[i], f[(i + 1) % len(f)]),
               max(f[i], f[(i + 1) % len(f)]))
              for f in faces for i in range(len(f))})
    return (nv, ne, len(faces),
            tuple(sorted(Counter(len(f) for f in faces).items())),
            tuple(vertex_configs(faces, nv)))


# ------------------------------------------------------------ isomorphism
def isomorphic(fa, fb):
    """exact isomorphism test of two oriented face structures"""
    for mirror in (False, True):
        gb = [f[::-1] for f in fb] if mirror else fb
        na, _ = half_edges(fa)
        nb, _ = half_edges(gb)
        if len(na) != len(nb):
            return False
        h0 = next(iter(na))
        for h1 in nb:
            phi = {}
            stack = [(h0, h1)]
            ok = True
            while stack and ok:
                x, y = stack.pop()
                if x in phi:
                    if phi[x] != y:
                        ok = False
                    continue
                if x not in na or y not in nb:
                    ok = False
                    break
                phi[x] = y
                stack.append((na[x], nb[y]))
                stack.append(((x[1], x[0]), (y[1], y[0])))
            if not ok or len(phi) != len(na) or len(set(phi.values())) != len(nb):
                continue
            # the induced vertex map must be well defined
            vmap = {}
            good = True
            for (a, b), (c, d) in phi.items():
                if vmap.setdefault(a, c) != c or vmap.setdefault(b, d) != d:
                    good = False
                    break
            if good:
                return True
    return False


def main():
    path = sys.argv[1] if len(sys.argv) > 1 else 'polyhedra_min.json'
    verbose = '--verbose' in sys.argv
    solids = json.load(open(path))['solids']

    groups = {}
    for s in solids:
        groups.setdefault(invariant(s), []).append(s)

    singles = sum(1 for g in groups.values() if len(g) == 1)
    print('%d solids, %d separated by the cheap invariants alone'
          % (len(solids), singles))

    dup = []
    for inv, g in sorted(groups.items(), key=lambda kv: -len(kv[1])):
        if len(g) == 1:
            continue
        if verbose:
            print('  isomorphism test on %s (V=%d E=%d F=%d)'
                  % (', '.join(x['id'] for x in g), inv[0], inv[1], inv[2]))
        for i in range(len(g)):
            for j in range(i + 1, len(g)):
                if isomorphic(g[i]['faces'], g[j]['faces']):
                    dup.append((g[i]['id'], g[j]['id']))
    if dup:
        print('DUPLICATES:', ', '.join('%s = %s' % d for d in dup))
        return 1
    print('no two solids are congruent')
    return 0


if __name__ == '__main__':
    sys.exit(main())
