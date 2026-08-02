# convex-polyhedron

環境構築

```
python3 -m venv ~/.venvs/poly && ~/.venvs/poly/bin/pip install sympy mpmath python-flint

```

整面凸多面体であることのチェック

```
~/.venvs/poly/bin/python verify_exact.py polyhedra_min.json
```

同じ多面体が含まれていないことのチェック

```
~/.venvs/poly/bin/python check_distinct.py
```
