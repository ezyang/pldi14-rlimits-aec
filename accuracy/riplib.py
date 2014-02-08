import locale

locale.setlocale(locale.LC_ALL, '')

lookup = {
        0: "retainer",
        1: "opl",
        3: "suml",
        4: "tree",
        5: "small",
        6: "large",
        7: "ghc",
        8: "block",
        9: "megablock"
        }

ix_t = 0
ix_n = 1
ix_k = 2 # resource limit in blocks
ix_census = 3
ix_blocks = 4
ix_valgrind = 5

init = []
for l in open("init.txt"):
    l = l.strip()
    if l == "":
        break
    init.append(int(l))

def get(r, ix):
    return locale.format("%d", delta(r,ix), grouping=True)

def delta(r, ix):
    if r:
        return r[ix] - init[ix]
    else:
        return init[ix]
