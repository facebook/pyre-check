import random
import math

a = input()
b_dict = {50: a, 32: a, 57: a}
c_dict = {22: b_dict, 62: b_dict}
d_dict = {30: c_dict, 54: c_dict, 11: c_dict, 48: c_dict, 6: c_dict, 98: c_dict}
e = random.choice(list(d_dict.values()))
f = random.choice(list(e.values()))
g = random.choice(list(f.values()))
def h():
    return g
def i():
    return h()
j = i()
k_dict = {2: j, 52: j, 51: j, 90: j, 26: j, 54: j, 64: j, 76: j, 26: j, 57: j}
l_dict = {47: k_dict, 65: k_dict, 11: k_dict, 3: k_dict, 54: k_dict, 7: k_dict, 43: k_dict}
m = random.choice(list(l_dict.values()))
n = random.choice(list(m.values()))
o_dict = {20: n, 48: n, 69: n, 14: n, 10: n, 90: n, 18: n}
p_dict = {58: o_dict, 17: o_dict}
q = random.choice(list(p_dict.values()))
r = random.choice(list(q.values()))
s = r + '.'
t = f'string {s}'
u = (t, t, t)
v, w, x = u
y = v + w + x
z = (y, y, y)
aa, ab, ac = z
ad = aa + ab + ac
ae = ad + '6'
af = ae + '6'
ag = af + '1'
ah_dict = {1: ag, 64: ag, 75: ag, 31: ag, 50: ag, 87: ag}
ai_dict = {24: ah_dict, 100: ah_dict, 32: ah_dict, 27: ah_dict, 9: ah_dict, 54: ah_dict, 18: ah_dict, 27: ah_dict}
aj_dict = {77: ai_dict, 18: ai_dict, 98: ai_dict, 42: ai_dict}
ak = random.choice(list(aj_dict.values()))
al = random.choice(list(ak.values()))
am = random.choice(list(al.values()))
an = am[0:]
ao = ''
for _ in range(5):
        if _ == 5:
            continue
        ao += an
def ap():
    return ao
def aq():
    return ap()
def ar():
    return aq()
at = ar()
au_dict = {23: at, 4: at, 28: at, 54: at, 2: at, 86: at, 80: at, 81: at, 20: at}
av_dict = {89: au_dict, 59: au_dict, 1: au_dict, 18: au_dict}
aw = random.choice(list(av_dict.values()))
ax = random.choice(list(aw.values()))
def ay():
    return ax
az = ay()
ba = ''
for _ in range(5):
        if _ == 5:
            break
        ba += az
bb = ''
for _ in range(4):
    bb += ba
bc = f'string {bb}'
print(bc)