import random
import math

a = input()
b_list = [a for _ in range(7)]
c_list = [b_list for _ in range(4)]
d_list = [c_list for _ in range(8)]
e = random.choice(d_list)
f = random.choice(e)
g = random.choice(f)
h = ''
for _ in range(6):
        if _ == 1:
            continue
        h += g
if h == h:
    k = h + 'c1'
elif h == '14':
    k = i + 'c2'
else:
    k = j + 'c3'
l = ''
for _ in range(9):
        if _ == 2:
            break
        l += k
m = ''
for _ in range(5):
        if _ == 3:
            continue
        m += l
n = m[0:]
o = ''
countero = 0
while countero < 3:
    p = ''
    counterp = 0
    while counterp < 4:
        p += o
        counterp += 1
        o += n
        countero += 1
def q():
    return p
r = q()
s_dict = {17: r, 80: r, 93: r, 2: r, 17: r, 70: r}
t_dict = {45: s_dict, 54: s_dict, 40: s_dict, 37: s_dict, 78: s_dict, 90: s_dict, 100: s_dict, 34: s_dict, 100: s_dict, 3: s_dict}
u_dict = {23: t_dict, 9: t_dict, 39: t_dict, 43: t_dict, 83: t_dict, 60: t_dict, 16: t_dict, 22: t_dict, 36: t_dict}
v = random.choice(list(u_dict.values()))
w = random.choice(list(v.values()))
x = random.choice(list(w.values()))
y = ''
countery = 0
while countery < 3:
    z = ''
    counterz = 0
    while counterz < 2:
        z += y
        counterz += 1
        y += x
        countery += 1
aa = z[0:]
ab = [aa for _ in range(7)]
random.shuffle(ab)
ac = random.choice(ab)
ad = ''
for _ in range(7):
        if _ == 5:
            continue
        ad += ac
ae_dict = {91: ad, 64: ad, 87: ad, 83: ad, 44: ad, 46: ad, 81: ad}
af_dict = {2: ae_dict, 57: ae_dict, 36: ae_dict}
ag_dict = {61: af_dict, 13: af_dict, 14: af_dict, 66: af_dict, 34: af_dict, 75: af_dict, 13: af_dict}
ah = random.choice(list(ag_dict.values()))
ai = random.choice(list(ah.values()))
aj = random.choice(list(ai.values()))
ak = (aj, aj, aj)
al, am, an = ak
ao = al + am + an
ap = f'string {ao}'
aq = f'string {ap}'
def ar():
    return aq
at = ar()
print(at)