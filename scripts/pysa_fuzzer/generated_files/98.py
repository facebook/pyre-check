import random
import math
a = input()
def b():
    return a
def c():
    return b()
def d():
    return c()
e = d()
f_dict = {93: e, 7: e, 100: e, 50: e, 45: e, 93: e, 84: e}
g = random.choice(list(f_dict.values()))
h = ''
for _ in range(5):
        if _ == 4:
            continue
        h += g
i = ''
for _ in range(4):
    for __ in range(5):
                i += h
def j():
    return i
k = j()
l = k + '.'
m_dict = {86: l, 55: l, 50: l, 34: l, 94: l, 66: l}
n_dict = {96: m_dict, 27: m_dict, 5: m_dict, 98: m_dict, 18: m_dict, 100: m_dict}
o = random.choice(list(n_dict.values()))
p = random.choice(list(o.values()))
def q():
    return p
def r():
    return q()
def s():
    return r()
t = s()
def u():
    return t
def v():
    return u()
def w():
    return v()
x = w()
y_list = [x for _ in range(2)]
z_list = [y_list for _ in range(6)]
aa_list = [z_list for _ in range(2)]
ab = random.choice(aa_list)
ac = random.choice(ab)
ad = random.choice(ac)
ae = f'string {ad}'
af = ae + '.'
ag = f'string {af}'
ah = (ag, ag, ag)
ai, aj, ak = ah
al = ai + aj + ak
if al == '4':
    am = al + ' c1'
elif al == '11':
    am = al + ' c2'
else:
    am = al + ' c3'
an = ''
counteran = 0
while counteran < 5:
    ao = ''
    counterao = 0
    while counterao < 5:
        ap = ''
        counterap = 0
        while counterap < 3:
            ap += ao
            counterap += 1
            ao += an
            counterao += 1
        an += am
        counteran += 1
aq = [ap for _ in range(9)]
random.shuffle(aq)
ar = random.choice(aq)
def at():
    return ar
au = at()
print(au)