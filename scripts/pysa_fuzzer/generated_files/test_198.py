import random
import math

a = input()
b = (a, a, a)
c, d, e = b
f = c + d + e
g = [f for _ in range(6)]
random.shuffle(g)
h = random.choice(g)
def i():
    return h
j = i()
k = j + '.'
l_dict = {91: k, 60: k, 96: k, 86: k}
m_dict = {50: l_dict, 62: l_dict, 25: l_dict, 25: l_dict}
n_dict = {33: m_dict, 90: m_dict, 46: m_dict, 11: m_dict}
o = random.choice(list(n_dict.values()))
p = random.choice(list(o.values()))
q = random.choice(list(p.values()))
r = q[0:]
s = ''
for _ in range(3):
    t = ''
    for _ in range(3):
        u = ''
        for _ in range(5):
            u += t
            t += s
        s += r
v = [u for _ in range(9)]
random.shuffle(v)
w = random.choice(v)
x_set = {w, w, w, w, w}
x = random.choice(list(x_set))
y = f'string {x}'
z = ''
for _ in range(5):
    z += y
aa = z + '.'
ab_set = {aa, aa, aa, aa, aa}
ab = random.choice(list(ab_set))
ac = [ab for _ in range(6)]
random.shuffle(ac)
ad = random.choice(ac)
ae = ad[0:]
af = ''
for _ in range(9):
        if _ == 5:
            continue
        af += ae
ag_dict = {5: af, 76: af}
ah_dict = {32: ag_dict, 74: ag_dict, 18: ag_dict}
ai_dict = {16: ah_dict, 86: ah_dict, 28: ah_dict, 5: ah_dict, 70: ah_dict, 38: ah_dict, 92: ah_dict, 51: ah_dict, 84: ah_dict}
aj = random.choice(list(ai_dict.values()))
ak = random.choice(list(aj.values()))
al = random.choice(list(ak.values()))
am = ''
counteram = 0
while counteram < 3:
    an = ''
    counteran = 0
    while counteran < 3:
        an += am
        counteran += 1
        am += al
        counteram += 1
print(an)