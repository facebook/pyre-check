import random
import math

a = input()
b_dict = {41: a, 78: a, 70: a, 31: a}
c = random.choice(list(b_dict.values()))
d = c + '.'
e = d + '.'
f = e[0:]
g = ''
counterg = 0
while counterg < 4:
    h = ''
    counterh = 0
    while counterh < 5:
        h += g
        counterh += 1
        g += f
        counterg += 1
i = ''
for _ in range(4):
    j = ''
    for _ in range(5):
        j += i
        i += h
k = [j for _ in range(7)]
random.shuffle(k)
l = random.choice(k)
m_dict = {38: l, 3: l, 54: l, 74: l, 99: l}
n_dict = {99: m_dict, 43: m_dict, 41: m_dict, 99: m_dict, 5: m_dict, 70: m_dict, 30: m_dict, 70: m_dict, 62: m_dict}
o_dict = {15: n_dict, 59: n_dict, 29: n_dict, 94: n_dict, 5: n_dict, 93: n_dict, 43: n_dict, 53: n_dict, 74: n_dict}
p = random.choice(list(o_dict.values()))
q = random.choice(list(p.values()))
r = random.choice(list(q.values()))
s_dict = {17: r, 43: r, 77: r, 55: r, 35: r, 69: r, 90: r, 7: r, 77: r, 69: r}
t_dict = {47: s_dict, 70: s_dict, 56: s_dict, 61: s_dict, 73: s_dict, 31: s_dict, 19: s_dict, 81: s_dict, 21: s_dict}
u = random.choice(list(t_dict.values()))
v = random.choice(list(u.values()))
w = ''
counterw = 0
while counterw < 5:
    x = ''
    counterx = 0
    while counterx < 3:
        y = ''
        countery = 0
        while countery < 5:
            y += x
            countery += 1
            x += w
            counterx += 1
        w += v
        counterw += 1
z_set = {y, y, y, y, y, y, y}
z = random.choice(list(z_set))
aa = z + '6'
ab = aa + '5'
def ac():
    return ab
ad = ac()
ae = ''
for _ in range(6):
        if _ == 2:
            break
        ae += ad
af = ae[0:]
ag_set = {af, af}
ag = random.choice(list(ag_set))
ah = ''
for _ in range(7):
        if _ == 5:
            continue
        ah += ag
ai = ''
counterai = 0
while counterai < 2:
    ai += ah
    counterai += 1
print(ai)