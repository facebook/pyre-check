import random
import math

a = input()
b_list = [a for _ in range(10)]
c = random.choice(b_list)
d = ''
for _ in range(7):
        if _ == 1:
            break
        d += c
e = d[0:]
f_list = [e for _ in range(7)]
g = random.choice(f_list)
def h():
    return g
def i():
    return h()
j = i()
k = ''
for _ in range(7):
        if _ == 1:
            continue
        k += j
l = k[0:]
m_dict = {52: l, 93: l, 89: l, 98: l, 68: l, 28: l, 22: l, 11: l, 9: l}
n = random.choice(list(m_dict.values()))
o_dict = {55: n, 82: n, 84: n, 33: n, 99: n, 75: n, 18: n}
p_dict = {3: o_dict, 14: o_dict, 52: o_dict, 94: o_dict, 31: o_dict, 16: o_dict}
q = random.choice(list(p_dict.values()))
r = random.choice(list(q.values()))
s = (r, r, r)
t, u, v = s
w = t + u + v
x = [w for _ in range(10)]
random.shuffle(x)
y = random.choice(x)
z = (y, y, y)
aa, ab, ac = z
ad = aa + ab + ac
ae = [ad for _ in range(5)]
random.shuffle(ae)
af = random.choice(ae)
ag = af + '1'
ah = ag + '6'
ai = ah + '3'
def aj():
    return ai
def ak():
    return aj()
def al():
    return ak()
am = al()
an_dict = {6: am, 41: am, 9: am}
ao_dict = {55: an_dict, 95: an_dict, 10: an_dict, 79: an_dict, 26: an_dict, 40: an_dict}
ap_dict = {30: ao_dict, 77: ao_dict, 77: ao_dict, 88: ao_dict, 63: ao_dict}
aq = random.choice(list(ap_dict.values()))
ar = random.choice(list(aq.values()))
at = random.choice(list(ar.values()))
au = at[0:]
av = ''
counterav = 0
while counterav < 2:
    aw = ''
    counteraw = 0
    while counteraw < 2:
        ax = ''
        counterax = 0
        while counterax < 5:
            ax += aw
            counterax += 1
            aw += av
            counteraw += 1
        av += au
        counterav += 1
print(ax)