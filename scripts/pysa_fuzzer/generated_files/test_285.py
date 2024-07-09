import random
import math

a = input()
b = a + '9'
c = b + '9'
d_list = [c for _ in range(7)]
e_list = [d_list for _ in range(9)]
f_list = [e_list for _ in range(7)]
g = random.choice(f_list)
h = random.choice(g)
i = random.choice(h)
j = [i for _ in range(10)]
random.shuffle(j)
k = random.choice(j)
l_dict = {43: k, 83: k, 49: k, 56: k, 70: k, 65: k, 65: k, 53: k, 10: k}
m_dict = {92: l_dict, 81: l_dict, 78: l_dict, 65: l_dict, 26: l_dict, 79: l_dict, 27: l_dict, 81: l_dict, 89: l_dict}
n_dict = {87: m_dict, 91: m_dict, 100: m_dict, 100: m_dict, 16: m_dict, 65: m_dict, 78: m_dict, 94: m_dict}
o = random.choice(list(n_dict.values()))
p = random.choice(list(o.values()))
q = random.choice(list(p.values()))
r = q + '.'
s = ''
for _ in range(3):
    t = ''
    for _ in range(2):
        t += s
        s += r
if t == t:
    w = t + 'c1'
elif t == '12':
    w = u + 'c2'
else:
    w = v + 'c3'
x = ''
for _ in range(3):
    x += w
y = (x, x, x)
z, aa, ab = y
ac = z + aa + ab
ad = ''
for _ in range(8):
        if _ == 2:
            break
        ad += ac
ae = ''
for _ in range(6):
        if _ == 3:
            break
        ae += ad
af_list = [ae for _ in range(3)]
ag = random.choice(af_list)
ah_dict = {52: ag, 71: ag, 94: ag}
ai_dict = {44: ah_dict, 77: ah_dict, 62: ah_dict, 24: ah_dict, 26: ah_dict, 93: ah_dict, 80: ah_dict, 57: ah_dict, 83: ah_dict}
aj = random.choice(list(ai_dict.values()))
ak = random.choice(list(aj.values()))
al = ''
for _ in range(3):
    for __ in range(3):
                al += ak
am = ''
for _ in range(5):
        if _ == 2:
            continue
        am += al
an = am[0:]
if an == an:
    aq = an + 'c1'
elif an == '14':
    aq = ao + 'c2'
else:
    aq = ap + 'c3'
ar = (aq, aq, aq)
at, au, av = ar
aw = at + au + av
print(aw)