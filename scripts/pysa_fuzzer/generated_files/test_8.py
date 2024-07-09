import random
import math

a = input()
b = ''
counterb = 0
while counterb < 5:
    c = ''
    counterc = 0
    while counterc < 2:
        d = ''
        counterd = 0
        while counterd < 5:
            d += c
            counterd += 1
            c += b
            counterc += 1
        b += a
        counterb += 1
e = f'string {d}'
f = e[0:]
g_dict = {41: f, 84: f, 52: f, 23: f, 30: f, 33: f, 1: f}
h_dict = {70: g_dict, 77: g_dict, 94: g_dict}
i = random.choice(list(h_dict.values()))
j = random.choice(list(i.values()))
k = [j for _ in range(10)]
random.shuffle(k)
l = random.choice(k)
m_dict = {76: l, 34: l, 54: l, 99: l, 93: l, 17: l, 93: l}
n = random.choice(list(m_dict.values()))
o = n + '4'
p = o + '1'
q = p + '9'
r = ''
counterr = 0
while counterr < 4:
    s = ''
    counters = 0
    while counters < 3:
        s += r
        counters += 1
        r += q
        counterr += 1
t = ''
countert = 0
while countert < 2:
    u = ''
    counteru = 0
    while counteru < 3:
        u += t
        counteru += 1
        t += s
        countert += 1
v = u[0:]
w = (v, v, v)
x, y, z = w
aa = x + y + z
ab_set = {aa, aa, aa, aa, aa, aa, aa, aa, aa, aa}
ab = random.choice(list(ab_set))
ac = ab[0:]
ad = (ac, ac, ac)
ae, af, ag = ad
ah = ae + af + ag
ai_dict = {6: ah, 63: ah, 99: ah, 92: ah}
aj_dict = {16: ai_dict, 97: ai_dict, 61: ai_dict, 3: ai_dict, 27: ai_dict, 100: ai_dict, 42: ai_dict}
ak = random.choice(list(aj_dict.values()))
al = random.choice(list(ak.values()))
def am():
    return al
def an():
    return am()
def ao():
    return an()
ap = ao()
aq = ap + '.'
ar = aq + '7'
at = ar + '8'
print(at)