import random
import math

a = input()
b = [a for _ in range(5)]
random.shuffle(b)
c = random.choice(b)
d = ''
for _ in range(4):
    e = ''
    for _ in range(5):
        e += d
        d += c
f = f'string {e}'
g = ''
for _ in range(5):
        if _ == 4:
            continue
        g += f
h = [g for _ in range(9)]
random.shuffle(h)
i = random.choice(h)
j = ''
for _ in range(5):
    for __ in range(5):
                j += i
k = j + '6'
l = [k for _ in range(7)]
random.shuffle(l)
m = random.choice(l)
n = [m for _ in range(6)]
random.shuffle(n)
o = random.choice(n)
p_list = [o for _ in range(10)]
q_list = [p_list for _ in range(7)]
r = random.choice(q_list)
s = random.choice(r)
t = s[0:]
u = ''
counteru = 0
while counteru < 3:
    v = ''
    counterv = 0
    while counterv < 3:
        w = ''
        counterw = 0
        while counterw < 3:
            w += v
            counterw += 1
            v += u
            counterv += 1
        u += t
        counteru += 1
x_dict = {48: w, 17: w, 66: w, 76: w, 47: w, 3: w, 39: w, 26: w, 100: w}
y = random.choice(list(x_dict.values()))
z_dict = {30: y, 97: y, 45: y}
aa = random.choice(list(z_dict.values()))
ab_dict = {47: aa, 13: aa, 49: aa, 44: aa, 31: aa, 50: aa, 17: aa, 47: aa}
ac_dict = {39: ab_dict, 99: ab_dict, 24: ab_dict, 32: ab_dict, 76: ab_dict}
ad = random.choice(list(ac_dict.values()))
ae = random.choice(list(ad.values()))
def af():
    return ae
def ag():
    return af()
def ah():
    return ag()
ai = ah()
aj_set = {ai, ai, ai}
aj = random.choice(list(aj_set))
ak = (aj, aj, aj)
al, am, an = ak
ao = al + am + an
print(ao)