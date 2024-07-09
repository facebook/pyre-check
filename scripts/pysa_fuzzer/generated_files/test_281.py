import random
import math

a = input()
b = ''
for _ in range(5):
    c = ''
    for _ in range(2):
        d = ''
        for _ in range(5):
            d += c
            c += b
        b += a
e = (d, d, d)
f, g, h = e
i = f + g + h
def j():
    return i
def k():
    return j()
def l():
    return k()
m = l()
n = (m, m, m)
o, p, q = n
r = o + p + q
s = f'string {r}'
t = s + '.'
u = t + '6'
v = u[0:]
w_list = [v for _ in range(4)]
x_list = [w_list for _ in range(10)]
y = random.choice(x_list)
z = random.choice(y)
aa_dict = {75: z, 100: z, 51: z, 9: z, 48: z, 18: z, 33: z, 63: z, 20: z, 82: z}
ab_dict = {42: aa_dict, 9: aa_dict, 91: aa_dict, 57: aa_dict, 84: aa_dict, 45: aa_dict, 75: aa_dict, 75: aa_dict}
ac_dict = {75: ab_dict, 22: ab_dict, 37: ab_dict, 63: ab_dict, 1: ab_dict, 28: ab_dict, 66: ab_dict}
ad = random.choice(list(ac_dict.values()))
ae = random.choice(list(ad.values()))
af = random.choice(list(ae.values()))
ag_set = {af, af, af, af}
ag = random.choice(list(ag_set))
ah_dict = {25: ag, 81: ag, 79: ag, 6: ag, 13: ag, 2: ag, 79: ag, 50: ag, 47: ag, 65: ag}
ai_dict = {91: ah_dict, 89: ah_dict, 26: ah_dict, 32: ah_dict, 33: ah_dict}
aj_dict = {40: ai_dict, 39: ai_dict, 37: ai_dict, 94: ai_dict, 76: ai_dict, 52: ai_dict, 93: ai_dict}
ak = random.choice(list(aj_dict.values()))
al = random.choice(list(ak.values()))
am = random.choice(list(al.values()))
an = (am, am, am)
ao, ap, aq = an
ar = ao + ap + aq
at_set = {ar, ar, ar, ar}
at = random.choice(list(at_set))
au = ''
counterau = 0
while counterau < 4:
    av = ''
    counterav = 0
    while counterav < 5:
        aw = ''
        counteraw = 0
        while counteraw < 4:
            aw += av
            counteraw += 1
            av += au
            counterav += 1
        au += at
        counterau += 1
ax = ''
counterax = 0
while counterax < 3:
    ax += aw
    counterax += 1
ay = (ax, ax, ax)
az, ba, bb = ay
bc = az + ba + bb
bd = [bc for _ in range(9)]
random.shuffle(bd)
be = random.choice(bd)
print(be)