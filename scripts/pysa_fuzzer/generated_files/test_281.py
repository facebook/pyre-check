import random
import math

a = input()
b = a + '3'
c = b + '2'
d = c + '9'
e = ''
for _ in range(3):
    f = ''
    for _ in range(3):
        g = ''
        for _ in range(3):
            g += f
            f += e
        e += d
h = ''
for _ in range(4):
    for __ in range(4):
                h += g
i_dict = {92: h, 57: h}
j_dict = {91: i_dict, 12: i_dict, 25: i_dict, 54: i_dict, 33: i_dict, 28: i_dict, 72: i_dict, 44: i_dict, 30: i_dict, 97: i_dict}
k = random.choice(list(j_dict.values()))
l = random.choice(list(k.values()))
m_dict = {60: l, 80: l, 15: l, 19: l, 76: l, 35: l, 99: l, 32: l}
n_dict = {6: m_dict, 23: m_dict, 92: m_dict, 9: m_dict, 78: m_dict, 53: m_dict, 16: m_dict, 21: m_dict, 90: m_dict, 52: m_dict}
o_dict = {18: n_dict, 18: n_dict, 52: n_dict}
p = random.choice(list(o_dict.values()))
q = random.choice(list(p.values()))
r = random.choice(list(q.values()))
s = r + '8'
t = s + '6'
u = t + '1'
v = ''
counterv = 0
while counterv < 5:
    w = ''
    counterw = 0
    while counterw < 4:
        x = ''
        counterx = 0
        while counterx < 3:
            x += w
            counterx += 1
            w += v
            counterw += 1
        v += u
        counterv += 1
if x == x:
    aa = x + 'c1'
elif x == '20':
    aa = y + 'c2'
else:
    aa = z + 'c3'
ab = ''
counterab = 0
while counterab < 2:
    ac = ''
    counterac = 0
    while counterac < 4:
        ac += ab
        counterac += 1
        ab += aa
        counterab += 1
ad = ac + '.'
if ad == ad:
    ag = ad + 'c1'
elif ad == '16':
    ag = ae + 'c2'
else:
    ag = af + 'c3'
ah_dict = {69: ag, 85: ag, 55: ag}
ai_dict = {94: ah_dict, 68: ah_dict, 24: ah_dict, 14: ah_dict, 10: ah_dict, 25: ah_dict}
aj_dict = {53: ai_dict, 59: ai_dict, 88: ai_dict}
ak = random.choice(list(aj_dict.values()))
al = random.choice(list(ak.values()))
am = random.choice(list(al.values()))
an = ''
for _ in range(7):
        if _ == 5:
            break
        an += am
ao_list = [an for _ in range(3)]
ap = random.choice(ao_list)
aq = (ap, ap, ap)
ar, at, au = aq
av = ar + at + au
aw_list = [av for _ in range(7)]
ax_list = [aw_list for _ in range(6)]
ay = random.choice(ax_list)
az = random.choice(ay)
ba = az[0:]
bb_dict = {95: ba, 82: ba, 22: ba, 35: ba}
bc_dict = {46: bb_dict, 65: bb_dict, 88: bb_dict, 36: bb_dict, 31: bb_dict, 42: bb_dict}
bd = random.choice(list(bc_dict.values()))
be = random.choice(list(bd.values()))
print(be)