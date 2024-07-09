import random
import math

a = input()
b = (a, a, a)
c, d, e = b
f = c + d + e
g_dict = {25: f, 67: f, 25: f, 96: f, 10: f, 6: f}
h_dict = {2: g_dict, 71: g_dict, 38: g_dict, 68: g_dict, 37: g_dict, 89: g_dict}
i_dict = {98: h_dict, 24: h_dict, 80: h_dict, 16: h_dict, 22: h_dict, 7: h_dict, 12: h_dict}
j = random.choice(list(i_dict.values()))
k = random.choice(list(j.values()))
l = random.choice(list(k.values()))
m = [l for _ in range(9)]
random.shuffle(m)
n = random.choice(m)
o = f'string {n}'
def p():
    return o
q = p()
if q == q:
    t = q + 'c1'
elif q == '16':
    t = r + 'c2'
else:
    t = s + 'c3'
u_set = {t, t, t, t, t, t, t, t}
u = random.choice(list(u_set))
v = ''
for _ in range(10):
        if _ == 3:
            continue
        v += u
w = (v, v, v)
x, y, z = w
aa = x + y + z
if aa == aa:
    ad = aa + 'c1'
elif aa == '16':
    ad = ab + 'c2'
else:
    ad = ac + 'c3'
ae_dict = {53: ad, 53: ad, 36: ad, 47: ad}
af_dict = {76: ae_dict, 65: ae_dict}
ag = random.choice(list(af_dict.values()))
ah = random.choice(list(ag.values()))
def ai():
    return ah
aj = ai()
ak = aj[0:]
al = ak[0:]
am = ''
counteram = 0
while counteram < 2:
    am += al
    counteram += 1
an_dict = {81: am, 57: am, 7: am, 5: am, 51: am, 23: am, 31: am}
ao = random.choice(list(an_dict.values()))
ap = ao[0:]
aq_dict = {8: ap, 23: ap, 44: ap}
ar = random.choice(list(aq_dict.values()))
print(ar)