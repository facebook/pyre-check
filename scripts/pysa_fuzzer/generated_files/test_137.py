import random
import math

a = input()
b = a + '2'
c = b + '5'
d = c + '1'
e = d + '4'
f = e + '3'
g = f + '7'
h_dict = {79: g, 17: g, 40: g, 22: g, 98: g, 48: g, 51: g, 14: g, 71: g}
i_dict = {73: h_dict, 11: h_dict, 95: h_dict, 49: h_dict, 63: h_dict, 83: h_dict, 89: h_dict, 58: h_dict, 81: h_dict}
j_dict = {44: i_dict, 71: i_dict, 76: i_dict, 51: i_dict, 12: i_dict, 25: i_dict, 54: i_dict, 55: i_dict, 5: i_dict}
k = random.choice(list(j_dict.values()))
l = random.choice(list(k.values()))
m = random.choice(list(l.values()))
n = ''
for _ in range(8):
        if _ == 5:
            break
        n += m
o = n + '.'
p = f'string {o}'
q_dict = {49: p, 76: p, 33: p, 54: p, 27: p, 43: p, 17: p}
r_dict = {21: q_dict, 3: q_dict, 77: q_dict, 89: q_dict, 98: q_dict}
s = random.choice(list(r_dict.values()))
t = random.choice(list(s.values()))
if t == t:
    w = t + 'c1'
elif t == '19':
    w = u + 'c2'
else:
    w = v + 'c3'
x = (w, w, w)
y, z, aa = x
ab = y + z + aa
ac = (ab, ab, ab)
ad, ae, af = ac
ag = ad + ae + af
ah = ''
for _ in range(5):
    for __ in range(3):
                ah += ag
if ah == ah:
    ak = ah + 'c1'
elif ah == '16':
    ak = ai + 'c2'
else:
    ak = aj + 'c3'
al = (ak, ak, ak)
am, an, ao = al
ap = am + an + ao
aq = ''
counteraq = 0
while counteraq < 5:
    ar = ''
    counterar = 0
    while counterar < 5:
        at = ''
        counterat = 0
        while counterat < 5:
            at += ar
            counterat += 1
            ar += aq
            counterar += 1
        aq += ap
        counteraq += 1
au = ''
for _ in range(10):
        if _ == 3:
            continue
        au += at
av = ''
counterav = 0
while counterav < 4:
    aw = ''
    counteraw = 0
    while counteraw < 5:
        ax = ''
        counterax = 0
        while counterax < 5:
            ax += aw
            counterax += 1
            aw += av
            counteraw += 1
        av += au
        counterav += 1
ay_dict = {90: ax, 17: ax}
az = random.choice(list(ay_dict.values()))
ba_set = {az, az, az, az, az, az, az, az}
ba = random.choice(list(ba_set))
print(ba)