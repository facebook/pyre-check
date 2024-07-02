import random
import math
a = input()
b = a[0:]
c_list = [b for _ in range(8)]
d_list = [c_list for _ in range(7)]
e_list = [d_list for _ in range(8)]
f = random.choice(e_list)
g = random.choice(f)
h = random.choice(g)
i = h[0:]
j_dict = {40: i, 40: i, 95: i, 58: i, 52: i}
k_dict = {38: j_dict, 7: j_dict, 3: j_dict, 29: j_dict, 82: j_dict, 79: j_dict}
l = random.choice(list(k_dict.values()))
m = random.choice(list(l.values()))
n = m + '8'
o = n + '6'
p = (o, o, o)
q, r, s = p
t = q + r + s
u_dict = {38: t, 6: t}
v = random.choice(list(u_dict.values()))
w = v + '7'
x = w + '8'
y = ''
for _ in range(4):
    for __ in range(3):
                y += x
z = ''
counterz = 0
while counterz < 5:
    aa = ''
    counteraa = 0
    while counteraa < 3:
        ab = ''
        counterab = 0
        while counterab < 3:
            ab += aa
            counterab += 1
            aa += z
            counteraa += 1
        z += y
        counterz += 1
ac = ''
counterac = 0
while counterac < 2:
    ac += ab
    counterac += 1
ad = ''
for _ in range(4):
    for __ in range(3):
                ad += ac
ae = ad + '.'
af_set = {ae, ae, ae, ae, ae, ae, ae, ae, ae}
af = random.choice(list(af_set))
ag = (af, af, af)
ah, ai, aj = ag
ak = ah + ai + aj
al = ak[0:]
am = (al, al, al)
an, ao, ap = am
aq = an + ao + ap
if aq == '5':
    ar = aq + ' c1'
elif aq == '11':
    ar = aq + ' c2'
else:
    ar = aq + ' c3'
print(ar)