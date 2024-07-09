import random
import math

a = input()
b_dict = {5: a, 13: a, 28: a, 56: a, 28: a}
c_dict = {83: b_dict, 44: b_dict, 44: b_dict}
d_dict = {26: c_dict, 42: c_dict, 30: c_dict, 32: c_dict, 54: c_dict, 18: c_dict}
e = random.choice(list(d_dict.values()))
f = random.choice(list(e.values()))
g = random.choice(list(f.values()))
h = ''
counterh = 0
while counterh < 4:
    h += g
    counterh += 1
i = ''
for _ in range(5):
    for __ in range(4):
                i += h
j_dict = {79: i, 83: i, 19: i, 79: i, 41: i, 57: i, 55: i}
k = random.choice(list(j_dict.values()))
l_list = [k for _ in range(3)]
m = random.choice(l_list)
n_list = [m for _ in range(5)]
o = random.choice(n_list)
def p():
    return o
def q():
    return p()
r = q()
s_dict = {94: r, 76: r, 7: r, 73: r, 64: r, 99: r, 73: r, 17: r, 49: r}
t_dict = {77: s_dict, 85: s_dict, 80: s_dict, 85: s_dict, 69: s_dict, 82: s_dict, 4: s_dict, 11: s_dict, 9: s_dict}
u_dict = {78: t_dict, 81: t_dict, 53: t_dict, 48: t_dict, 55: t_dict}
v = random.choice(list(u_dict.values()))
w = random.choice(list(v.values()))
x = random.choice(list(w.values()))
y = x[0:]
z = y + '.'
aa = ''
counteraa = 0
while counteraa < 2:
    ab = ''
    counterab = 0
    while counterab < 5:
        ac = ''
        counterac = 0
        while counterac < 5:
            ac += ab
            counterac += 1
            ab += aa
            counterab += 1
        aa += z
        counteraa += 1
ad = ''
for _ in range(5):
    ad += ac
ae = (ad, ad, ad)
af, ag, ah = ae
ai = af + ag + ah
def aj():
    return ai
def ak():
    return aj()
al = ak()
am = (al, al, al)
an, ao, ap = am
aq = an + ao + ap
ar_list = [aq for _ in range(8)]
at_list = [ar_list for _ in range(3)]
au = random.choice(at_list)
av = random.choice(au)
aw = av + '.'
ax = aw + '2'
print(ax)