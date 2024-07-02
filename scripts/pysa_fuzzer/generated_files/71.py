import random
import math
a = input()
b = ''
counterb = 0
while counterb < 5:
    c = ''
    counterc = 0
    while counterc < 3:
        d = ''
        counterd = 0
        while counterd < 4:
            d += c
            counterd += 1
            c += b
            counterc += 1
        b += a
        counterb += 1
e_set = {d, d, d, d, d, d}
e = random.choice(list(e_set))
f = e + '.'
g = [f for _ in range(9)]
random.shuffle(g)
h = random.choice(g)
i_dict = {71: h, 23: h}
j_dict = {34: i_dict, 85: i_dict, 50: i_dict, 74: i_dict, 25: i_dict, 89: i_dict, 51: i_dict, 64: i_dict}
k_dict = {44: j_dict, 55: j_dict}
l = random.choice(list(k_dict.values()))
m = random.choice(list(l.values()))
n = random.choice(list(m.values()))
if n == '6':
    o = n + ' c1'
elif n == '19':
    o = n + ' c2'
else:
    o = n + ' c3'
p = ''
for _ in range(3):
    p += o
q = (p, p, p)
r, s, t = q
u = r + s + t
v_set = {u, u, u, u, u, u, u}
v = random.choice(list(v_set))
w_dict = {20: v, 44: v, 27: v, 64: v, 52: v}
x_dict = {100: w_dict, 40: w_dict, 34: w_dict, 15: w_dict, 85: w_dict}
y_dict = {26: x_dict, 68: x_dict, 11: x_dict, 49: x_dict, 22: x_dict, 19: x_dict, 24: x_dict}
z = random.choice(list(y_dict.values()))
aa = random.choice(list(z.values()))
ab = random.choice(list(aa.values()))
ac = f'string {ab}'
ad_set = {ac, ac, ac, ac, ac, ac, ac, ac}
ad = random.choice(list(ad_set))
def ae():
    return ad
def af():
    return ae()
def ag():
    return af()
ah = ag()
ai = ah + '.'
aj_list = [ai for _ in range(9)]
ak_list = [aj_list for _ in range(9)]
al_list = [ak_list for _ in range(4)]
am = random.choice(al_list)
an = random.choice(am)
ao = random.choice(an)
if ao == '4':
    ap = ao + ' c1'
elif ao == '19':
    ap = ao + ' c2'
else:
    ap = ao + ' c3'
aq = (ap, ap, ap)
ar, at, au = aq
av = ar + at + au
def aw():
    return av
def ax():
    return aw()
def ay():
    return ax()
az = ay()
print(az)