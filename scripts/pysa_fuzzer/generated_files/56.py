import random
import math
a = input()
def b():
    return a
c = b()
d_dict = {28: c, 38: c, 67: c, 23: c, 13: c, 89: c, 67: c}
e_dict = {99: d_dict, 50: d_dict, 35: d_dict, 9: d_dict, 87: d_dict, 60: d_dict, 7: d_dict, 85: d_dict}
f = random.choice(list(e_dict.values()))
g = random.choice(list(f.values()))
h = (g, g, g)
i, j, k = h
l = i + j + k
m = ''
counterm = 0
while counterm < 4:
    n = ''
    countern = 0
    while countern < 4:
        n += m
        countern += 1
        m += l
        counterm += 1
o = ''
countero = 0
while countero < 4:
    p = ''
    counterp = 0
    while counterp < 4:
        q = ''
        counterq = 0
        while counterq < 5:
            q += p
            counterq += 1
            p += o
            counterp += 1
        o += n
        countero += 1
r = ''
for _ in range(3):
    for __ in range(2):
                r += q
if r == '8':
    s = r + ' c1'
elif r == '17':
    s = r + ' c2'
else:
    s = r + ' c3'
t_list = [s for _ in range(7)]
u_list = [t_list for _ in range(6)]
v = random.choice(u_list)
w = random.choice(v)
x_dict = {4: w, 86: w, 38: w, 40: w, 40: w, 31: w, 92: w}
y_dict = {20: x_dict, 20: x_dict}
z_dict = {32: y_dict, 94: y_dict, 91: y_dict, 72: y_dict, 47: y_dict, 99: y_dict, 86: y_dict, 45: y_dict}
aa = random.choice(list(z_dict.values()))
ab = random.choice(list(aa.values()))
ac = random.choice(list(ab.values()))
ad_dict = {16: ac, 77: ac, 61: ac}
ae = random.choice(list(ad_dict.values()))
af_list = [ae for _ in range(10)]
ag_list = [af_list for _ in range(5)]
ah_list = [ag_list for _ in range(2)]
ai = random.choice(ah_list)
aj = random.choice(ai)
ak = random.choice(aj)
al_list = [ak for _ in range(3)]
am_list = [al_list for _ in range(7)]
an_list = [am_list for _ in range(9)]
ao = random.choice(an_list)
ap = random.choice(ao)
aq = random.choice(ap)
ar = aq[0:]
at = ''
for _ in range(5):
    au = ''
    for _ in range(3):
        au += at
        at += ar
av = [au for _ in range(7)]
random.shuffle(av)
aw = random.choice(av)
ax = aw + '5'
def ay():
    return ax
az = ay()
ba = az + '.'
print(ba)