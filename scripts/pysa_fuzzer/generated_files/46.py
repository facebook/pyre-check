import random
import math
a = input()
b = (a, a, a)
c, d, e = b
f = c + d + e
g = f + '1'
h = g + '9'
i = h + '2'
j_dict = {92: i, 70: i, 10: i, 79: i, 33: i, 26: i}
k_dict = {43: j_dict, 50: j_dict, 9: j_dict, 97: j_dict}
l = random.choice(list(k_dict.values()))
m = random.choice(list(l.values()))
n = m + '9'
o = ''
countero = 0
while countero < 3:
    p = ''
    counterp = 0
    while counterp < 4:
        p += o
        counterp += 1
        o += n
        countero += 1
q = ''
for _ in range(4):
    q += p
r_list = [q for _ in range(4)]
s_list = [r_list for _ in range(8)]
t_list = [s_list for _ in range(3)]
u = random.choice(t_list)
v = random.choice(u)
w = random.choice(v)
x = w + '.'
def y():
    return x
z = y()
def aa():
    return z
ab = aa()
ac = ab[0:]
if ac == '1':
    ad = ac + ' c1'
elif ac == '19':
    ad = ac + ' c2'
else:
    ad = ac + ' c3'
ae = ad + '.'
af = [ae for _ in range(10)]
random.shuffle(af)
ag = random.choice(af)
ah = ''
for _ in range(2):
    ai = ''
    for _ in range(2):
        ai += ah
        ah += ag
aj = [ai for _ in range(9)]
random.shuffle(aj)
ak = random.choice(aj)
al = ak + '8'
am = al + '1'
an = am + '9'
ao_list = [an for _ in range(7)]
ap_list = [ao_list for _ in range(6)]
aq = random.choice(ap_list)
ar = random.choice(aq)
print(ar)