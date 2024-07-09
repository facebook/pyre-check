import random
import math

a = input()
b = ''
for _ in range(4):
    for __ in range(5):
                b += a
c_list = [b for _ in range(2)]
d_list = [c_list for _ in range(8)]
e = random.choice(d_list)
f = random.choice(e)
def g():
    return f
h = g()
i = h[0:]
def j():
    return i
def k():
    return j()
def l():
    return k()
m = l()
n = ''
for _ in range(4):
    for __ in range(3):
                n += m
o = n + '1'
p = o + '5'
q_list = [p for _ in range(4)]
r_list = [q_list for _ in range(3)]
s = random.choice(r_list)
t = random.choice(s)
u_list = [t for _ in range(4)]
v_list = [u_list for _ in range(6)]
w_list = [v_list for _ in range(9)]
x = random.choice(w_list)
y = random.choice(x)
z = random.choice(y)
aa = ''
for _ in range(4):
    for __ in range(5):
                aa += z
ab_dict = {51: aa, 9: aa, 4: aa, 55: aa}
ac = random.choice(list(ab_dict.values()))
ad = f'string {ac}'
ae = ''
for _ in range(3):
    for __ in range(5):
                ae += ad
af = f'string {ae}'
ag_list = [af for _ in range(8)]
ah_list = [ag_list for _ in range(9)]
ai_list = [ah_list for _ in range(5)]
aj = random.choice(ai_list)
ak = random.choice(aj)
al = random.choice(ak)
am_list = [al for _ in range(4)]
an_list = [am_list for _ in range(9)]
ao_list = [an_list for _ in range(7)]
ap = random.choice(ao_list)
aq = random.choice(ap)
ar = random.choice(aq)
at = ar + '4'
au = at + '8'
av = au[0:]
print(av)