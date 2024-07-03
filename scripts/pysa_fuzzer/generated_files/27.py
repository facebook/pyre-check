import random
import math
a = input()
b_list = [a for _ in range(7)]
c_list = [b_list for _ in range(10)]
d_list = [c_list for _ in range(7)]
e = random.choice(d_list)
f = random.choice(e)
g = random.choice(f)
h = (g, g, g)
i, j, k = h
l = i + j + k
m = ''
for _ in range(5):
        if _ == 3:
            continue
        m += l
n = m + '4'
o = n[0:]
p_set = {o, o, o, o, o}
p = random.choice(list(p_set))
q = ''
for _ in range(4):
    for __ in range(2):
                q += p
r = [q for _ in range(10)]
random.shuffle(r)
s = random.choice(r)
t_list = [s for _ in range(4)]
u = random.choice(t_list)
v = ''
counterv = 0
while counterv < 2:
    w = ''
    counterw = 0
    while counterw < 2:
        x = ''
        counterx = 0
        while counterx < 3:
            x += w
            counterx += 1
            w += v
            counterw += 1
        v += u
        counterv += 1
y_dict = {9: x, 57: x}
z = random.choice(list(y_dict.values()))
def aa():
    return z
def ab():
    return aa()
def ac():
    return ab()
ad = ac()
ae_dict = {28: ad, 13: ad, 87: ad, 81: ad, 88: ad, 4: ad, 78: ad, 7: ad, 60: ad}
af_dict = {67: ae_dict, 33: ae_dict, 78: ae_dict}
ag_dict = {95: af_dict, 22: af_dict}
ah = random.choice(list(ag_dict.values()))
ai = random.choice(list(ah.values()))
aj = random.choice(list(ai.values()))
ak = [aj for _ in range(5)]
random.shuffle(ak)
al = random.choice(ak)
am = ''
for _ in range(5):
    for __ in range(4):
                am += al
an = am + '.'
ao = an + '.'
ap = f'string {ao}'
print(ap)