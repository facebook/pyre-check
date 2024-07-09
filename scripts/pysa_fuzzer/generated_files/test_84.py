import random
import math

a = input()
b = a + '4'
c_dict = {52: b, 69: b, 75: b, 98: b, 36: b, 86: b, 82: b, 20: b, 74: b, 84: b}
d_dict = {62: c_dict, 84: c_dict, 52: c_dict, 84: c_dict, 68: c_dict, 10: c_dict}
e_dict = {55: d_dict, 12: d_dict, 18: d_dict, 35: d_dict, 15: d_dict, 29: d_dict, 93: d_dict, 88: d_dict, 13: d_dict}
f = random.choice(list(e_dict.values()))
g = random.choice(list(f.values()))
h = random.choice(list(g.values()))
i = ''
for _ in range(5):
    j = ''
    for _ in range(5):
        k = ''
        for _ in range(5):
            k += j
            j += i
        i += h
def l():
    return k
def m():
    return l()
def n():
    return m()
o = n()
p = ''
counterp = 0
while counterp < 4:
    p += o
    counterp += 1
q_set = {p, p, p, p, p}
q = random.choice(list(q_set))
r_list = [q for _ in range(7)]
s = random.choice(r_list)
t_list = [s for _ in range(9)]
u_list = [t_list for _ in range(7)]
v_list = [u_list for _ in range(4)]
w = random.choice(v_list)
x = random.choice(w)
y = random.choice(x)
def z():
    return y
aa = z()
ab = ''
for _ in range(8):
        if _ == 4:
            continue
        ab += aa
ac_set = {ab, ab, ab, ab, ab, ab, ab, ab, ab}
ac = random.choice(list(ac_set))
ad = [ac for _ in range(9)]
random.shuffle(ad)
ae = random.choice(ad)
af = ''
for _ in range(5):
    ag = ''
    for _ in range(4):
        ah = ''
        for _ in range(4):
            ah += ag
            ag += af
        af += ae
ai = ''
counterai = 0
while counterai < 5:
    ai += ah
    counterai += 1
aj = f'string {ai}'
if aj == aj:
    am = aj + 'c1'
elif aj == '19':
    am = ak + 'c2'
else:
    am = al + 'c3'
an = [am for _ in range(7)]
random.shuffle(an)
ao = random.choice(an)
if ao == ao:
    ar = ao + 'c1'
elif ao == '16':
    ar = ap + 'c2'
else:
    ar = aq + 'c3'
print(ar)