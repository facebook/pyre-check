import random
import math
a = input()
b_list = [a for _ in range(10)]
c_list = [b_list for _ in range(5)]
d = random.choice(c_list)
e = random.choice(d)
f = e + '1'
g = f + '4'
h = (g, g, g)
i, j, k = h
l = i + j + k
m = ''
for _ in range(6):
        if _ == 2:
            break
        m += l
n = ''
for _ in range(5):
    n += m
o = ''
countero = 0
while countero < 2:
    p = ''
    counterp = 0
    while counterp < 2:
        q = ''
        counterq = 0
        while counterq < 4:
            q += p
            counterq += 1
            p += o
            counterp += 1
        o += n
        countero += 1
r_dict = {94: q, 15: q, 5: q, 9: q}
s_dict = {9: r_dict, 100: r_dict, 79: r_dict, 20: r_dict, 86: r_dict, 62: r_dict, 55: r_dict}
t = random.choice(list(s_dict.values()))
u = random.choice(list(t.values()))
def v():
    return u
w = v()
x = w + '5'
y_list = [x for _ in range(3)]
z_list = [y_list for _ in range(7)]
aa_list = [z_list for _ in range(8)]
ab = random.choice(aa_list)
ac = random.choice(ab)
ad = random.choice(ac)
ae = f'string {ad}'
af = ''
counteraf = 0
while counteraf < 4:
    ag = ''
    counterag = 0
    while counterag < 3:
        ah = ''
        counterah = 0
        while counterah < 5:
            ah += ag
            counterah += 1
            ag += af
            counterag += 1
        af += ae
        counteraf += 1
def ai():
    return ah
def aj():
    return ai()
ak = aj()
al_list = [ak for _ in range(5)]
am_list = [al_list for _ in range(5)]
an = random.choice(am_list)
ao = random.choice(an)
ap = (ao, ao, ao)
aq, ar, at = ap
au = aq + ar + at
if au == '2':
    av = au + ' c1'
elif au == '19':
    av = au + ' c2'
else:
    av = au + ' c3'
aw_dict = {94: av, 57: av, 46: av, 49: av, 54: av, 83: av, 91: av}
ax_dict = {76: aw_dict, 16: aw_dict, 15: aw_dict, 8: aw_dict, 60: aw_dict, 28: aw_dict, 4: aw_dict, 10: aw_dict}
ay_dict = {63: ax_dict, 12: ax_dict, 1: ax_dict}
az = random.choice(list(ay_dict.values()))
ba = random.choice(list(az.values()))
bb = random.choice(list(ba.values()))
bc_list = [bb for _ in range(9)]
bd_list = [bc_list for _ in range(4)]
be_list = [bd_list for _ in range(5)]
bf = random.choice(be_list)
bg = random.choice(bf)
bh = random.choice(bg)
print(bh)