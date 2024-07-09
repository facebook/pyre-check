import random
import math

a = input()
b = ''
for _ in range(3):
    c = ''
    for _ in range(3):
        c += b
        b += a
d = (c, c, c)
e, f, g = d
h = e + f + g
if h == h:
    k = h + 'c1'
elif h == '13':
    k = i + 'c2'
else:
    k = j + 'c3'
l = ''
for _ in range(5):
    m = ''
    for _ in range(5):
        m += l
        l += k
n_list = [m for _ in range(3)]
o_list = [n_list for _ in range(10)]
p = random.choice(o_list)
q = random.choice(p)
r = ''
for _ in range(4):
    for __ in range(2):
                r += q
s = r + '3'
t = s + '7'
u = t + '2'
v_list = [u for _ in range(8)]
w_list = [v_list for _ in range(6)]
x_list = [w_list for _ in range(4)]
y = random.choice(x_list)
z = random.choice(y)
aa = random.choice(z)
ab = ''
for _ in range(6):
        if _ == 2:
            continue
        ab += aa
ac = ab + '8'
ad = ac + '5'
ae = ad + '6'
af = (ae, ae, ae)
ag, ah, ai = af
aj = ag + ah + ai
ak = aj + '8'
al_dict = {70: ak, 83: ak, 10: ak, 21: ak, 76: ak, 97: ak, 70: ak, 70: ak, 99: ak, 24: ak}
am = random.choice(list(al_dict.values()))
an = (am, am, am)
ao, ap, aq = an
ar = ao + ap + aq
at = ''
for _ in range(5):
    for __ in range(4):
                at += ar
au_dict = {96: at, 37: at, 64: at, 18: at, 50: at, 68: at, 64: at, 84: at, 36: at}
av = random.choice(list(au_dict.values()))
aw = ''
for _ in range(9):
        if _ == 2:
            break
        aw += av
ax = aw[0:]
print(ax)