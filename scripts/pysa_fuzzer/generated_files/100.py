import random
import math
a = input()
b = ''
for _ in range(3):
    c = ''
    for _ in range(2):
        c += b
        b += a
if c == '8':
    d = c + ' c1'
elif c == '19':
    d = c + ' c2'
else:
    d = c + ' c3'
e_list = [d for _ in range(6)]
f_list = [e_list for _ in range(6)]
g_list = [f_list for _ in range(8)]
h = random.choice(g_list)
i = random.choice(h)
j = random.choice(i)
k_dict = {20: j, 80: j, 75: j}
l_dict = {31: k_dict, 57: k_dict, 63: k_dict, 95: k_dict, 74: k_dict, 97: k_dict}
m_dict = {11: l_dict, 79: l_dict}
n = random.choice(list(m_dict.values()))
o = random.choice(list(n.values()))
p = random.choice(list(o.values()))
q = p + '.'
r = (q, q, q)
s, t, u = r
v = s + t + u
w = ''
for _ in range(7):
        if _ == 2:
            break
        w += v
x_dict = {96: w, 1: w, 24: w, 30: w, 96: w, 35: w, 35: w, 92: w, 31: w, 60: w}
y_dict = {44: x_dict, 57: x_dict, 38: x_dict, 17: x_dict, 69: x_dict, 87: x_dict, 82: x_dict}
z = random.choice(list(y_dict.values()))
aa = random.choice(list(z.values()))
ab = aa + '2'
ac = ab + '7'
ad = ac + '2'
def ae():
    return ad
def af():
    return ae()
def ag():
    return af()
ah = ag()
ai_dict = {61: ah, 82: ah, 78: ah, 90: ah, 100: ah, 26: ah, 42: ah, 86: ah}
aj = random.choice(list(ai_dict.values()))
ak_list = [aj for _ in range(7)]
al_list = [ak_list for _ in range(5)]
am = random.choice(al_list)
an = random.choice(am)
ao = (an, an, an)
ap, aq, ar = ao
at = ap + aq + ar
au = [at for _ in range(5)]
random.shuffle(au)
av = random.choice(au)
aw_dict = {53: av, 74: av, 12: av, 45: av, 61: av, 57: av, 53: av}
ax_dict = {92: aw_dict, 21: aw_dict, 2: aw_dict, 8: aw_dict, 14: aw_dict, 37: aw_dict, 7: aw_dict}
ay_dict = {30: ax_dict, 19: ax_dict}
az = random.choice(list(ay_dict.values()))
ba = random.choice(list(az.values()))
bb = random.choice(list(ba.values()))
bc = ''
for _ in range(2):
    for __ in range(5):
                bc += bb
bd = ''
for _ in range(4):
    be = ''
    for _ in range(5):
        bf = ''
        for _ in range(2):
            bf += be
            be += bd
        bd += bc
bg_dict = {19: bf, 25: bf, 56: bf, 67: bf, 80: bf, 51: bf, 4: bf}
bh_dict = {30: bg_dict, 52: bg_dict, 22: bg_dict, 64: bg_dict}
bi_dict = {24: bh_dict, 10: bh_dict, 7: bh_dict, 59: bh_dict}
bj = random.choice(list(bi_dict.values()))
bk = random.choice(list(bj.values()))
bl = random.choice(list(bk.values()))
print(bl)