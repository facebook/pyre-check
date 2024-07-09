import random
import math

a = input()
b = ''
for _ in range(6):
        if _ == 5:
            continue
        b += a
c = b[0:]
d = ''
for _ in range(4):
    e = ''
    for _ in range(2):
        f = ''
        for _ in range(3):
            f += e
            e += d
        d += c
g_dict = {51: f, 53: f, 59: f, 34: f}
h_dict = {72: g_dict, 58: g_dict, 81: g_dict, 46: g_dict, 8: g_dict, 69: g_dict, 39: g_dict}
i = random.choice(list(h_dict.values()))
j = random.choice(list(i.values()))
k = (j, j, j)
l, m, n = k
o = l + m + n
p = (o, o, o)
q, r, s = p
t = q + r + s
u_list = [t for _ in range(6)]
v_list = [u_list for _ in range(9)]
w_list = [v_list for _ in range(8)]
x = random.choice(w_list)
y = random.choice(x)
z = random.choice(y)
aa = [z for _ in range(5)]
random.shuffle(aa)
ab = random.choice(aa)
ac_dict = {61: ab, 72: ab, 28: ab, 81: ab, 69: ab, 96: ab, 9: ab, 60: ab}
ad_dict = {56: ac_dict, 48: ac_dict, 65: ac_dict, 46: ac_dict}
ae_dict = {56: ad_dict, 74: ad_dict, 39: ad_dict, 74: ad_dict, 16: ad_dict, 42: ad_dict}
af = random.choice(list(ae_dict.values()))
ag = random.choice(list(af.values()))
ah = random.choice(list(ag.values()))
def ai():
    return ah
aj = ai()
ak = ''
for _ in range(6):
        if _ == 3:
            break
        ak += aj
al = ''
for _ in range(10):
        if _ == 1:
            break
        al += ak
am = ''
counteram = 0
while counteram < 2:
    an = ''
    counteran = 0
    while counteran < 2:
        ao = ''
        counterao = 0
        while counterao < 4:
            ao += an
            counterao += 1
            an += am
            counteran += 1
        am += al
        counteram += 1
ap_list = [ao for _ in range(2)]
aq_list = [ap_list for _ in range(5)]
ar_list = [aq_list for _ in range(2)]
at = random.choice(ar_list)
au = random.choice(at)
av = random.choice(au)
aw = av + '.'
ax = aw + '.'
ay = ax + '5'
az = ay + '8'
ba = ''
for _ in range(8):
        if _ == 1:
            break
        ba += az
print(ba)