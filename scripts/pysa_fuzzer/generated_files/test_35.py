import random
import math

a = input()
b_dict = {91: a, 37: a, 3: a, 69: a}
c_dict = {13: b_dict, 31: b_dict, 89: b_dict, 43: b_dict, 57: b_dict, 79: b_dict, 3: b_dict, 30: b_dict}
d_dict = {3: c_dict, 41: c_dict, 89: c_dict, 78: c_dict, 45: c_dict, 93: c_dict, 81: c_dict}
e = random.choice(list(d_dict.values()))
f = random.choice(list(e.values()))
g = random.choice(list(f.values()))
h = g[0:]
i_set = {h, h, h, h, h, h, h}
i = random.choice(list(i_set))
if i == i:
    l = i + 'c1'
elif i == '19':
    l = j + 'c2'
else:
    l = k + 'c3'
m = l[0:]
n = ''
countern = 0
while countern < 2:
    o = ''
    countero = 0
    while countero < 3:
        p = ''
        counterp = 0
        while counterp < 3:
            p += o
            counterp += 1
            o += n
            countero += 1
        n += m
        countern += 1
q_set = {p, p, p, p, p, p, p, p, p, p}
q = random.choice(list(q_set))
r_list = [q for _ in range(3)]
s_list = [r_list for _ in range(10)]
t = random.choice(s_list)
u = random.choice(t)
v = (u, u, u)
w, x, y = v
z = w + x + y
aa = z + '1'
ab = [aa for _ in range(6)]
random.shuffle(ab)
ac = random.choice(ab)
ad_dict = {7: ac, 59: ac, 24: ac, 68: ac, 59: ac}
ae_dict = {36: ad_dict, 13: ad_dict, 66: ad_dict, 13: ad_dict, 65: ad_dict, 12: ad_dict, 87: ad_dict, 33: ad_dict, 63: ad_dict, 82: ad_dict}
af_dict = {37: ae_dict, 65: ae_dict, 44: ae_dict, 87: ae_dict, 98: ae_dict, 99: ae_dict, 22: ae_dict, 4: ae_dict}
ag = random.choice(list(af_dict.values()))
ah = random.choice(list(ag.values()))
ai = random.choice(list(ah.values()))
aj = (ai, ai, ai)
ak, al, am = aj
an = ak + al + am
ao = (an, an, an)
ap, aq, ar = ao
at = ap + aq + ar
au = ''
for _ in range(8):
        if _ == 4:
            break
        au += at
av = au + '.'
aw = ''
counteraw = 0
while counteraw < 4:
    ax = ''
    counterax = 0
    while counterax < 2:
        ay = ''
        counteray = 0
        while counteray < 3:
            ay += ax
            counteray += 1
            ax += aw
            counterax += 1
        aw += av
        counteraw += 1
az = ''
for _ in range(8):
        if _ == 1:
            break
        az += ay
print(az)