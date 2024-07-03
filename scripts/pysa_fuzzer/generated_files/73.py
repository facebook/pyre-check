import random
import math
a = input()
b = (a, a, a)
c, d, e = b
f = c + d + e
g_list = [f for _ in range(10)]
h = random.choice(g_list)
i_dict = {56: h, 45: h, 10: h}
j_dict = {36: i_dict, 4: i_dict, 89: i_dict, 3: i_dict, 62: i_dict, 31: i_dict, 4: i_dict, 92: i_dict, 3: i_dict, 92: i_dict}
k_dict = {5: j_dict, 40: j_dict, 8: j_dict, 84: j_dict, 2: j_dict, 39: j_dict}
l = random.choice(list(k_dict.values()))
m = random.choice(list(l.values()))
n = random.choice(list(m.values()))
o = [n for _ in range(8)]
random.shuffle(o)
p = random.choice(o)
q = (p, p, p)
r, s, t = q
u = r + s + t
v = u + '3'
w = v + '6'
x = w + '3'
y_list = [x for _ in range(9)]
z = random.choice(y_list)
aa = z + '9'
ab = aa + '5'
ac = ab + '6'
ad = ''
for _ in range(5):
    for __ in range(3):
                ad += ac
ae = [ad for _ in range(5)]
random.shuffle(ae)
af = random.choice(ae)
ag_set = {af, af}
ag = random.choice(list(ag_set))
ah = ag[0:]
ai_set = {ah, ah, ah, ah, ah, ah, ah, ah, ah, ah}
ai = random.choice(list(ai_set))
aj = [ai for _ in range(10)]
random.shuffle(aj)
ak = random.choice(aj)
al = ''
for _ in range(4):
    am = ''
    for _ in range(2):
        an = ''
        for _ in range(4):
            an += am
            am += al
        al += ak
ao = (an, an, an)
ap, aq, ar = ao
at = ap + aq + ar
if at == '8':
    au = at + ' c1'
elif at == '18':
    au = at + ' c2'
else:
    au = at + ' c3'
av_dict = {4: au, 26: au, 11: au, 64: au, 58: au, 23: au, 17: au, 36: au}
aw_dict = {68: av_dict, 32: av_dict, 47: av_dict}
ax = random.choice(list(aw_dict.values()))
ay = random.choice(list(ax.values()))
print(ay)