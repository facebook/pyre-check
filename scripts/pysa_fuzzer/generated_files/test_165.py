import random
import math

a = input()
b = ''
for _ in range(5):
    c = ''
    for _ in range(2):
        c += b
        b += a
d_set = {c, c, c, c, c, c, c, c, c}
d = random.choice(list(d_set))
e = ''
for _ in range(5):
        if _ == 3:
            continue
        e += d
f_set = {e, e, e, e, e, e}
f = random.choice(list(f_set))
g = ''
counterg = 0
while counterg < 2:
    h = ''
    counterh = 0
    while counterh < 2:
        h += g
        counterh += 1
        g += f
        counterg += 1
i = [h for _ in range(9)]
random.shuffle(i)
j = random.choice(i)
k = ''
counterk = 0
while counterk < 4:
    l = ''
    counterl = 0
    while counterl < 3:
        l += k
        counterl += 1
        k += j
        counterk += 1
m = ''
counterm = 0
while counterm < 5:
    n = ''
    countern = 0
    while countern < 4:
        n += m
        countern += 1
        m += l
        counterm += 1
o_list = [n for _ in range(6)]
p_list = [o_list for _ in range(5)]
q_list = [p_list for _ in range(9)]
r = random.choice(q_list)
s = random.choice(r)
t = random.choice(s)
u = (t, t, t)
v, w, x = u
y = v + w + x
z_dict = {100: y, 30: y, 14: y, 47: y, 27: y, 63: y}
aa_dict = {65: z_dict, 26: z_dict, 1: z_dict, 58: z_dict, 5: z_dict, 4: z_dict, 66: z_dict, 10: z_dict}
ab = random.choice(list(aa_dict.values()))
ac = random.choice(list(ab.values()))
if ac == ac:
    af = ac + 'c1'
elif ac == '13':
    af = ad + 'c2'
else:
    af = ae + 'c3'
ag = f'string {af}'
ah = ag + '.'
ai = f'string {ah}'
aj = ''
counteraj = 0
while counteraj < 4:
    aj += ai
    counteraj += 1
ak_set = {aj, aj, aj, aj, aj, aj, aj, aj, aj, aj}
ak = random.choice(list(ak_set))
al_list = [ak for _ in range(3)]
am_list = [al_list for _ in range(10)]
an_list = [am_list for _ in range(8)]
ao = random.choice(an_list)
ap = random.choice(ao)
aq = random.choice(ap)
print(aq)