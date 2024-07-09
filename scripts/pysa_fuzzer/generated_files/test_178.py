import random
import math

a = input()
b = f'string {a}'
c = ''
for _ in range(9):
        if _ == 4:
            continue
        c += b
d_list = [c for _ in range(8)]
e_list = [d_list for _ in range(4)]
f_list = [e_list for _ in range(8)]
g = random.choice(f_list)
h = random.choice(g)
i = random.choice(h)
def j():
    return i
k = j()
l = (k, k, k)
m, n, o = l
p = m + n + o
q_dict = {53: p, 88: p, 90: p, 40: p, 13: p, 63: p, 61: p, 21: p, 4: p, 55: p}
r = random.choice(list(q_dict.values()))
s_list = [r for _ in range(8)]
t_list = [s_list for _ in range(9)]
u_list = [t_list for _ in range(7)]
v = random.choice(u_list)
w = random.choice(v)
x = random.choice(w)
y = ''
for _ in range(4):
    y += x
z = y[0:]
aa = ''
counteraa = 0
while counteraa < 3:
    ab = ''
    counterab = 0
    while counterab < 2:
        ab += aa
        counterab += 1
        aa += z
        counteraa += 1
ac = [ab for _ in range(9)]
random.shuffle(ac)
ad = random.choice(ac)
ae = ''
counterae = 0
while counterae < 4:
    ae += ad
    counterae += 1
af = ae[0:]
ag_dict = {60: af, 30: af}
ah_dict = {24: ag_dict, 23: ag_dict, 34: ag_dict, 87: ag_dict, 79: ag_dict, 81: ag_dict, 52: ag_dict}
ai_dict = {42: ah_dict, 69: ah_dict, 12: ah_dict, 54: ah_dict, 6: ah_dict, 42: ah_dict}
aj = random.choice(list(ai_dict.values()))
ak = random.choice(list(aj.values()))
al = random.choice(list(ak.values()))
am_dict = {71: al, 24: al, 90: al, 38: al, 20: al, 26: al, 47: al}
an_dict = {71: am_dict, 31: am_dict, 54: am_dict, 78: am_dict, 78: am_dict, 49: am_dict}
ao = random.choice(list(an_dict.values()))
ap = random.choice(list(ao.values()))
aq_dict = {78: ap, 52: ap, 98: ap}
ar_dict = {82: aq_dict, 5: aq_dict, 93: aq_dict, 37: aq_dict, 56: aq_dict, 41: aq_dict, 4: aq_dict, 89: aq_dict, 52: aq_dict, 15: aq_dict}
at = random.choice(list(ar_dict.values()))
au = random.choice(list(at.values()))
av = au + '.'
aw = [av for _ in range(9)]
random.shuffle(aw)
ax = random.choice(aw)
print(ax)