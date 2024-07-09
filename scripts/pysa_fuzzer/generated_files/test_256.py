import random
import math

a = input()
b_list = [a for _ in range(3)]
c_list = [b_list for _ in range(3)]
d = random.choice(c_list)
e = random.choice(d)
f_dict = {32: e, 83: e, 90: e, 76: e, 38: e}
g_dict = {61: f_dict, 30: f_dict, 57: f_dict, 40: f_dict, 74: f_dict}
h_dict = {29: g_dict, 32: g_dict, 65: g_dict, 90: g_dict, 81: g_dict, 73: g_dict, 40: g_dict, 82: g_dict}
i = random.choice(list(h_dict.values()))
j = random.choice(list(i.values()))
k = random.choice(list(j.values()))
l = (k, k, k)
m, n, o = l
p = m + n + o
q = (p, p, p)
r, s, t = q
u = r + s + t
v_dict = {41: u, 1: u, 100: u}
w_dict = {31: v_dict, 84: v_dict, 93: v_dict, 65: v_dict, 92: v_dict, 89: v_dict, 11: v_dict, 31: v_dict}
x_dict = {39: w_dict, 9: w_dict, 64: w_dict}
y = random.choice(list(x_dict.values()))
z = random.choice(list(y.values()))
aa = random.choice(list(z.values()))
def ab():
    return aa
def ac():
    return ab()
ad = ac()
ae = ''
for _ in range(4):
    for __ in range(5):
                ae += ad
def af():
    return ae
ag = af()
ah_dict = {66: ag, 82: ag, 72: ag}
ai_dict = {70: ah_dict, 84: ah_dict, 87: ah_dict, 76: ah_dict, 87: ah_dict, 21: ah_dict, 9: ah_dict, 62: ah_dict}
aj = random.choice(list(ai_dict.values()))
ak = random.choice(list(aj.values()))
al_set = {ak, ak, ak, ak, ak}
al = random.choice(list(al_set))
if al == al:
    ao = al + 'c1'
elif al == '15':
    ao = am + 'c2'
else:
    ao = an + 'c3'
ap = ''
for _ in range(2):
    aq = ''
    for _ in range(4):
        ar = ''
        for _ in range(5):
            ar += aq
            aq += ap
        ap += ao
at = ar[0:]
def au():
    return at
def av():
    return au()
aw = av()
ax_set = {aw, aw, aw}
ax = random.choice(list(ax_set))
ay_dict = {53: ax, 63: ax}
az_dict = {19: ay_dict, 9: ay_dict, 54: ay_dict, 5: ay_dict, 60: ay_dict, 83: ay_dict, 18: ay_dict, 91: ay_dict, 5: ay_dict, 52: ay_dict}
ba = random.choice(list(az_dict.values()))
bb = random.choice(list(ba.values()))
bc = (bb, bb, bb)
bd, be, bf = bc
bg = bd + be + bf
bh_set = {bg, bg, bg, bg}
bh = random.choice(list(bh_set))
print(bh)