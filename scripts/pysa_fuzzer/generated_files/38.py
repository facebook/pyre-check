import random
import math
a = input()
b = (a, a, a)
c, d, e = b
f = c + d + e
if f == '10':
    g = f + ' c1'
elif f == '11':
    g = f + ' c2'
else:
    g = f + ' c3'
h = g + '7'
i = h + '1'
j_dict = {86: i, 75: i, 40: i, 96: i, 53: i, 74: i, 5: i, 5: i, 57: i}
k_dict = {32: j_dict, 63: j_dict, 48: j_dict, 83: j_dict, 49: j_dict, 81: j_dict}
l = random.choice(list(k_dict.values()))
m = random.choice(list(l.values()))
n = ''
for _ in range(2):
    for __ in range(3):
                n += m
def o():
    return n
def p():
    return o()
q = p()
r_set = {q, q, q}
r = random.choice(list(r_set))
s = r + '.'
t = f'string {s}'
u_list = [t for _ in range(5)]
v_list = [u_list for _ in range(6)]
w = random.choice(v_list)
x = random.choice(w)
y = ''
for _ in range(10):
        if _ == 4:
            break
        y += x
z = ''
counterz = 0
while counterz < 4:
    aa = ''
    counteraa = 0
    while counteraa < 4:
        aa += z
        counteraa += 1
        z += y
        counterz += 1
ab_dict = {13: aa, 68: aa, 12: aa, 70: aa, 68: aa, 79: aa, 79: aa, 30: aa}
ac_dict = {68: ab_dict, 60: ab_dict}
ad_dict = {72: ac_dict, 23: ac_dict, 75: ac_dict, 95: ac_dict, 57: ac_dict, 36: ac_dict, 68: ac_dict, 23: ac_dict}
ae = random.choice(list(ad_dict.values()))
af = random.choice(list(ae.values()))
ag = random.choice(list(af.values()))
ah = ''
for _ in range(3):
    for __ in range(4):
                ah += ag
ai = f'string {ah}'
aj_dict = {92: ai, 16: ai, 92: ai, 33: ai, 41: ai, 4: ai, 7: ai, 64: ai}
ak_dict = {51: aj_dict, 37: aj_dict, 76: aj_dict, 8: aj_dict, 51: aj_dict, 71: aj_dict, 61: aj_dict, 38: aj_dict}
al = random.choice(list(ak_dict.values()))
am = random.choice(list(al.values()))
an = [am for _ in range(7)]
random.shuffle(an)
ao = random.choice(an)
ap = ''
for _ in range(5):
    for __ in range(4):
                ap += ao
print(ap)