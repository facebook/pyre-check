import random
import math
a = input()
b = a + '8'
c_dict = {57: b, 55: b, 49: b, 37: b, 50: b, 97: b, 12: b, 54: b, 93: b}
d_dict = {62: c_dict, 99: c_dict, 21: c_dict, 96: c_dict, 66: c_dict}
e_dict = {33: d_dict, 40: d_dict, 92: d_dict, 88: d_dict, 58: d_dict, 94: d_dict, 30: d_dict, 5: d_dict, 95: d_dict}
f = random.choice(list(e_dict.values()))
g = random.choice(list(f.values()))
h = random.choice(list(g.values()))
i = f'string {h}'
if i == '10':
    j = i + ' c1'
elif i == '20':
    j = i + ' c2'
else:
    j = i + ' c3'
k = j + '6'
l_list = [k for _ in range(7)]
m = random.choice(l_list)
n_list = [m for _ in range(6)]
o_list = [n_list for _ in range(2)]
p = random.choice(o_list)
q = random.choice(p)
r_dict = {49: q, 4: q, 74: q, 55: q, 88: q, 86: q, 52: q, 11: q, 40: q}
s_dict = {78: r_dict, 9: r_dict, 42: r_dict, 41: r_dict, 5: r_dict, 30: r_dict}
t_dict = {16: s_dict, 86: s_dict}
u = random.choice(list(t_dict.values()))
v = random.choice(list(u.values()))
w = random.choice(list(v.values()))
x = (w, w, w)
y, z, aa = x
ab = y + z + aa
ac = ''
counterac = 0
while counterac < 5:
    ad = ''
    counterad = 0
    while counterad < 5:
        ad += ac
        counterad += 1
        ac += ab
        counterac += 1
if ad == '7':
    ae = ad + ' c1'
elif ad == '18':
    ae = ad + ' c2'
else:
    ae = ad + ' c3'
af = ae + '.'
ag_set = {af, af}
ag = random.choice(list(ag_set))
ah = ''
for _ in range(3):
    ai = ''
    for _ in range(3):
        aj = ''
        for _ in range(3):
            aj += ai
            ai += ah
        ah += ag
ak = f'string {aj}'
al = [ak for _ in range(9)]
random.shuffle(al)
am = random.choice(al)
an = f'string {am}'
def ao():
    return an
def ap():
    return ao()
aq = ap()
print(aq)