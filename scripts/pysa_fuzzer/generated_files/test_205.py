import random
import math

a = input()
b_list = [a for _ in range(8)]
c_list = [b_list for _ in range(8)]
d_list = [c_list for _ in range(5)]
e = random.choice(d_list)
f = random.choice(e)
g = random.choice(f)
h_set = {g, g, g, g, g, g, g, g, g}
h = random.choice(list(h_set))
i_set = {h, h}
i = random.choice(list(i_set))
j = ''
for _ in range(3):
    k = ''
    for _ in range(2):
        k += j
        j += i
l = [k for _ in range(10)]
random.shuffle(l)
m = random.choice(l)
def n():
    return m
def o():
    return n()
p = o()
q = (p, p, p)
r, s, t = q
u = r + s + t
v_dict = {80: u, 42: u, 18: u, 81: u}
w_dict = {74: v_dict, 28: v_dict, 75: v_dict, 77: v_dict, 3: v_dict, 60: v_dict, 27: v_dict, 7: v_dict, 71: v_dict, 84: v_dict}
x = random.choice(list(w_dict.values()))
y = random.choice(list(x.values()))
if y == y:
    ab = y + 'c1'
elif y == '14':
    ab = z + 'c2'
else:
    ab = aa + 'c3'
ac = (ab, ab, ab)
ad, ae, af = ac
ag = ad + ae + af
ah = ''
for _ in range(10):
        if _ == 2:
            break
        ah += ag
ai = ''
for _ in range(9):
        if _ == 5:
            break
        ai += ah
aj = ''
counteraj = 0
while counteraj < 5:
    aj += ai
    counteraj += 1
if aj == aj:
    am = aj + 'c1'
elif aj == '17':
    am = ak + 'c2'
else:
    am = al + 'c3'
def an():
    return am
def ao():
    return an()
ap = ao()
aq = ap[0:]
ar = aq + '.'
at = ar + '3'
au = at + '8'
av = au + '8'
print(av)