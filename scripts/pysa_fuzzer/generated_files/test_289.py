import random
import math

a = input()
b = a + '.'
c_list = [b for _ in range(9)]
d_list = [c_list for _ in range(10)]
e_list = [d_list for _ in range(10)]
f = random.choice(e_list)
g = random.choice(f)
h = random.choice(g)
i_dict = {34: h, 53: h, 92: h, 4: h, 38: h, 2: h}
j_dict = {97: i_dict, 98: i_dict, 82: i_dict, 62: i_dict, 7: i_dict, 73: i_dict, 32: i_dict, 72: i_dict, 39: i_dict}
k_dict = {59: j_dict, 19: j_dict}
l = random.choice(list(k_dict.values()))
m = random.choice(list(l.values()))
n = random.choice(list(m.values()))
o = n + '8'
p = o + '9'
def q():
    return p
def r():
    return q()
s = r()
t = ''
for _ in range(3):
    for __ in range(4):
                t += s
u_dict = {9: t, 77: t, 32: t}
v_dict = {83: u_dict, 85: u_dict, 55: u_dict, 54: u_dict, 53: u_dict, 32: u_dict}
w = random.choice(list(v_dict.values()))
x = random.choice(list(w.values()))
y = ''
for _ in range(4):
    z = ''
    for _ in range(5):
        aa = ''
        for _ in range(3):
            aa += z
            z += y
        y += x
ab = ''
for _ in range(4):
    ac = ''
    for _ in range(5):
        ac += ab
        ab += aa
def ad():
    return ac
ae = ad()
af = ''
for _ in range(2):
    for __ in range(3):
                af += ae
ag = ''
counterag = 0
while counterag < 3:
    ah = ''
    counterah = 0
    while counterah < 5:
        ah += ag
        counterah += 1
        ag += af
        counterag += 1
ai = f'string {ah}'
aj = [ai for _ in range(10)]
random.shuffle(aj)
ak = random.choice(aj)
al = ''
for _ in range(8):
        if _ == 4:
            break
        al += ak
am = ''
for _ in range(3):
    an = ''
    for _ in range(2):
        an += am
        am += al
ao = ''
for _ in range(2):
    ap = ''
    for _ in range(3):
        aq = ''
        for _ in range(3):
            aq += ap
            ap += ao
        ao += an
ar = f'string {aq}'
print(ar)