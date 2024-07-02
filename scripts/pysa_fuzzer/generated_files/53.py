import random
import math
a = input()
b_list = [a for _ in range(2)]
c = random.choice(b_list)
d_list = [c for _ in range(7)]
e_list = [d_list for _ in range(2)]
f = random.choice(e_list)
g = random.choice(f)
h = ''
for _ in range(3):
    for __ in range(5):
                h += g
i = h + '2'
j = i + '8'
k = j + '8'
l = (k, k, k)
m, n, o = l
p = m + n + o
q = [p for _ in range(10)]
random.shuffle(q)
r = random.choice(q)
s = ''
for _ in range(10):
        if _ == 2:
            continue
        s += r
t = s + '1'
u = ''
for _ in range(7):
        if _ == 1:
            continue
        u += t
v = (u, u, u)
w, x, y = v
z = w + x + y
def aa():
    return z
def ab():
    return aa()
def ac():
    return ab()
ad = ac()
ae = f'string {ad}'
def af():
    return ae
def ag():
    return af()
ah = ag()
ai_dict = {62: ah, 2: ah, 2: ah, 95: ah, 82: ah, 7: ah}
aj_dict = {13: ai_dict, 27: ai_dict, 39: ai_dict, 92: ai_dict, 29: ai_dict, 96: ai_dict, 34: ai_dict, 12: ai_dict, 37: ai_dict}
ak_dict = {87: aj_dict, 47: aj_dict, 52: aj_dict, 31: aj_dict, 45: aj_dict, 6: aj_dict, 19: aj_dict, 16: aj_dict, 50: aj_dict}
al = random.choice(list(ak_dict.values()))
am = random.choice(list(al.values()))
an = random.choice(list(am.values()))
ao = ''
counterao = 0
while counterao < 2:
    ap = ''
    counterap = 0
    while counterap < 2:
        ap += ao
        counterap += 1
        ao += an
        counterao += 1
aq = ''
for _ in range(5):
    ar = ''
    for _ in range(3):
        ar += aq
        aq += ap
at = ar + '1'
au = at + '1'
av = ''
for _ in range(4):
    for __ in range(3):
                av += au
print(av)