import random
import math

a = input()
b = f'string {a}'
c_list = [b for _ in range(10)]
d = random.choice(c_list)
e_dict = {92: d, 18: d, 93: d, 14: d, 58: d, 14: d}
f_dict = {13: e_dict, 82: e_dict, 23: e_dict, 26: e_dict, 42: e_dict, 46: e_dict, 29: e_dict, 98: e_dict, 21: e_dict, 46: e_dict}
g_dict = {16: f_dict, 6: f_dict, 77: f_dict, 8: f_dict}
h = random.choice(list(g_dict.values()))
i = random.choice(list(h.values()))
j = random.choice(list(i.values()))
if j == j:
    m = j + 'c1'
elif j == '17':
    m = k + 'c2'
else:
    m = l + 'c3'
n = m + '8'
o = n + '1'
p = o + '6'
q_set = {p, p, p, p, p, p}
q = random.choice(list(q_set))
r_set = {q, q, q}
r = random.choice(list(r_set))
s = r + '6'
t = s + '1'
u = [t for _ in range(8)]
random.shuffle(u)
v = random.choice(u)
w = ''
for _ in range(8):
        if _ == 4:
            break
        w += v
x = w + '7'
y = x + '2'
z = y + '9'
def aa():
    return z
ab = aa()
def ac():
    return ab
ad = ac()
def ae():
    return ad
af = ae()
ag = af + '3'
ah = [ag for _ in range(5)]
random.shuffle(ah)
ai = random.choice(ah)
aj = ''
for _ in range(5):
    ak = ''
    for _ in range(2):
        ak += aj
        aj += ai
al = ak[0:]
print(al)