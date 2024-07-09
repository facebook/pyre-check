import random
import math

a = input()
b = f'string {a}'
c = ''
for _ in range(2):
    for __ in range(5):
                c += b
d_set = {c, c, c, c, c, c}
d = random.choice(list(d_set))
e = ''
for _ in range(4):
    e += d
f = [e for _ in range(9)]
random.shuffle(f)
g = random.choice(f)
h = f'string {g}'
i = h[0:]
j = i[0:]
k = j + '.'
l_list = [k for _ in range(7)]
m_list = [l_list for _ in range(6)]
n = random.choice(m_list)
o = random.choice(n)
p = ''
for _ in range(4):
    for __ in range(2):
                p += o
q = ''
counterq = 0
while counterq < 4:
    q += p
    counterq += 1
r_set = {q, q, q, q, q, q, q, q, q, q}
r = random.choice(list(r_set))
s_dict = {90: r, 70: r, 78: r, 10: r, 40: r}
t_dict = {79: s_dict, 77: s_dict, 19: s_dict, 80: s_dict, 26: s_dict, 26: s_dict, 65: s_dict, 42: s_dict, 23: s_dict}
u_dict = {24: t_dict, 69: t_dict, 16: t_dict, 45: t_dict, 7: t_dict, 20: t_dict, 71: t_dict}
v = random.choice(list(u_dict.values()))
w = random.choice(list(v.values()))
x = random.choice(list(w.values()))
y_list = [x for _ in range(10)]
z = random.choice(y_list)
aa_dict = {48: z, 83: z, 90: z, 35: z, 7: z}
ab_dict = {56: aa_dict, 84: aa_dict, 61: aa_dict, 32: aa_dict, 28: aa_dict, 48: aa_dict}
ac = random.choice(list(ab_dict.values()))
ad = random.choice(list(ac.values()))
def ae():
    return ad
def af():
    return ae()
ag = af()
ah = ag + '.'
print(ah)