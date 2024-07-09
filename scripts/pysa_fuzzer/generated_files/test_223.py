import random
import math

a = input()
b_set = {a, a, a, a, a, a, a}
b = random.choice(list(b_set))
c = ''
for _ in range(4):
    for __ in range(4):
                c += b
d = c + '.'
e_list = [d for _ in range(7)]
f_list = [e_list for _ in range(8)]
g_list = [f_list for _ in range(3)]
h = random.choice(g_list)
i = random.choice(h)
j = random.choice(i)
k = [j for _ in range(6)]
random.shuffle(k)
l = random.choice(k)
m_list = [l for _ in range(6)]
n_list = [m_list for _ in range(9)]
o_list = [n_list for _ in range(6)]
p = random.choice(o_list)
q = random.choice(p)
r = random.choice(q)
s_set = {r, r, r, r, r, r, r, r, r, r}
s = random.choice(list(s_set))
t = ''
for _ in range(4):
    t += s
u = [t for _ in range(8)]
random.shuffle(u)
v = random.choice(u)
w_set = {v, v, v}
w = random.choice(list(w_set))
x_dict = {100: w, 89: w, 81: w}
y = random.choice(list(x_dict.values()))
z = f'string {y}'
aa_dict = {69: z, 6: z, 97: z, 65: z, 53: z, 60: z, 48: z, 44: z}
ab = random.choice(list(aa_dict.values()))
ac = ''
counterac = 0
while counterac < 4:
    ad = ''
    counterad = 0
    while counterad < 4:
        ad += ac
        counterad += 1
        ac += ab
        counterac += 1
ae = ad[0:]
af = f'string {ae}'
ag = af + '6'
ah = ag + '8'
ai = ah + '4'
aj = ai + '7'
print(aj)