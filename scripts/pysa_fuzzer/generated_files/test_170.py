import random
import math

a = input()
b_dict = {5: a, 88: a, 3: a, 88: a, 57: a}
c = random.choice(list(b_dict.values()))
d_set = {c, c, c, c, c, c, c}
d = random.choice(list(d_set))
e = f'string {d}'
f = e + '.'
g_set = {f, f, f, f, f, f, f, f}
g = random.choice(list(g_set))
h_list = [g for _ in range(5)]
i_list = [h_list for _ in range(2)]
j = random.choice(i_list)
k = random.choice(j)
l = f'string {k}'
m = l + '.'
def n():
    return m
def o():
    return n()
def p():
    return o()
q = p()
r = q[0:]
s_set = {r, r, r, r, r, r, r, r, r, r}
s = random.choice(list(s_set))
t = ''
for _ in range(8):
        if _ == 2:
            break
        t += s
u = t + '.'
v = f'string {u}'
w = [v for _ in range(6)]
random.shuffle(w)
x = random.choice(w)
y = [x for _ in range(6)]
random.shuffle(y)
z = random.choice(y)
aa_dict = {60: z, 48: z, 99: z, 68: z, 97: z, 46: z, 91: z, 55: z}
ab = random.choice(list(aa_dict.values()))
if ab == ab:
    ae = ab + 'c1'
elif ab == '14':
    ae = ac + 'c2'
else:
    ae = ad + 'c3'
print(ae)