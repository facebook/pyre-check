import random
import math

a = input()
b = [a for _ in range(6)]
random.shuffle(b)
c = random.choice(b)
d = [c for _ in range(7)]
random.shuffle(d)
e = random.choice(d)
f = ''
counterf = 0
while counterf < 3:
    g = ''
    counterg = 0
    while counterg < 2:
        g += f
        counterg += 1
        f += e
        counterf += 1
h = f'string {g}'
i = f'string {h}'
j = i + '.'
k = j[0:]
l = ''
for _ in range(5):
        if _ == 5:
            continue
        l += k
if l == l:
    o = l + 'c1'
elif l == '14':
    o = m + 'c2'
else:
    o = n + 'c3'
p = ''
counterp = 0
while counterp < 2:
    q = ''
    counterq = 0
    while counterq < 4:
        q += p
        counterq += 1
        p += o
        counterp += 1
r = ''
for _ in range(10):
        if _ == 4:
            break
        r += q
s_set = {r, r}
s = random.choice(list(s_set))
t = ''
for _ in range(3):
    u = ''
    for _ in range(5):
        v = ''
        for _ in range(2):
            v += u
            u += t
        t += s
w_dict = {22: v, 6: v, 89: v, 43: v, 72: v, 96: v, 100: v, 92: v, 36: v}
x_dict = {33: w_dict, 83: w_dict}
y = random.choice(list(x_dict.values()))
z = random.choice(list(y.values()))
aa = [z for _ in range(5)]
random.shuffle(aa)
ab = random.choice(aa)
if ab == ab:
    ae = ab + 'c1'
elif ab == '20':
    ae = ac + 'c2'
else:
    ae = ad + 'c3'
af = ''
counteraf = 0
while counteraf < 4:
    af += ae
    counteraf += 1
ag_dict = {89: af, 28: af, 75: af, 68: af, 62: af, 52: af, 16: af, 72: af, 88: af}
ah_dict = {60: ag_dict, 89: ag_dict, 1: ag_dict, 64: ag_dict}
ai_dict = {47: ah_dict, 13: ah_dict, 83: ah_dict, 18: ah_dict, 91: ah_dict}
aj = random.choice(list(ai_dict.values()))
ak = random.choice(list(aj.values()))
al = random.choice(list(ak.values()))
print(al)