import random
import math

a = input()
b = f'string {a}'
c = f'string {b}'
d = f'string {c}'
e = f'string {d}'
f_set = {e, e, e}
f = random.choice(list(f_set))
g_dict = {7: f, 33: f, 98: f, 61: f, 57: f, 90: f, 74: f, 1: f, 5: f}
h_dict = {45: g_dict, 75: g_dict, 23: g_dict, 59: g_dict, 3: g_dict}
i = random.choice(list(h_dict.values()))
j = random.choice(list(i.values()))
k = ''
for _ in range(10):
        if _ == 4:
            continue
        k += j
l = ''
for _ in range(5):
    l += k
m = l[0:]
if m == m:
    p = m + 'c1'
elif m == '16':
    p = n + 'c2'
else:
    p = o + 'c3'
q = (p, p, p)
r, s, t = q
u = r + s + t
v = u + '.'
w_dict = {43: v, 64: v, 56: v, 81: v, 46: v, 16: v, 3: v, 84: v, 59: v}
x = random.choice(list(w_dict.values()))
if x == x:
    aa = x + 'c1'
elif x == '18':
    aa = y + 'c2'
else:
    aa = z + 'c3'
if aa == aa:
    ad = aa + 'c1'
elif aa == '19':
    ad = ab + 'c2'
else:
    ad = ac + 'c3'
ae = [ad for _ in range(6)]
random.shuffle(ae)
af = random.choice(ae)
ag = [af for _ in range(7)]
random.shuffle(ag)
ah = random.choice(ag)
if ah == ah:
    ak = ah + 'c1'
elif ah == '14':
    ak = ai + 'c2'
else:
    ak = aj + 'c3'
print(ak)