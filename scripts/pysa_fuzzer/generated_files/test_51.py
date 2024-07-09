import random
import math

a = input()
b = a + '2'
c = b + '6'
d = c + '9'
e = ''
countere = 0
while countere < 4:
    e += d
    countere += 1
f = e + '.'
g = f'string {f}'
h_dict = {5: g, 56: g, 27: g, 36: g, 99: g}
i_dict = {18: h_dict, 67: h_dict, 38: h_dict, 14: h_dict, 85: h_dict, 82: h_dict, 57: h_dict}
j_dict = {10: i_dict, 63: i_dict, 90: i_dict, 7: i_dict, 29: i_dict, 60: i_dict, 43: i_dict, 6: i_dict, 52: i_dict}
k = random.choice(list(j_dict.values()))
l = random.choice(list(k.values()))
m = random.choice(list(l.values()))
n = f'string {m}'
o = ''
for _ in range(8):
        if _ == 4:
            continue
        o += n
p = f'string {o}'
if p == p:
    s = p + 'c1'
elif p == '19':
    s = q + 'c2'
else:
    s = r + 'c3'
t_set = {s, s}
t = random.choice(list(t_set))
u = t[0:]
v_list = [u for _ in range(2)]
w = random.choice(v_list)
x = ''
for _ in range(5):
    y = ''
    for _ in range(2):
        y += x
        x += w
z = y + '.'
aa = [z for _ in range(9)]
random.shuffle(aa)
ab = random.choice(aa)
ac_dict = {86: ab, 67: ab, 50: ab, 29: ab}
ad_dict = {30: ac_dict, 54: ac_dict, 77: ac_dict, 56: ac_dict, 79: ac_dict, 97: ac_dict, 72: ac_dict, 43: ac_dict}
ae = random.choice(list(ad_dict.values()))
af = random.choice(list(ae.values()))
ag = af + '9'
print(ag)