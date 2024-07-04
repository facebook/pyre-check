import random
import math
a = input()
def b():
    return a
def c():
    return b()
d = c()
e = f'string {d}'
f = ''
for _ in range(2):
    for __ in range(3):
                f += e
if f == '1':
    g = f + ' c1'
elif f == '16':
    g = f + ' c2'
else:
    g = f + ' c3'
h_set = {g, g, g, g, g, g, g, g, g}
h = random.choice(list(h_set))
i = h + '.'
j = ''
for _ in range(4):
    for __ in range(3):
                j += i
k = [j for _ in range(8)]
random.shuffle(k)
l = random.choice(k)
m = (l, l, l)
n, o, p = m
q = n + o + p
r = q + '.'
s_list = [r for _ in range(3)]
t = random.choice(s_list)
u = t[0:]
v = f'string {u}'
w = v + '.'
def x():
    return w
y = x()
if y == '4':
    z = y + ' c1'
elif y == '19':
    z = y + ' c2'
else:
    z = y + ' c3'
aa = ''
for _ in range(5):
        if _ == 4:
            break
        aa += z
ab_dict = {5: aa, 14: aa, 62: aa, 80: aa, 81: aa, 23: aa, 97: aa, 33: aa, 41: aa, 59: aa}
ac_dict = {68: ab_dict, 20: ab_dict, 73: ab_dict, 32: ab_dict, 1: ab_dict, 28: ab_dict, 30: ab_dict, 15: ab_dict}
ad_dict = {74: ac_dict, 76: ac_dict, 47: ac_dict, 88: ac_dict, 38: ac_dict}
ae = random.choice(list(ad_dict.values()))
af = random.choice(list(ae.values()))
ag = random.choice(list(af.values()))
print(ag)