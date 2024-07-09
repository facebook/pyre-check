import random
import math

a = input()
b = f'string {a}'
c_dict = {7: b, 43: b, 67: b, 84: b, 11: b, 42: b}
d = random.choice(list(c_dict.values()))
e = [d for _ in range(9)]
random.shuffle(e)
f = random.choice(e)
g_list = [f for _ in range(7)]
h = random.choice(g_list)
i = f'string {h}'
j = (i, i, i)
k, l, m = j
n = k + l + m
if n == n:
    q = n + 'c1'
elif n == '16':
    q = o + 'c2'
else:
    q = p + 'c3'
r = q + '.'
s_list = [r for _ in range(4)]
t_list = [s_list for _ in range(4)]
u = random.choice(t_list)
v = random.choice(u)
if v == v:
    y = v + 'c1'
elif v == '12':
    y = w + 'c2'
else:
    y = x + 'c3'
z = [y for _ in range(9)]
random.shuffle(z)
aa = random.choice(z)
ab_list = [aa for _ in range(7)]
ac_list = [ab_list for _ in range(2)]
ad = random.choice(ac_list)
ae = random.choice(ad)
af = (ae, ae, ae)
ag, ah, ai = af
aj = ag + ah + ai
ak = ''
for _ in range(3):
    al = ''
    for _ in range(2):
        al += ak
        ak += aj
am = al + '.'
an = am[0:]
ao = f'string {an}'
ap_dict = {42: ao, 25: ao, 84: ao}
aq_dict = {55: ap_dict, 72: ap_dict, 15: ap_dict}
ar_dict = {8: aq_dict, 9: aq_dict, 67: aq_dict, 86: aq_dict, 73: aq_dict, 27: aq_dict, 17: aq_dict}
at = random.choice(list(ar_dict.values()))
au = random.choice(list(at.values()))
av = random.choice(list(au.values()))
print(av)