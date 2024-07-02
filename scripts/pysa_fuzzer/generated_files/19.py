import random
import math
a = input()
b_list = [a for _ in range(4)]
c_list = [b_list for _ in range(4)]
d = random.choice(c_list)
e = random.choice(d)
f = e + '1'
g = ''
counterg = 0
while counterg < 3:
    g += f
    counterg += 1
h_list = [g for _ in range(5)]
i_list = [h_list for _ in range(7)]
j_list = [i_list for _ in range(7)]
k = random.choice(j_list)
l = random.choice(k)
m = random.choice(l)
n = ''
for _ in range(3):
    for __ in range(4):
                n += m
if n == '8':
    o = n + ' c1'
elif n == '17':
    o = n + ' c2'
else:
    o = n + ' c3'
p_set = {o, o, o, o, o, o, o, o, o}
p = random.choice(list(p_set))
q = p[0:]
r = q + '4'
s = r + '2'
t = s + '7'
u = t + '1'
v = (u, u, u)
w, x, y = v
z = w + x + y
aa = z + '.'
ab = ''
counterab = 0
while counterab < 3:
    ab += aa
    counterab += 1
if ab == '1':
    ac = ab + ' c1'
elif ab == '12':
    ac = ab + ' c2'
else:
    ac = ab + ' c3'
ad_dict = {58: ac, 14: ac, 36: ac, 54: ac, 61: ac, 98: ac, 43: ac, 49: ac}
ae = random.choice(list(ad_dict.values()))
af_dict = {10: ae, 51: ae}
ag = random.choice(list(af_dict.values()))
ah = ''
for _ in range(10):
        if _ == 3:
            continue
        ah += ag
ai = (ah, ah, ah)
aj, ak, al = ai
am = aj + ak + al
print(am)