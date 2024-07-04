import random
import math
a = input()
b = a + '4'
c = b + '3'
d = c + '8'
e = d + '6'
f = [e for _ in range(10)]
random.shuffle(f)
g = random.choice(f)
h = g + '4'
i = h + '9'
j = i + '9'
k = j + '3'
l = [k for _ in range(9)]
random.shuffle(l)
m = random.choice(l)
n = m + '4'
o = n + '6'
p = [o for _ in range(10)]
random.shuffle(p)
q = random.choice(p)
r = ''
for _ in range(5):
    for __ in range(5):
                r += q
if r == '8':
    s = r + ' c1'
elif r == '17':
    s = r + ' c2'
else:
    s = r + ' c3'
t = ''
for _ in range(5):
    t += s
u = (t, t, t)
v, w, x = u
y = v + w + x
z = y + '.'
aa = z + '.'
ab = aa + '.'
ac_set = {ab, ab, ab, ab, ab, ab, ab, ab, ab}
ac = random.choice(list(ac_set))
ad_dict = {7: ac, 20: ac, 33: ac, 51: ac, 92: ac}
ae = random.choice(list(ad_dict.values()))
print(ae)