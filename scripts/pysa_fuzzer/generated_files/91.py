import random
import math
a = input()
b = ''
for _ in range(9):
        if _ == 1:
            continue
        b += a
if b == '4':
    c = b + ' c1'
elif b == '14':
    c = b + ' c2'
else:
    c = b + ' c3'
d = ''
counterd = 0
while counterd < 2:
    d += c
    counterd += 1
e = ''
countere = 0
while countere < 4:
    e += d
    countere += 1
f = ''
for _ in range(5):
        if _ == 2:
            continue
        f += e
g = [f for _ in range(9)]
random.shuffle(g)
h = random.choice(g)
i = f'string {h}'
j = i + '1'
k = j + '7'
l = k + '8'
m = l + '9'
n = m + '6'
o = n + '9'
p = f'string {o}'
q = p + '1'
r = q + '8'
s_set = {r, r, r, r, r, r, r, r, r, r}
s = random.choice(list(s_set))
t = (s, s, s)
u, v, w = t
x = u + v + w
y_list = [x for _ in range(9)]
z = random.choice(y_list)
aa_list = [z for _ in range(6)]
ab_list = [aa_list for _ in range(2)]
ac_list = [ab_list for _ in range(8)]
ad = random.choice(ac_list)
ae = random.choice(ad)
af = random.choice(ae)
ag = f'string {af}'
if ag == '2':
    ah = ag + ' c1'
elif ag == '18':
    ah = ag + ' c2'
else:
    ah = ag + ' c3'
print(ah)