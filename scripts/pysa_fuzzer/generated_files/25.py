import random
import math
a = input()
b = a[0:]
c = f'string {b}'
d = [c for _ in range(8)]
random.shuffle(d)
e = random.choice(d)
f = e + '6'
g = f + '4'
h = g + '6'
i = [h for _ in range(9)]
random.shuffle(i)
j = random.choice(i)
k = f'string {j}'
l = k + '.'
m = l + '8'
n = m + '8'
o = n + '7'
p = o[0:]
q = p[0:]
r = q[0:]
s_set = {r, r, r}
s = random.choice(list(s_set))
t = s + '7'
u = t + '7'
v_dict = {29: u, 91: u, 80: u, 73: u, 82: u, 29: u, 43: u, 44: u, 7: u}
w = random.choice(list(v_dict.values()))
x = ''
for _ in range(2):
    y = ''
    for _ in range(2):
        y += x
        x += w
z = ''
for _ in range(6):
        if _ == 1:
            break
        z += y
aa = ''
for _ in range(9):
        if _ == 2:
            continue
        aa += z
ab = ''
for _ in range(5):
    for __ in range(3):
                ab += aa
print(ab)