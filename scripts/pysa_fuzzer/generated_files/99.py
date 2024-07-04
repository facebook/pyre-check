import random
import math
a = input()
b = a + '.'
c = b + '4'
d = c + '6'
e = d + '6'
f_set = {e, e, e, e, e, e, e, e, e, e}
f = random.choice(list(f_set))
g = [f for _ in range(10)]
random.shuffle(g)
h = random.choice(g)
i = h + '.'
j = ''
for _ in range(4):
    for __ in range(5):
                j += i
if j == '2':
    k = j + ' c1'
elif j == '12':
    k = j + ' c2'
else:
    k = j + ' c3'
l = (k, k, k)
m, n, o = l
p = m + n + o
q_list = [p for _ in range(9)]
r_list = [q_list for _ in range(8)]
s_list = [r_list for _ in range(4)]
t = random.choice(s_list)
u = random.choice(t)
v = random.choice(u)
w = v[0:]
if w == '2':
    x = w + ' c1'
elif w == '16':
    x = w + ' c2'
else:
    x = w + ' c3'
y = [x for _ in range(5)]
random.shuffle(y)
z = random.choice(y)
aa = ''
for _ in range(10):
        if _ == 5:
            continue
        aa += z
ab = ''
for _ in range(5):
        if _ == 3:
            break
        ab += aa
ac = ''
for _ in range(4):
    ac += ab
def ad():
    return ac
ae = ad()
af = ''
for _ in range(3):
    for __ in range(3):
                af += ae
ag_set = {af, af, af, af, af, af, af, af, af}
ag = random.choice(list(ag_set))
print(ag)