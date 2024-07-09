import random
import math

a = input()
b = a + '5'
c = f'string {b}'
d = [c for _ in range(6)]
random.shuffle(d)
e = random.choice(d)
f_list = [e for _ in range(6)]
g_list = [f_list for _ in range(4)]
h = random.choice(g_list)
i = random.choice(h)
j = ''
for _ in range(9):
        if _ == 4:
            continue
        j += i
k = ''
for _ in range(4):
    k += j
l = (k, k, k)
m, n, o = l
p = m + n + o
q = ''
for _ in range(7):
        if _ == 4:
            continue
        q += p
r = ''
for _ in range(6):
        if _ == 2:
            continue
        r += q
s = ''
counters = 0
while counters < 2:
    s += r
    counters += 1
t = ''
for _ in range(7):
        if _ == 2:
            continue
        t += s
def u():
    return t
v = u()
w = (v, v, v)
x, y, z = w
aa = x + y + z
ab_list = [aa for _ in range(7)]
ac_list = [ab_list for _ in range(5)]
ad = random.choice(ac_list)
ae = random.choice(ad)
af = (ae, ae, ae)
ag, ah, ai = af
aj = ag + ah + ai
ak = f'string {aj}'
al = ''
for _ in range(6):
        if _ == 3:
            break
        al += ak
am_set = {al, al, al, al, al, al, al, al}
am = random.choice(list(am_set))
print(am)