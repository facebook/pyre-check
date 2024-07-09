import random
import math

a = input()
def b():
    return a
def c():
    return b()
d = c()
e_dict = {85: d, 53: d, 31: d, 22: d, 62: d, 51: d, 94: d, 73: d, 55: d, 37: d}
f = random.choice(list(e_dict.values()))
g = [f for _ in range(8)]
random.shuffle(g)
h = random.choice(g)
def i():
    return h
j = i()
k = ''
counterk = 0
while counterk < 5:
    l = ''
    counterl = 0
    while counterl < 3:
        l += k
        counterl += 1
        k += j
        counterk += 1
m = (l, l, l)
n, o, p = m
q = n + o + p
r = [q for _ in range(8)]
random.shuffle(r)
s = random.choice(r)
t = ''
for _ in range(10):
        if _ == 1:
            continue
        t += s
u = t + '.'
v = f'string {u}'
w_list = [v for _ in range(9)]
x_list = [w_list for _ in range(5)]
y = random.choice(x_list)
z = random.choice(y)
aa = ''
for _ in range(4):
    for __ in range(2):
                aa += z
if aa == aa:
    ad = aa + 'c1'
elif aa == '15':
    ad = ab + 'c2'
else:
    ad = ac + 'c3'
ae = [ad for _ in range(10)]
random.shuffle(ae)
af = random.choice(ae)
if af == af:
    ai = af + 'c1'
elif af == '13':
    ai = ag + 'c2'
else:
    ai = ah + 'c3'
if ai == ai:
    al = ai + 'c1'
elif ai == '16':
    al = aj + 'c2'
else:
    al = ak + 'c3'
am_set = {al, al, al, al, al, al, al, al, al, al}
am = random.choice(list(am_set))
an = f'string {am}'
print(an)