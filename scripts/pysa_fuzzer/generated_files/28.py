import random
import math
a = input()
if a == '3':
    b = a + ' c1'
elif a == '14':
    b = a + ' c2'
else:
    b = a + ' c3'
c = b + '.'
d_set = {c, c, c}
d = random.choice(list(d_set))
e = d + '.'
f = f'string {e}'
def g():
    return f
h = g()
i = h[0:]
j = i + '8'
k = j + '7'
l = k + '6'
if l == '8':
    m = l + ' c1'
elif l == '13':
    m = l + ' c2'
else:
    m = l + ' c3'
n = m[0:]
o = (n, n, n)
p, q, r = o
s = p + q + r
if s == '4':
    t = s + ' c1'
elif s == '14':
    t = s + ' c2'
else:
    t = s + ' c3'
u_list = [t for _ in range(5)]
v_list = [u_list for _ in range(7)]
w = random.choice(v_list)
x = random.choice(w)
def y():
    return x
z = y()
aa_dict = {86: z, 44: z}
ab = random.choice(list(aa_dict.values()))
ac = f'string {ab}'
ad = ''
counterad = 0
while counterad < 5:
    ad += ac
    counterad += 1
ae = [ad for _ in range(7)]
random.shuffle(ae)
af = random.choice(ae)
print(af)