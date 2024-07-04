import random
import math
a = input()
b = ''
counterb = 0
while counterb < 4:
    c = ''
    counterc = 0
    while counterc < 4:
        d = ''
        counterd = 0
        while counterd < 3:
            d += c
            counterd += 1
            c += b
            counterc += 1
        b += a
        counterb += 1
e = f'string {d}'
f = [e for _ in range(5)]
random.shuffle(f)
g = random.choice(f)
h = ''
for _ in range(7):
        if _ == 1:
            continue
        h += g
if h == '8':
    i = h + ' c1'
elif h == '18':
    i = h + ' c2'
else:
    i = h + ' c3'
j = ''
for _ in range(3):
    for __ in range(3):
                j += i
k = ''
counterk = 0
while counterk < 2:
    k += j
    counterk += 1
l = k[0:]
if l == '4':
    m = l + ' c1'
elif l == '20':
    m = l + ' c2'
else:
    m = l + ' c3'
if m == '2':
    n = m + ' c1'
elif m == '19':
    n = m + ' c2'
else:
    n = m + ' c3'
o = ''
for _ in range(4):
    for __ in range(4):
                o += n
p = o + '.'
q = p + '1'
r = q + '4'
s = [r for _ in range(6)]
random.shuffle(s)
t = random.choice(s)
u = ''
for _ in range(2):
    v = ''
    for _ in range(5):
        v += u
        u += t
w_list = [v for _ in range(2)]
x_list = [w_list for _ in range(3)]
y_list = [x_list for _ in range(9)]
z = random.choice(y_list)
aa = random.choice(z)
ab = random.choice(aa)
ac_list = [ab for _ in range(7)]
ad_list = [ac_list for _ in range(8)]
ae_list = [ad_list for _ in range(8)]
af = random.choice(ae_list)
ag = random.choice(af)
ah = random.choice(ag)
def ai():
    return ah
def aj():
    return ai()
ak = aj()
print(ak)