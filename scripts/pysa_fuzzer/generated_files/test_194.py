import random
import math

a = input()
b = [a for _ in range(7)]
random.shuffle(b)
c = random.choice(b)
d_dict = {25: c, 81: c}
e = random.choice(list(d_dict.values()))
f = ''
for _ in range(5):
    g = ''
    for _ in range(3):
        h = ''
        for _ in range(4):
            h += g
            g += f
        f += e
i = h + '.'
j = ''
counterj = 0
while counterj < 3:
    k = ''
    counterk = 0
    while counterk < 5:
        k += j
        counterk += 1
        j += i
        counterj += 1
l = (k, k, k)
m, n, o = l
p = m + n + o
q = ''
for _ in range(4):
    r = ''
    for _ in range(3):
        s = ''
        for _ in range(4):
            s += r
            r += q
        q += p
t_list = [s for _ in range(10)]
u_list = [t_list for _ in range(10)]
v = random.choice(u_list)
w = random.choice(v)
x = (w, w, w)
y, z, aa = x
ab = y + z + aa
ac_list = [ab for _ in range(5)]
ad_list = [ac_list for _ in range(7)]
ae = random.choice(ad_list)
af = random.choice(ae)
ag_set = {af, af, af, af, af}
ag = random.choice(list(ag_set))
ah_list = [ag for _ in range(6)]
ai_list = [ah_list for _ in range(10)]
aj_list = [ai_list for _ in range(2)]
ak = random.choice(aj_list)
al = random.choice(ak)
am = random.choice(al)
an_set = {am, am, am, am, am, am, am, am}
an = random.choice(list(an_set))
ao = ''
for _ in range(8):
        if _ == 1:
            break
        ao += an
if ao == ao:
    ar = ao + 'c1'
elif ao == '17':
    ar = ap + 'c2'
else:
    ar = aq + 'c3'
at = ar[0:]
def au():
    return at
def av():
    return au()
aw = av()
ax = (aw, aw, aw)
ay, az, ba = ax
bb = ay + az + ba
print(bb)