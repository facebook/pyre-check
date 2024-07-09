import random
import math

a = input()
if a == a:
    d = a + 'c1'
elif a == '13':
    d = b + 'c2'
else:
    d = c + 'c3'
e_list = [d for _ in range(6)]
f_list = [e_list for _ in range(6)]
g_list = [f_list for _ in range(6)]
h = random.choice(g_list)
i = random.choice(h)
j = random.choice(i)
k = ''
counterk = 0
while counterk < 4:
    l = ''
    counterl = 0
    while counterl < 4:
        m = ''
        counterm = 0
        while counterm < 4:
            m += l
            counterm += 1
            l += k
            counterl += 1
        k += j
        counterk += 1
n = ''
for _ in range(2):
    o = ''
    for _ in range(2):
        o += n
        n += m
p = o[0:]
q = p[0:]
r = ''
for _ in range(3):
    for __ in range(4):
                r += q
s = (r, r, r)
t, u, v = s
w = t + u + v
x = [w for _ in range(5)]
random.shuffle(x)
y = random.choice(x)
z = f'string {y}'
aa = (z, z, z)
ab, ac, ad = aa
ae = ab + ac + ad
af = ''
for _ in range(2):
    af += ae
ag_dict = {98: af, 31: af, 54: af, 94: af}
ah_dict = {76: ag_dict, 90: ag_dict, 31: ag_dict}
ai = random.choice(list(ah_dict.values()))
aj = random.choice(list(ai.values()))
def ak():
    return aj
def al():
    return ak()
def am():
    return al()
an = am()
ao = an[0:]
ap = (ao, ao, ao)
aq, ar, at = ap
au = aq + ar + at
av = (au, au, au)
aw, ax, ay = av
az = aw + ax + ay
def ba():
    return az
def bb():
    return ba()
def bc():
    return bb()
bd = bc()
print(bd)