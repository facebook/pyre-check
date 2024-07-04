import random
import math
a = input()
b = a + '.'
c = ''
counterc = 0
while counterc < 2:
    c += b
    counterc += 1
d = (c, c, c)
e, f, g = d
h = e + f + g
def i():
    return h
def j():
    return i()
k = j()
l = ''
for _ in range(5):
    for __ in range(4):
                l += k
if l == '5':
    m = l + ' c1'
elif l == '17':
    m = l + ' c2'
else:
    m = l + ' c3'
if m == '6':
    n = m + ' c1'
elif m == '14':
    n = m + ' c2'
else:
    n = m + ' c3'
o_list = [n for _ in range(10)]
p_list = [o_list for _ in range(5)]
q_list = [p_list for _ in range(8)]
r = random.choice(q_list)
s = random.choice(r)
t = random.choice(s)
u_dict = {93: t, 13: t, 51: t, 3: t, 90: t, 5: t, 85: t}
v_dict = {96: u_dict, 100: u_dict, 63: u_dict, 53: u_dict}
w = random.choice(list(v_dict.values()))
x = random.choice(list(w.values()))
y = x[0:]
z = y + '2'
aa = (z, z, z)
ab, ac, ad = aa
ae = ab + ac + ad
af_list = [ae for _ in range(8)]
ag = random.choice(af_list)
ah = ag[0:]
ai_list = [ah for _ in range(3)]
aj_list = [ai_list for _ in range(3)]
ak_list = [aj_list for _ in range(3)]
al = random.choice(ak_list)
am = random.choice(al)
an = random.choice(am)
if an == '3':
    ao = an + ' c1'
elif an == '18':
    ao = an + ' c2'
else:
    ao = an + ' c3'
ap = ao + '.'
def aq():
    return ap
def ar():
    return aq()
def at():
    return ar()
au = at()
print(au)