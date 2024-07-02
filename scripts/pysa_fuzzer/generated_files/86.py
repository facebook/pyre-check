import random
import math
a = input()
b = a + '9'
c = b + '9'
d = c + '9'
e_dict = {2: d, 91: d, 67: d, 8: d, 54: d, 86: d, 40: d, 20: d, 73: d}
f = random.choice(list(e_dict.values()))
def g():
    return f
def h():
    return g()
def i():
    return h()
j = i()
k = j[0:]
l = ''
for _ in range(2):
    m = ''
    for _ in range(5):
        m += l
        l += k
n = (m, m, m)
o, p, q = n
r = o + p + q
if r == '7':
    s = r + ' c1'
elif r == '18':
    s = r + ' c2'
else:
    s = r + ' c3'
t = s + '.'
u = t + '.'
v_list = [u for _ in range(3)]
w_list = [v_list for _ in range(8)]
x = random.choice(w_list)
y = random.choice(x)
z = ''
for _ in range(5):
        if _ == 2:
            break
        z += y
if z == '1':
    aa = z + ' c1'
elif z == '16':
    aa = z + ' c2'
else:
    aa = z + ' c3'
ab = ''
for _ in range(5):
    ac = ''
    for _ in range(3):
        ad = ''
        for _ in range(4):
            ad += ac
            ac += ab
        ab += aa
ae = (ad, ad, ad)
af, ag, ah = ae
ai = af + ag + ah
aj = [ai for _ in range(7)]
random.shuffle(aj)
ak = random.choice(aj)
al = f'string {ak}'
am = (al, al, al)
an, ao, ap = am
aq = an + ao + ap
ar = aq + '.'
print(ar)