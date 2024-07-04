import random
import math
a = input()
b = f'string {a}'
c = (b, b, b)
d, e, f = c
g = d + e + f
h = [g for _ in range(8)]
random.shuffle(h)
i = random.choice(h)
def j():
    return i
k = j()
l_set = {k, k, k, k, k, k, k, k}
l = random.choice(list(l_set))
def m():
    return l
def n():
    return m()
o = n()
p = [o for _ in range(9)]
random.shuffle(p)
q = random.choice(p)
r = q[0:]
s = r[0:]
t_list = [s for _ in range(6)]
u_list = [t_list for _ in range(3)]
v_list = [u_list for _ in range(7)]
w = random.choice(v_list)
x = random.choice(w)
y = random.choice(x)
z = f'string {y}'
aa = ''
for _ in range(5):
    aa += z
def ab():
    return aa
def ac():
    return ab()
def ad():
    return ac()
ae = ad()
af = ae + '.'
ag = ''
for _ in range(8):
        if _ == 2:
            break
        ag += af
ah = ag + '5'
ai = ah + '3'
aj = ai + '4'
ak = aj + '.'
if ak == '3':
    al = ak + ' c1'
elif ak == '12':
    al = ak + ' c2'
else:
    al = ak + ' c3'
print(al)