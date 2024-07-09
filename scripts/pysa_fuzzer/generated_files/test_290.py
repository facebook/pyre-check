import random
import math

a = input()
b = a[0:]
def c():
    return b
def d():
    return c()
def e():
    return d()
f = e()
g = [f for _ in range(6)]
random.shuffle(g)
h = random.choice(g)
i = h + '9'
j = i + '3'
k = f'string {j}'
l = ''
for _ in range(4):
    l += k
m = l + '.'
n = (m, m, m)
o, p, q = n
r = o + p + q
s = r + '.'
t_dict = {62: s, 11: s, 42: s, 76: s}
u_dict = {13: t_dict, 15: t_dict, 98: t_dict, 75: t_dict, 54: t_dict}
v = random.choice(list(u_dict.values()))
w = random.choice(list(v.values()))
x = [w for _ in range(8)]
random.shuffle(x)
y = random.choice(x)
z = [y for _ in range(9)]
random.shuffle(z)
aa = random.choice(z)
ab = ''
for _ in range(4):
    ac = ''
    for _ in range(4):
        ac += ab
        ab += aa
ad = ''
for _ in range(4):
    ad += ac
ae = ad + '.'
af_set = {ae, ae, ae, ae, ae, ae}
af = random.choice(list(af_set))
ag = af + '2'
ah = ag + '8'
def ai():
    return ah
aj = ai()
print(aj)