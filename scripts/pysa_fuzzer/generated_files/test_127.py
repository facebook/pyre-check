import random
import math

a = input()
b = [a for _ in range(8)]
random.shuffle(b)
c = random.choice(b)
d_set = {c, c, c, c, c, c}
d = random.choice(list(d_set))
e = (d, d, d)
f, g, h = e
i = f + g + h
j = [i for _ in range(10)]
random.shuffle(j)
k = random.choice(j)
def l():
    return k
m = l()
n_dict = {62: m, 24: m, 34: m, 60: m, 26: m, 85: m, 65: m}
o = random.choice(list(n_dict.values()))
p_set = {o, o, o}
p = random.choice(list(p_set))
q = ''
for _ in range(2):
    for __ in range(3):
                q += p
r = q + '.'
s_list = [r for _ in range(5)]
t_list = [s_list for _ in range(3)]
u = random.choice(t_list)
v = random.choice(u)
w = v + '9'
x = w + '6'
y = ''
for _ in range(5):
    for __ in range(2):
                y += x
z = ''
for _ in range(10):
        if _ == 2:
            continue
        z += y
aa = z + '.'
ab_dict = {71: aa, 71: aa, 42: aa, 28: aa, 98: aa, 81: aa, 13: aa, 86: aa}
ac = random.choice(list(ab_dict.values()))
ad = (ac, ac, ac)
ae, af, ag = ad
ah = ae + af + ag
def ai():
    return ah
def aj():
    return ai()
ak = aj()
if ak == ak:
    an = ak + 'c1'
elif ak == '13':
    an = al + 'c2'
else:
    an = am + 'c3'
print(an)