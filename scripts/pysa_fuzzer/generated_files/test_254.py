import random
import math

a = input()
b = [a for _ in range(8)]
random.shuffle(b)
c = random.choice(b)
d = ''
for _ in range(4):
    e = ''
    for _ in range(2):
        e += d
        d += c
f = e + '.'
g = ''
for _ in range(9):
        if _ == 3:
            break
        g += f
h = g + '3'
i = h + '3'
j = i + '7'
if j == j:
    m = j + 'c1'
elif j == '15':
    m = k + 'c2'
else:
    m = l + 'c3'
n = f'string {m}'
o_dict = {61: n, 27: n, 94: n, 82: n, 84: n, 48: n, 85: n, 37: n, 23: n}
p_dict = {87: o_dict, 91: o_dict}
q_dict = {38: p_dict, 4: p_dict, 65: p_dict, 44: p_dict, 55: p_dict}
r = random.choice(list(q_dict.values()))
s = random.choice(list(r.values()))
t = random.choice(list(s.values()))
u = t + '2'
v = u + '9'
w = [v for _ in range(8)]
random.shuffle(w)
x = random.choice(w)
y = ''
countery = 0
while countery < 2:
    y += x
    countery += 1
def z():
    return y
def aa():
    return z()
ab = aa()
ac = ab[0:]
ad = ac[0:]
ae_list = [ad for _ in range(2)]
af_list = [ae_list for _ in range(10)]
ag_list = [af_list for _ in range(10)]
ah = random.choice(ag_list)
ai = random.choice(ah)
aj = random.choice(ai)
def ak():
    return aj
al = ak()
am_set = {al, al, al, al, al}
am = random.choice(list(am_set))
an = am + '.'
print(an)