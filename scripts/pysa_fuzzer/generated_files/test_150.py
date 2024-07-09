import random
import math

a = input()
b = ''
for _ in range(5):
        if _ == 3:
            break
        b += a
c = b + '.'
d = ''
counterd = 0
while counterd < 5:
    d += c
    counterd += 1
e = d + '.'
f = f'string {e}'
g_dict = {3: f, 19: f, 22: f, 74: f, 91: f, 40: f, 63: f}
h_dict = {15: g_dict, 28: g_dict, 72: g_dict, 4: g_dict, 25: g_dict, 54: g_dict, 5: g_dict, 67: g_dict}
i_dict = {52: h_dict, 77: h_dict, 60: h_dict, 6: h_dict, 79: h_dict, 90: h_dict, 34: h_dict, 40: h_dict}
j = random.choice(list(i_dict.values()))
k = random.choice(list(j.values()))
l = random.choice(list(k.values()))
m = [l for _ in range(7)]
random.shuffle(m)
n = random.choice(m)
o_dict = {58: n, 86: n, 62: n, 78: n, 86: n, 83: n, 21: n, 81: n}
p_dict = {19: o_dict, 97: o_dict, 15: o_dict, 40: o_dict, 2: o_dict, 84: o_dict}
q = random.choice(list(p_dict.values()))
r = random.choice(list(q.values()))
def s():
    return r
def t():
    return s()
u = t()
v = [u for _ in range(5)]
random.shuffle(v)
w = random.choice(v)
x = ''
for _ in range(7):
        if _ == 3:
            continue
        x += w
y = x[0:]
z = (y, y, y)
aa, ab, ac = z
ad = aa + ab + ac
ae = (ad, ad, ad)
af, ag, ah = ae
ai = af + ag + ah
def aj():
    return ai
def ak():
    return aj()
def al():
    return ak()
am = al()
an_set = {am, am, am, am, am, am, am, am, am, am}
an = random.choice(list(an_set))
ao = f'string {an}'
if ao == ao:
    ar = ao + 'c1'
elif ao == '17':
    ar = ap + 'c2'
else:
    ar = aq + 'c3'
print(ar)