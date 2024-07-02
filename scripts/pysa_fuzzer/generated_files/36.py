import random
import math
a = input()
b = a + '9'
c = b[0:]
d = c + '.'
e = d[0:]
f_set = {e, e, e, e, e, e, e, e, e}
f = random.choice(list(f_set))
g = (f, f, f)
h, i, j = g
k = h + i + j
l = k + '.'
def m():
    return l
n = m()
o = n + '1'
p = o + '3'
q = f'string {p}'
def r():
    return q
s = r()
def t():
    return s
def u():
    return t()
v = u()
w_dict = {44: v, 83: v, 7: v, 85: v, 25: v, 61: v, 85: v, 60: v}
x_dict = {57: w_dict, 1: w_dict, 36: w_dict, 32: w_dict}
y_dict = {88: x_dict, 32: x_dict, 42: x_dict}
z = random.choice(list(y_dict.values()))
aa = random.choice(list(z.values()))
ab = random.choice(list(aa.values()))
ac = f'string {ab}'
ad = ''
for _ in range(6):
        if _ == 2:
            break
        ad += ac
ae = [ad for _ in range(9)]
random.shuffle(ae)
af = random.choice(ae)
ag = f'string {af}'
ah_dict = {43: ag, 40: ag, 97: ag, 92: ag, 19: ag, 96: ag}
ai_dict = {52: ah_dict, 65: ah_dict, 79: ah_dict, 48: ah_dict, 87: ah_dict}
aj = random.choice(list(ai_dict.values()))
ak = random.choice(list(aj.values()))
print(ak)