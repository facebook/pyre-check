import random
import math
a = input()
b = ''
for _ in range(5):
    b += a
c = f'string {b}'
d = (c, c, c)
e, f, g = d
h = e + f + g
i_set = {h, h, h, h, h, h, h, h}
i = random.choice(list(i_set))
j = i + '9'
k = j + '8'
l = k + '8'
m = l + '4'
n = ''
for _ in range(5):
    for __ in range(3):
                n += m
o = ''
for _ in range(10):
        if _ == 5:
            break
        o += n
p = ''
for _ in range(4):
    for __ in range(5):
                p += o
q_dict = {34: p, 57: p, 52: p, 66: p, 1: p, 54: p, 20: p, 4: p, 63: p, 66: p}
r_dict = {14: q_dict, 48: q_dict, 15: q_dict}
s = random.choice(list(r_dict.values()))
t = random.choice(list(s.values()))
u_dict = {33: t, 62: t, 58: t, 58: t, 15: t, 65: t}
v_dict = {95: u_dict, 1: u_dict, 41: u_dict, 2: u_dict, 52: u_dict, 34: u_dict, 84: u_dict}
w = random.choice(list(v_dict.values()))
x = random.choice(list(w.values()))
y = ''
for _ in range(4):
    for __ in range(3):
                y += x
z = f'string {y}'
aa = z + '7'
ab = aa + '2'
def ac():
    return ab
def ad():
    return ac()
def ae():
    return ad()
af = ae()
ag = [af for _ in range(8)]
random.shuffle(ag)
ah = random.choice(ag)
def ai():
    return ah
def aj():
    return ai()
def ak():
    return aj()
al = ak()
am = al + '4'
print(am)