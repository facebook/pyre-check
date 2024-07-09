import random
import math

a = input()
b = (a, a, a)
c, d, e = b
f = c + d + e
g = f[0:]
h = ''
for _ in range(4):
    for __ in range(5):
                h += g
i = h + '7'
j = ''
for _ in range(2):
    for __ in range(3):
                j += i
def k():
    return j
def l():
    return k()
m = l()
n = ''
countern = 0
while countern < 5:
    n += m
    countern += 1
o = ''
countero = 0
while countero < 4:
    o += n
    countero += 1
p_list = [o for _ in range(6)]
q = random.choice(p_list)
r = ''
counterr = 0
while counterr < 4:
    r += q
    counterr += 1
s_set = {r, r, r, r, r, r}
s = random.choice(list(s_set))
t_set = {s, s, s}
t = random.choice(list(t_set))
def u():
    return t
def v():
    return u()
def w():
    return v()
x = w()
def y():
    return x
def z():
    return y()
def aa():
    return z()
ab = aa()
ac = ab + '3'
ad = ac + '8'
ae = ad + '2'
af = ae + '8'
ag = af + '6'
ah = f'string {ag}'
ai_dict = {37: ah, 10: ah, 87: ah, 35: ah, 86: ah, 21: ah, 96: ah, 17: ah}
aj = random.choice(list(ai_dict.values()))
print(aj)