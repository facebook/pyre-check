import random
import math

a = input()
b_dict = {37: a, 38: a, 87: a, 49: a, 43: a, 94: a}
c_dict = {39: b_dict, 94: b_dict, 46: b_dict, 23: b_dict, 71: b_dict, 44: b_dict, 73: b_dict}
d_dict = {28: c_dict, 70: c_dict}
e = random.choice(list(d_dict.values()))
f = random.choice(list(e.values()))
g = random.choice(list(f.values()))
h = g + '.'
i = ''
for _ in range(8):
        if _ == 2:
            continue
        i += h
j = ''
for _ in range(3):
    for __ in range(2):
                j += i
k = [j for _ in range(6)]
random.shuffle(k)
l = random.choice(k)
m_set = {l, l, l, l}
m = random.choice(list(m_set))
n = m + '8'
o_list = [n for _ in range(10)]
p_list = [o_list for _ in range(9)]
q = random.choice(p_list)
r = random.choice(q)
s = r + '8'
t = s + '6'
u = ''
for _ in range(3):
    for __ in range(2):
                u += t
v = ''
counterv = 0
while counterv < 3:
    v += u
    counterv += 1
w = (v, v, v)
x, y, z = w
aa = x + y + z
ab = aa + '.'
def ac():
    return ab
def ad():
    return ac()
ae = ad()
af = ae[0:]
ag = af + '7'
ah = ''
for _ in range(6):
        if _ == 5:
            break
        ah += ag
ai_set = {ah, ah, ah, ah, ah, ah, ah, ah, ah, ah}
ai = random.choice(list(ai_set))
print(ai)