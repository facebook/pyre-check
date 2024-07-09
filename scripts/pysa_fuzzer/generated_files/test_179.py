import random
import math

a = input()
b = [a for _ in range(6)]
random.shuffle(b)
c = random.choice(b)
d = ''
for _ in range(6):
        if _ == 3:
            break
        d += c
e = ''
countere = 0
while countere < 3:
    f = ''
    counterf = 0
    while counterf < 4:
        g = ''
        counterg = 0
        while counterg < 3:
            g += f
            counterg += 1
            f += e
            counterf += 1
        e += d
        countere += 1
h = g + '2'
i = h + '8'
j = i + '.'
def k():
    return j
def l():
    return k()
m = l()
n = [m for _ in range(10)]
random.shuffle(n)
o = random.choice(n)
p_set = {o, o, o, o}
p = random.choice(list(p_set))
q_set = {p, p, p, p, p, p, p, p, p, p}
q = random.choice(list(q_set))
r = ''
for _ in range(2):
    for __ in range(5):
                r += q
if r == r:
    u = r + 'c1'
elif r == '12':
    u = s + 'c2'
else:
    u = t + 'c3'
v = [u for _ in range(8)]
random.shuffle(v)
w = random.choice(v)
x_set = {w, w, w, w, w, w, w, w}
x = random.choice(list(x_set))
def y():
    return x
z = y()
aa = [z for _ in range(7)]
random.shuffle(aa)
ab = random.choice(aa)
ac = ab[0:]
if ac == ac:
    af = ac + 'c1'
elif ac == '16':
    af = ad + 'c2'
else:
    af = ae + 'c3'
ag_dict = {36: af, 81: af, 61: af, 41: af, 15: af, 94: af, 75: af, 39: af}
ah_dict = {74: ag_dict, 32: ag_dict}
ai_dict = {13: ah_dict, 20: ah_dict, 33: ah_dict, 96: ah_dict, 48: ah_dict, 3: ah_dict, 53: ah_dict, 51: ah_dict}
aj = random.choice(list(ai_dict.values()))
ak = random.choice(list(aj.values()))
al = random.choice(list(ak.values()))
print(al)