import random
import math

a = input()
b = ''
for _ in range(4):
    for __ in range(5):
                b += a
c = b + '3'
d = c + '3'
e = ''
for _ in range(4):
    e += d
f = [e for _ in range(8)]
random.shuffle(f)
g = random.choice(f)
h = ''
for _ in range(6):
        if _ == 4:
            continue
        h += g
i_set = {h, h, h, h, h, h, h}
i = random.choice(list(i_set))
j = (i, i, i)
k, l, m = j
n = k + l + m
o_list = [n for _ in range(4)]
p_list = [o_list for _ in range(2)]
q = random.choice(p_list)
r = random.choice(q)
s = r + '7'
t = s + '5'
u = t + '9'
v = u[0:]
w = [v for _ in range(6)]
random.shuffle(w)
x = random.choice(w)
y = ''
for _ in range(5):
    for __ in range(2):
                y += x
z = ''
counterz = 0
while counterz < 4:
    z += y
    counterz += 1
aa_list = [z for _ in range(8)]
ab = random.choice(aa_list)
ac = ''
for _ in range(9):
        if _ == 4:
            break
        ac += ab
ad = [ac for _ in range(8)]
random.shuffle(ad)
ae = random.choice(ad)
af_set = {ae, ae, ae, ae, ae, ae, ae, ae, ae}
af = random.choice(list(af_set))
ag_dict = {90: af, 92: af, 54: af, 78: af, 49: af, 55: af, 19: af, 21: af, 35: af}
ah_dict = {14: ag_dict, 22: ag_dict, 75: ag_dict, 42: ag_dict, 19: ag_dict}
ai = random.choice(list(ah_dict.values()))
aj = random.choice(list(ai.values()))
print(aj)