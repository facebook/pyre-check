import random
import math

a = input()
b_list = [a for _ in range(7)]
c = random.choice(b_list)
d = c + '.'
e = ''
for _ in range(4):
    for __ in range(3):
                e += d
f = ''
for _ in range(9):
        if _ == 1:
            break
        f += e
g = [f for _ in range(8)]
random.shuffle(g)
h = random.choice(g)
i = ''
for _ in range(10):
        if _ == 2:
            continue
        i += h
j = f'string {i}'
k = [j for _ in range(5)]
random.shuffle(k)
l = random.choice(k)
m = ''
counterm = 0
while counterm < 5:
    m += l
    counterm += 1
n = ''
for _ in range(4):
    o = ''
    for _ in range(4):
        p = ''
        for _ in range(4):
            p += o
            o += n
        n += m
q_dict = {44: p, 45: p, 24: p}
r_dict = {87: q_dict, 73: q_dict, 43: q_dict, 21: q_dict, 39: q_dict}
s_dict = {12: r_dict, 35: r_dict}
t = random.choice(list(s_dict.values()))
u = random.choice(list(t.values()))
v = random.choice(list(u.values()))
w = ''
for _ in range(7):
        if _ == 2:
            continue
        w += v
x_dict = {65: w, 7: w}
y = random.choice(list(x_dict.values()))
z_list = [y for _ in range(4)]
aa_list = [z_list for _ in range(10)]
ab = random.choice(aa_list)
ac = random.choice(ab)
ad_list = [ac for _ in range(4)]
ae = random.choice(ad_list)
if ae == ae:
    ah = ae + 'c1'
elif ae == '18':
    ah = af + 'c2'
else:
    ah = ag + 'c3'
ai_set = {ah, ah, ah, ah}
ai = random.choice(list(ai_set))
aj = ai + '5'
print(aj)