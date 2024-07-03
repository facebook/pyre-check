import random
import math
a = input()
b = a + '.'
c = b[0:]
d = c + '.'
e = ''
for _ in range(5):
        if _ == 3:
            continue
        e += d
f = f'string {e}'
g = f + '2'
h = g + '3'
i = h + '3'
j = ''
for _ in range(4):
    k = ''
    for _ in range(4):
        l = ''
        for _ in range(3):
            l += k
            k += j
        j += i
m_list = [l for _ in range(6)]
n_list = [m_list for _ in range(9)]
o = random.choice(n_list)
p = random.choice(o)
q = ''
counterq = 0
while counterq < 4:
    q += p
    counterq += 1
r = ''
for _ in range(5):
    s = ''
    for _ in range(5):
        t = ''
        for _ in range(2):
            t += s
            s += r
        r += q
u_dict = {86: t, 18: t, 85: t}
v = random.choice(list(u_dict.values()))
w = v + '1'
x = w + '2'
y = ''
for _ in range(3):
    for __ in range(3):
                y += x
z = [y for _ in range(7)]
random.shuffle(z)
aa = random.choice(z)
ab = ''
for _ in range(5):
    for __ in range(5):
                ab += aa
ac = ab + '9'
ad = ac + '6'
ae_list = [ad for _ in range(4)]
af = random.choice(ae_list)
ag_dict = {51: af, 25: af, 72: af, 35: af, 82: af}
ah_dict = {99: ag_dict, 36: ag_dict, 9: ag_dict, 39: ag_dict, 27: ag_dict, 8: ag_dict, 35: ag_dict, 83: ag_dict, 68: ag_dict, 22: ag_dict}
ai_dict = {2: ah_dict, 83: ah_dict, 32: ah_dict, 93: ah_dict, 100: ah_dict, 84: ah_dict, 53: ah_dict}
aj = random.choice(list(ai_dict.values()))
ak = random.choice(list(aj.values()))
al = random.choice(list(ak.values()))
print(al)