import random
import math

a = input()
b_list = [a for _ in range(2)]
c = random.choice(b_list)
d = c[0:]
e = ''
countere = 0
while countere < 3:
    f = ''
    counterf = 0
    while counterf < 2:
        f += e
        counterf += 1
        e += d
        countere += 1
g_list = [f for _ in range(5)]
h_list = [g_list for _ in range(4)]
i = random.choice(h_list)
j = random.choice(i)
k = j + '7'
l = k + '3'
m = l + '6'
n_list = [m for _ in range(2)]
o = random.choice(n_list)
p = o + '8'
q = p + '2'
r = q + '4'
s = r + '.'
t = s + '.'
u_set = {t, t, t, t}
u = random.choice(list(u_set))
v = (u, u, u)
w, x, y = v
z = w + x + y
aa_dict = {78: z, 79: z, 100: z, 88: z}
ab_dict = {85: aa_dict, 72: aa_dict, 51: aa_dict, 20: aa_dict}
ac = random.choice(list(ab_dict.values()))
ad = random.choice(list(ac.values()))
ae = ad[0:]
af = [ae for _ in range(6)]
random.shuffle(af)
ag = random.choice(af)
ah_set = {ag, ag, ag, ag, ag, ag, ag, ag, ag}
ah = random.choice(list(ah_set))
ai = ah[0:]
aj = ''
for _ in range(5):
    for __ in range(3):
                aj += ai
print(aj)