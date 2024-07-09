import random
import math

a = input()
b = (a, a, a)
c, d, e = b
f = c + d + e
g_set = {f, f, f, f, f, f, f, f, f}
g = random.choice(list(g_set))
h = [g for _ in range(8)]
random.shuffle(h)
i = random.choice(h)
j_set = {i, i, i, i, i, i, i}
j = random.choice(list(j_set))
def k():
    return j
def l():
    return k()
m = l()
n = m + '.'
o = [n for _ in range(10)]
random.shuffle(o)
p = random.choice(o)
q = ''
for _ in range(10):
        if _ == 5:
            continue
        q += p
r = q + '1'
s = r + '5'
t = s + '2'
u = ''
for _ in range(5):
        if _ == 4:
            continue
        u += t
v = (u, u, u)
w, x, y = v
z = w + x + y
aa = ''
counteraa = 0
while counteraa < 5:
    ab = ''
    counterab = 0
    while counterab < 3:
        ac = ''
        counterac = 0
        while counterac < 3:
            ac += ab
            counterac += 1
            ab += aa
            counterab += 1
        aa += z
        counteraa += 1
ad = ''
for _ in range(7):
        if _ == 1:
            break
        ad += ac
ae_list = [ad for _ in range(4)]
af = random.choice(ae_list)
ag_dict = {20: af, 4: af}
ah_dict = {76: ag_dict, 51: ag_dict, 34: ag_dict, 71: ag_dict, 93: ag_dict}
ai_dict = {95: ah_dict, 68: ah_dict, 45: ah_dict}
aj = random.choice(list(ai_dict.values()))
ak = random.choice(list(aj.values()))
al = random.choice(list(ak.values()))
am = f'string {al}'
an = ''
for _ in range(2):
    for __ in range(5):
                an += am
ao = ''
for _ in range(5):
    ao += an
print(ao)