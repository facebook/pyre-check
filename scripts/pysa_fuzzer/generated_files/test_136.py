import random
import math

a = input()
def b():
    return a
def c():
    return b()
def d():
    return c()
e = d()
f = e + '.'
g_list = [f for _ in range(7)]
h = random.choice(g_list)
i_list = [h for _ in range(7)]
j = random.choice(i_list)
k = ''
counterk = 0
while counterk < 5:
    l = ''
    counterl = 0
    while counterl < 5:
        m = ''
        counterm = 0
        while counterm < 4:
            m += l
            counterm += 1
            l += k
            counterl += 1
        k += j
        counterk += 1
n = m + '8'
o = n + '8'
p = o + '5'
q = (p, p, p)
r, s, t = q
u = r + s + t
v = u + '.'
w_set = {v, v, v, v}
w = random.choice(list(w_set))
x_set = {w, w, w, w, w}
x = random.choice(list(x_set))
y = ''
for _ in range(5):
    for __ in range(3):
                y += x
z_list = [y for _ in range(6)]
aa = random.choice(z_list)
ab_set = {aa, aa, aa, aa, aa, aa, aa}
ab = random.choice(list(ab_set))
ac = ab + '2'
ad = f'string {ac}'
ae_set = {ad, ad, ad, ad}
ae = random.choice(list(ae_set))
if ae == ae:
    ah = ae + 'c1'
elif ae == '12':
    ah = af + 'c2'
else:
    ah = ag + 'c3'
ai = ah + '.'
print(ai)