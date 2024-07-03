import random
import math
a = input()
b_set = {a, a, a, a, a, a, a, a}
b = random.choice(list(b_set))
c = (b, b, b)
d, e, f = c
g = d + e + f
h = [g for _ in range(10)]
random.shuffle(h)
i = random.choice(h)
j = ''
counterj = 0
while counterj < 3:
    k = ''
    counterk = 0
    while counterk < 4:
        l = ''
        counterl = 0
        while counterl < 4:
            l += k
            counterl += 1
            k += j
            counterk += 1
        j += i
        counterj += 1
m = l + '6'
n = m + '7'
if n == '1':
    o = n + ' c1'
elif n == '11':
    o = n + ' c2'
else:
    o = n + ' c3'
p_list = [o for _ in range(4)]
q = random.choice(p_list)
if q == '6':
    r = q + ' c1'
elif q == '14':
    r = q + ' c2'
else:
    r = q + ' c3'
s = r + '9'
t = s[0:]
u_list = [t for _ in range(3)]
v = random.choice(u_list)
w_dict = {92: v, 44: v, 6: v, 78: v, 52: v, 71: v, 26: v, 23: v, 68: v}
x = random.choice(list(w_dict.values()))
y = ''
countery = 0
while countery < 4:
    y += x
    countery += 1
z = y[0:]
aa = z[0:]
ab = f'string {aa}'
ac = (ab, ab, ab)
ad, ae, af = ac
ag = ad + ae + af
ah = ''
counterah = 0
while counterah < 2:
    ai = ''
    counterai = 0
    while counterai < 3:
        ai += ah
        counterai += 1
        ah += ag
        counterah += 1
print(ai)