import random
import math

a = input()
b = ''
counterb = 0
while counterb < 4:
    c = ''
    counterc = 0
    while counterc < 3:
        d = ''
        counterd = 0
        while counterd < 5:
            d += c
            counterd += 1
            c += b
            counterc += 1
        b += a
        counterb += 1
e = [d for _ in range(7)]
random.shuffle(e)
f = random.choice(e)
if f == f:
    i = f + 'c1'
elif f == '19':
    i = g + 'c2'
else:
    i = h + 'c3'
j = ''
for _ in range(6):
        if _ == 2:
            break
        j += i
k = ''
counterk = 0
while counterk < 5:
    l = ''
    counterl = 0
    while counterl < 4:
        l += k
        counterl += 1
        k += j
        counterk += 1
def m():
    return l
n = m()
o_dict = {83: n, 59: n, 40: n, 59: n, 5: n, 81: n, 41: n, 6: n, 67: n}
p_dict = {55: o_dict, 15: o_dict, 61: o_dict, 80: o_dict, 59: o_dict, 68: o_dict}
q = random.choice(list(p_dict.values()))
r = random.choice(list(q.values()))
s = f'string {r}'
t_set = {s, s, s, s, s, s}
t = random.choice(list(t_set))
u = f'string {t}'
v = f'string {u}'
w = v + '1'
x = w + '9'
y_dict = {87: x, 97: x, 47: x}
z_dict = {88: y_dict, 7: y_dict, 27: y_dict, 12: y_dict, 64: y_dict}
aa = random.choice(list(z_dict.values()))
ab = random.choice(list(aa.values()))
ac = ab[0:]
if ac == ac:
    af = ac + 'c1'
elif ac == '20':
    af = ad + 'c2'
else:
    af = ae + 'c3'
ag = af + '1'
ah = ag + '6'
ai_list = [ah for _ in range(7)]
aj = random.choice(ai_list)
ak = aj + '9'
print(ak)