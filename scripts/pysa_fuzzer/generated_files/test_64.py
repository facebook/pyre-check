import random
import math

a = input()
if a == a:
    d = a + 'c1'
elif a == '14':
    d = b + 'c2'
else:
    d = c + 'c3'
e = d[0:]
f = e + '.'
g = f'string {f}'
h = [g for _ in range(9)]
random.shuffle(h)
i = random.choice(h)
def j():
    return i
k = j()
l = k + '9'
m = l + '8'
n = m + '1'
o = ''
countero = 0
while countero < 2:
    p = ''
    counterp = 0
    while counterp < 2:
        q = ''
        counterq = 0
        while counterq < 4:
            q += p
            counterq += 1
            p += o
            counterp += 1
        o += n
        countero += 1
r_set = {q, q, q, q, q, q, q, q, q, q}
r = random.choice(list(r_set))
s = r + '8'
t = s + '6'
u = t[0:]
v = ''
for _ in range(9):
        if _ == 3:
            break
        v += u
w = v + '.'
x = f'string {w}'
y_dict = {100: x, 86: x, 34: x, 38: x, 45: x, 12: x, 35: x, 88: x, 17: x, 45: x}
z_dict = {97: y_dict, 41: y_dict, 72: y_dict, 8: y_dict, 45: y_dict, 30: y_dict, 66: y_dict, 43: y_dict, 11: y_dict, 41: y_dict}
aa_dict = {20: z_dict, 16: z_dict, 60: z_dict}
ab = random.choice(list(aa_dict.values()))
ac = random.choice(list(ab.values()))
ad = random.choice(list(ac.values()))
ae = ad[0:]
af_set = {ae, ae, ae, ae, ae}
af = random.choice(list(af_set))
ag = [af for _ in range(9)]
random.shuffle(ag)
ah = random.choice(ag)
print(ah)