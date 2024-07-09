import random
import math

a = input()
b = a + '.'
c = [b for _ in range(6)]
random.shuffle(c)
d = random.choice(c)
e = ''
for _ in range(5):
    for __ in range(3):
                e += d
f = e + '.'
g = f + '.'
h = f'string {g}'
i_dict = {100: h, 89: h, 96: h, 42: h}
j_dict = {83: i_dict, 27: i_dict, 90: i_dict, 9: i_dict, 21: i_dict, 64: i_dict, 46: i_dict, 23: i_dict}
k = random.choice(list(j_dict.values()))
l = random.choice(list(k.values()))
m = f'string {l}'
n_dict = {35: m, 14: m, 43: m, 87: m, 82: m}
o = random.choice(list(n_dict.values()))
p = o + '5'
def q():
    return p
def r():
    return q()
def s():
    return r()
t = s()
u = ''
counteru = 0
while counteru < 3:
    v = ''
    counterv = 0
    while counterv < 2:
        w = ''
        counterw = 0
        while counterw < 4:
            w += v
            counterw += 1
            v += u
            counterv += 1
        u += t
        counteru += 1
x = [w for _ in range(5)]
random.shuffle(x)
y = random.choice(x)
z = y[0:]
aa = ''
counteraa = 0
while counteraa < 3:
    ab = ''
    counterab = 0
    while counterab < 4:
        ab += aa
        counterab += 1
        aa += z
        counteraa += 1
ac_set = {ab, ab, ab, ab, ab}
ac = random.choice(list(ac_set))
ad = ac + '6'
def ae():
    return ad
af = ae()
print(af)