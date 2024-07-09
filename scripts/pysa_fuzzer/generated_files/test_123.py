import random
import math

a = input()
b = f'string {a}'
c_set = {b, b, b, b, b, b, b, b}
c = random.choice(list(c_set))
d = ''
for _ in range(5):
        if _ == 2:
            break
        d += c
e = f'string {d}'
def f():
    return e
def g():
    return f()
def h():
    return g()
i = h()
j = ''
counterj = 0
while counterj < 2:
    k = ''
    counterk = 0
    while counterk < 3:
        l = ''
        counterl = 0
        while counterl < 4:
            l += k
            counterl += 1
            k += j
            counterk += 1
        j += i
        counterj += 1
m = l + '2'
n = ''
countern = 0
while countern < 2:
    o = ''
    countero = 0
    while countero < 2:
        o += n
        countero += 1
        n += m
        countern += 1
if o == o:
    r = o + 'c1'
elif o == '12':
    r = p + 'c2'
else:
    r = q + 'c3'
s = (r, r, r)
t, u, v = s
w = t + u + v
x = [w for _ in range(10)]
random.shuffle(x)
y = random.choice(x)
z = y + '1'
aa = ''
counteraa = 0
while counteraa < 3:
    ab = ''
    counterab = 0
    while counterab < 2:
        ab += aa
        counterab += 1
        aa += z
        counteraa += 1
ac = [ab for _ in range(10)]
random.shuffle(ac)
ad = random.choice(ac)
ae = ad + '8'
af = ae + '7'
ag = af + '4'
ah = f'string {ag}'
ai = ''
counterai = 0
while counterai < 3:
    aj = ''
    counteraj = 0
    while counteraj < 3:
        ak = ''
        counterak = 0
        while counterak < 2:
            ak += aj
            counterak += 1
            aj += ai
            counteraj += 1
        ai += ah
        counterai += 1
al = f'string {ak}'
print(al)