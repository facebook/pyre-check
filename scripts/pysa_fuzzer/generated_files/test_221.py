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
def f():
    return e
def g():
    return f()
h = g()
i_set = {h, h, h, h, h}
i = random.choice(list(i_set))
j = ''
for _ in range(9):
        if _ == 3:
            continue
        j += i
k = ''
counterk = 0
while counterk < 3:
    l = ''
    counterl = 0
    while counterl < 5:
        m = ''
        counterm = 0
        while counterm < 5:
            m += l
            counterm += 1
            l += k
            counterl += 1
        k += j
        counterk += 1
n = [m for _ in range(10)]
random.shuffle(n)
o = random.choice(n)
p = ''
for _ in range(4):
    for __ in range(2):
                p += o
q_list = [p for _ in range(10)]
r = random.choice(q_list)
s_list = [r for _ in range(5)]
t_list = [s_list for _ in range(10)]
u = random.choice(t_list)
v = random.choice(u)
w_list = [v for _ in range(10)]
x = random.choice(w_list)
y = [x for _ in range(7)]
random.shuffle(y)
z = random.choice(y)
aa_set = {z, z, z}
aa = random.choice(list(aa_set))
ab = aa + '.'
ac = ''
for _ in range(5):
        if _ == 4:
            continue
        ac += ab
def ad():
    return ac
def ae():
    return ad()
af = ae()
ag = af + '.'
ah_list = [ag for _ in range(10)]
ai = random.choice(ah_list)
aj_set = {ai, ai, ai}
aj = random.choice(list(aj_set))
print(aj)