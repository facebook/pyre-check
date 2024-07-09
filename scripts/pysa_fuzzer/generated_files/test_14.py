import random
import math

a = input()
b = ''
for _ in range(5):
    for __ in range(3):
                b += a
c = (b, b, b)
d, e, f = c
g = d + e + f
h = g + '8'
i = h + '4'
j = ''
counterj = 0
while counterj < 3:
    k = ''
    counterk = 0
    while counterk < 3:
        k += j
        counterk += 1
        j += i
        counterj += 1
l = k + '2'
m = l + '3'
n = m + '2'
o_dict = {16: n, 56: n, 99: n}
p_dict = {84: o_dict, 81: o_dict}
q = random.choice(list(p_dict.values()))
r = random.choice(list(q.values()))
s = f'string {r}'
t_list = [s for _ in range(8)]
u = random.choice(t_list)
def v():
    return u
w = v()
def x():
    return w
def y():
    return x()
def z():
    return y()
aa = z()
if aa == aa:
    ad = aa + 'c1'
elif aa == '12':
    ad = ab + 'c2'
else:
    ad = ac + 'c3'
def ae():
    return ad
def af():
    return ae()
def ag():
    return af()
ah = ag()
ai = ah + '6'
if ai == ai:
    al = ai + 'c1'
elif ai == '11':
    al = aj + 'c2'
else:
    al = ak + 'c3'
am = ''
for _ in range(2):
    an = ''
    for _ in range(3):
        an += am
        am += al
ao = ''
counterao = 0
while counterao < 3:
    ap = ''
    counterap = 0
    while counterap < 5:
        aq = ''
        counteraq = 0
        while counteraq < 3:
            aq += ap
            counteraq += 1
            ap += ao
            counterap += 1
        ao += an
        counterao += 1
ar = aq + '2'
def at():
    return ar
def au():
    return at()
av = au()
print(av)