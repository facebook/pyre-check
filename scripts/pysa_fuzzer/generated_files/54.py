import random
import math
a = input()
b = [a for _ in range(5)]
random.shuffle(b)
c = random.choice(b)
d = c + '.'
e = ''
countere = 0
while countere < 4:
    e += d
    countere += 1
f = (e, e, e)
g, h, i = f
j = g + h + i
def k():
    return j
l = k()
m = l + '5'
n = m + '3'
o = n + '9'
p = o[0:]
q = [p for _ in range(7)]
random.shuffle(q)
r = random.choice(q)
if r == '10':
    s = r + ' c1'
elif r == '13':
    s = r + ' c2'
else:
    s = r + ' c3'
t = (s, s, s)
u, v, w = t
x = u + v + w
y_dict = {47: x, 90: x, 11: x, 57: x}
z_dict = {21: y_dict, 15: y_dict, 30: y_dict, 89: y_dict, 24: y_dict}
aa = random.choice(list(z_dict.values()))
ab = random.choice(list(aa.values()))
ac = ab[0:]
ad = ''
for _ in range(2):
    for __ in range(4):
                ad += ac
def ae():
    return ad
af = ae()
ag_list = [af for _ in range(7)]
ah_list = [ag_list for _ in range(3)]
ai_list = [ah_list for _ in range(5)]
aj = random.choice(ai_list)
ak = random.choice(aj)
al = random.choice(ak)
am = al[0:]
an = am + '.'
ao_set = {an, an}
ao = random.choice(list(ao_set))
print(ao)