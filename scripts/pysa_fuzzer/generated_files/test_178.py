import random
import math

a = input()
b_list = [a for _ in range(3)]
c_list = [b_list for _ in range(2)]
d_list = [c_list for _ in range(9)]
e = random.choice(d_list)
f = random.choice(e)
g = random.choice(f)
h = f'string {g}'
def i():
    return h
j = i()
def k():
    return j
def l():
    return k()
m = l()
n = f'string {m}'
o = n + '.'
def p():
    return o
def q():
    return p()
r = q()
s = r + '1'
t = s + '1'
u = t + '6'
v = u + '.'
w = ''
for _ in range(4):
    x = ''
    for _ in range(5):
        x += w
        w += v
y = x + '1'
z = y + '2'
aa_list = [z for _ in range(8)]
ab_list = [aa_list for _ in range(5)]
ac = random.choice(ab_list)
ad = random.choice(ac)
if ad == ad:
    ag = ad + 'c1'
elif ad == '15':
    ag = ae + 'c2'
else:
    ag = af + 'c3'
ah = f'string {ag}'
ai = ah[0:]
if ai == ai:
    al = ai + 'c1'
elif ai == '15':
    al = aj + 'c2'
else:
    al = ak + 'c3'
am = [al for _ in range(8)]
random.shuffle(am)
an = random.choice(am)
ao = an + '.'
print(ao)