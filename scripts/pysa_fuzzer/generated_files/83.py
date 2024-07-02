import random
import math
a = input()
b = [a for _ in range(5)]
random.shuffle(b)
c = random.choice(b)
if c == '1':
    d = c + ' c1'
elif c == '14':
    d = c + ' c2'
else:
    d = c + ' c3'
e = d + '7'
f = e + '2'
g = [f for _ in range(9)]
random.shuffle(g)
h = random.choice(g)
def i():
    return h
def j():
    return i()
k = j()
l_list = [k for _ in range(6)]
m = random.choice(l_list)
n = ''
countern = 0
while countern < 4:
    o = ''
    countero = 0
    while countero < 5:
        o += n
        countero += 1
        n += m
        countern += 1
p_set = {o, o, o, o, o}
p = random.choice(list(p_set))
q_set = {p, p, p, p, p, p, p, p, p}
q = random.choice(list(q_set))
if q == '7':
    r = q + ' c1'
elif q == '15':
    r = q + ' c2'
else:
    r = q + ' c3'
def s():
    return r
t = s()
u = t + '.'
v = u + '.'
w_dict = {67: v, 82: v, 63: v, 39: v}
x_dict = {11: w_dict, 83: w_dict, 41: w_dict, 48: w_dict, 3: w_dict, 75: w_dict, 21: w_dict, 18: w_dict}
y_dict = {60: x_dict, 98: x_dict}
z = random.choice(list(y_dict.values()))
aa = random.choice(list(z.values()))
ab = random.choice(list(aa.values()))
ac = (ab, ab, ab)
ad, ae, af = ac
ag = ad + ae + af
def ah():
    return ag
def ai():
    return ah()
aj = ai()
if aj == '5':
    ak = aj + ' c1'
elif aj == '16':
    ak = aj + ' c2'
else:
    ak = aj + ' c3'
al = ''
for _ in range(5):
    for __ in range(3):
                al += ak
print(al)