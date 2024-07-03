import random
import math
a = input()
b = ''
for _ in range(5):
    for __ in range(3):
                b += a
c_list = [b for _ in range(7)]
d_list = [c_list for _ in range(9)]
e_list = [d_list for _ in range(5)]
f = random.choice(e_list)
g = random.choice(f)
h = random.choice(g)
i = ''
for _ in range(5):
        if _ == 5:
            break
        i += h
j_list = [i for _ in range(9)]
k = random.choice(j_list)
l = ''
counterl = 0
while counterl < 4:
    l += k
    counterl += 1
if l == '5':
    m = l + ' c1'
elif l == '13':
    m = l + ' c2'
else:
    m = l + ' c3'
n = ''
countern = 0
while countern < 5:
    o = ''
    countero = 0
    while countero < 2:
        o += n
        countero += 1
        n += m
        countern += 1
p = o + '.'
q = [p for _ in range(10)]
random.shuffle(q)
r = random.choice(q)
s_list = [r for _ in range(2)]
t = random.choice(s_list)
if t == '5':
    u = t + ' c1'
elif t == '19':
    u = t + ' c2'
else:
    u = t + ' c3'
v_list = [u for _ in range(6)]
w_list = [v_list for _ in range(9)]
x_list = [w_list for _ in range(4)]
y = random.choice(x_list)
z = random.choice(y)
aa = random.choice(z)
ab_set = {aa, aa, aa, aa, aa, aa, aa}
ab = random.choice(list(ab_set))
ac = (ab, ab, ab)
ad, ae, af = ac
ag = ad + ae + af
ah = ''
counterah = 0
while counterah < 2:
    ai = ''
    counterai = 0
    while counterai < 3:
        aj = ''
        counteraj = 0
        while counteraj < 2:
            aj += ai
            counteraj += 1
            ai += ah
            counterai += 1
        ah += ag
        counterah += 1
def ak():
    return aj
al = ak()
am = ''
for _ in range(5):
    an = ''
    for _ in range(4):
        an += am
        am += al
ao = [an for _ in range(10)]
random.shuffle(ao)
ap = random.choice(ao)
print(ap)