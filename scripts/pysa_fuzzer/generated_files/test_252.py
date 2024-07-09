import random
import math

a = input()
b = [a for _ in range(5)]
random.shuffle(b)
c = random.choice(b)
d = c + '.'
e_set = {d, d, d, d, d, d, d}
e = random.choice(list(e_set))
if e == e:
    h = e + 'c1'
elif e == '19':
    h = f + 'c2'
else:
    h = g + 'c3'
i_list = [h for _ in range(6)]
j = random.choice(i_list)
k = (j, j, j)
l, m, n = k
o = l + m + n
p = (o, o, o)
q, r, s = p
t = q + r + s
if t == t:
    w = t + 'c1'
elif t == '18':
    w = u + 'c2'
else:
    w = v + 'c3'
def x():
    return w
def y():
    return x()
def z():
    return y()
aa = z()
ab_list = [aa for _ in range(2)]
ac_list = [ab_list for _ in range(5)]
ad_list = [ac_list for _ in range(7)]
ae = random.choice(ad_list)
af = random.choice(ae)
ag = random.choice(af)
ah_list = [ag for _ in range(10)]
ai = random.choice(ah_list)
aj = (ai, ai, ai)
ak, al, am = aj
an = ak + al + am
ao = ''
for _ in range(6):
        if _ == 5:
            continue
        ao += an
ap = ''
counterap = 0
while counterap < 3:
    aq = ''
    counteraq = 0
    while counteraq < 5:
        aq += ap
        counteraq += 1
        ap += ao
        counterap += 1
ar = aq[0:]
at = [ar for _ in range(5)]
random.shuffle(at)
au = random.choice(at)
av_list = [au for _ in range(2)]
aw = random.choice(av_list)
ax_dict = {16: aw, 76: aw, 50: aw, 25: aw}
ay = random.choice(list(ax_dict.values()))
print(ay)