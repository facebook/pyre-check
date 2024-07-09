import random
import math

a = input()
b_list = [a for _ in range(8)]
c = random.choice(b_list)
d = (c, c, c)
e, f, g = d
h = e + f + g
i_set = {h, h, h, h, h, h, h, h}
i = random.choice(list(i_set))
j = ''
counterj = 0
while counterj < 4:
    k = ''
    counterk = 0
    while counterk < 4:
        l = ''
        counterl = 0
        while counterl < 3:
            l += k
            counterl += 1
            k += j
            counterk += 1
        j += i
        counterj += 1
m = ''
counterm = 0
while counterm < 3:
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
        m += l
        counterm += 1
p_list = [o for _ in range(9)]
q_list = [p_list for _ in range(5)]
r_list = [q_list for _ in range(3)]
s = random.choice(r_list)
t = random.choice(s)
u = random.choice(t)
if u == u:
    x = u + 'c1'
elif u == '16':
    x = v + 'c2'
else:
    x = w + 'c3'
y = f'string {x}'
z = y + '.'
aa = z + '2'
ab = [aa for _ in range(8)]
random.shuffle(ab)
ac = random.choice(ab)
ad = (ac, ac, ac)
ae, af, ag = ad
ah = ae + af + ag
ai = ''
for _ in range(9):
        if _ == 3:
            break
        ai += ah
aj = ''
for _ in range(5):
    ak = ''
    for _ in range(5):
        ak += aj
        aj += ai
al_list = [ak for _ in range(2)]
am_list = [al_list for _ in range(2)]
an = random.choice(am_list)
ao = random.choice(an)
ap_set = {ao, ao, ao, ao, ao, ao, ao}
ap = random.choice(list(ap_set))
aq = ''
for _ in range(3):
    for __ in range(3):
                aq += ap
ar = aq[0:]
print(ar)