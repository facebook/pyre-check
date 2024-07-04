import random
import math
a = input()
b_list = [a for _ in range(10)]
c_list = [b_list for _ in range(4)]
d = random.choice(c_list)
e = random.choice(d)
f = e + '.'
g = [f for _ in range(10)]
random.shuffle(g)
h = random.choice(g)
i = ''
counteri = 0
while counteri < 5:
    j = ''
    counterj = 0
    while counterj < 3:
        j += i
        counterj += 1
        i += h
        counteri += 1
k = ''
counterk = 0
while counterk < 4:
    k += j
    counterk += 1
l = ''
for _ in range(9):
        if _ == 3:
            continue
        l += k
m = ''
counterm = 0
while counterm < 2:
    n = ''
    countern = 0
    while countern < 5:
        o = ''
        countero = 0
        while countero < 5:
            o += n
            countero += 1
            n += m
            countern += 1
        m += l
        counterm += 1
p = f'string {o}'
q = p[0:]
r = [q for _ in range(8)]
random.shuffle(r)
s = random.choice(r)
t = [s for _ in range(8)]
random.shuffle(t)
u = random.choice(t)
v_dict = {4: u, 17: u, 16: u, 57: u, 94: u, 67: u}
w = random.choice(list(v_dict.values()))
x_list = [w for _ in range(10)]
y_list = [x_list for _ in range(10)]
z = random.choice(y_list)
aa = random.choice(z)
ab = ''
for _ in range(4):
    ac = ''
    for _ in range(3):
        ac += ab
        ab += aa
ad_dict = {60: ac, 63: ac, 75: ac, 65: ac, 81: ac, 81: ac}
ae_dict = {86: ad_dict, 11: ad_dict, 21: ad_dict, 1: ad_dict}
af = random.choice(list(ae_dict.values()))
ag = random.choice(list(af.values()))
ah = ''
for _ in range(5):
    ai = ''
    for _ in range(4):
        ai += ah
        ah += ag
aj = ai + '.'
ak = ''
for _ in range(5):
    for __ in range(3):
                ak += aj
print(ak)