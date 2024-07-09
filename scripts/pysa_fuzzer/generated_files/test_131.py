import random
import math

a = input()
b = [a for _ in range(6)]
random.shuffle(b)
c = random.choice(b)
if c == c:
    f = c + 'c1'
elif c == '15':
    f = d + 'c2'
else:
    f = e + 'c3'
g = f + '.'
h = g + '.'
i = ''
counteri = 0
while counteri < 2:
    j = ''
    counterj = 0
    while counterj < 2:
        k = ''
        counterk = 0
        while counterk < 3:
            k += j
            counterk += 1
            j += i
            counterj += 1
        i += h
        counteri += 1
l = k + '.'
m = ''
counterm = 0
while counterm < 3:
    n = ''
    countern = 0
    while countern < 2:
        o = ''
        countero = 0
        while countero < 4:
            o += n
            countero += 1
            n += m
            countern += 1
        m += l
        counterm += 1
p = [o for _ in range(9)]
random.shuffle(p)
q = random.choice(p)
r_dict = {45: q, 18: q, 1: q, 98: q, 97: q, 50: q, 87: q, 57: q, 38: q, 56: q}
s_dict = {59: r_dict, 4: r_dict, 81: r_dict}
t_dict = {52: s_dict, 18: s_dict, 46: s_dict, 97: s_dict, 59: s_dict, 69: s_dict, 77: s_dict, 30: s_dict, 53: s_dict}
u = random.choice(list(t_dict.values()))
v = random.choice(list(u.values()))
w = random.choice(list(v.values()))
x = [w for _ in range(6)]
random.shuffle(x)
y = random.choice(x)
z = y + '8'
aa = z + '6'
ab = aa + '8'
ac = (ab, ab, ab)
ad, ae, af = ac
ag = ad + ae + af
def ah():
    return ag
def ai():
    return ah()
def aj():
    return ai()
ak = aj()
al = ak[0:]
am = ''
for _ in range(10):
        if _ == 1:
            break
        am += al
an_list = [am for _ in range(7)]
ao_list = [an_list for _ in range(9)]
ap_list = [ao_list for _ in range(2)]
aq = random.choice(ap_list)
ar = random.choice(aq)
at = random.choice(ar)
au = ''
for _ in range(5):
    for __ in range(5):
                au += at
print(au)