import random
import math
a = input()
b = a[0:]
c = b + '5'
if c == '8':
    d = c + ' c1'
elif c == '15':
    d = c + ' c2'
else:
    d = c + ' c3'
e_set = {d, d, d}
e = random.choice(list(e_set))
f_list = [e for _ in range(7)]
g = random.choice(f_list)
h_set = {g, g, g, g, g, g}
h = random.choice(list(h_set))
i = ''
counteri = 0
while counteri < 4:
    j = ''
    counterj = 0
    while counterj < 2:
        j += i
        counterj += 1
        i += h
        counteri += 1
def k():
    return j
l = k()
def m():
    return l
n = m()
o = ''
countero = 0
while countero < 2:
    o += n
    countero += 1
p = o + '.'
q = p + '5'
r_dict = {16: q, 61: q, 47: q}
s_dict = {93: r_dict, 62: r_dict, 9: r_dict, 65: r_dict}
t = random.choice(list(s_dict.values()))
u = random.choice(list(t.values()))
v = ''
for _ in range(3):
    w = ''
    for _ in range(2):
        x = ''
        for _ in range(3):
            x += w
            w += v
        v += u
y_dict = {95: x, 22: x, 59: x, 66: x, 91: x, 31: x, 86: x}
z_dict = {14: y_dict, 76: y_dict, 44: y_dict}
aa = random.choice(list(z_dict.values()))
ab = random.choice(list(aa.values()))
ac = ab[0:]
if ac == '2':
    ad = ac + ' c1'
elif ac == '16':
    ad = ac + ' c2'
else:
    ad = ac + ' c3'
ae = (ad, ad, ad)
af, ag, ah = ae
ai = af + ag + ah
print(ai)