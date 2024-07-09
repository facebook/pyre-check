import random
import math

a = input()
b = f'string {a}'
c_list = [b for _ in range(2)]
d_list = [c_list for _ in range(5)]
e = random.choice(d_list)
f = random.choice(e)
g = f + '.'
h_dict = {49: g, 51: g, 61: g, 81: g, 82: g, 51: g}
i = random.choice(list(h_dict.values()))
j = ''
for _ in range(2):
    j += i
k = [j for _ in range(8)]
random.shuffle(k)
l = random.choice(k)
m = (l, l, l)
n, o, p = m
q = n + o + p
def r():
    return q
def s():
    return r()
def t():
    return s()
u = t()
v_set = {u, u, u, u}
v = random.choice(list(v_set))
w = v + '.'
x = w + '7'
y = x + '6'
z = y + '4'
aa = z[0:]
ab = f'string {aa}'
ac = ''
counterac = 0
while counterac < 3:
    ad = ''
    counterad = 0
    while counterad < 5:
        ae = ''
        counterae = 0
        while counterae < 4:
            ae += ad
            counterae += 1
            ad += ac
            counterad += 1
        ac += ab
        counterac += 1
af_list = [ae for _ in range(10)]
ag = random.choice(af_list)
ah = ''
for _ in range(4):
    for __ in range(3):
                ah += ag
ai_dict = {58: ah, 61: ah, 37: ah, 59: ah, 70: ah, 37: ah, 82: ah}
aj_dict = {46: ai_dict, 60: ai_dict, 77: ai_dict, 67: ai_dict}
ak_dict = {45: aj_dict, 23: aj_dict, 17: aj_dict}
al = random.choice(list(ak_dict.values()))
am = random.choice(list(al.values()))
an = random.choice(list(am.values()))
ao = an[0:]
print(ao)