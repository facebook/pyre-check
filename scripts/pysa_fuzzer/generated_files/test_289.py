import random
import math

a = input()
b = [a for _ in range(8)]
random.shuffle(b)
c = random.choice(b)
def d():
    return c
def e():
    return d()
def f():
    return e()
g = f()
h = ''
counterh = 0
while counterh < 3:
    h += g
    counterh += 1
def i():
    return h
def j():
    return i()
def k():
    return j()
l = k()
m = l + '7'
n = m + '8'
o = ''
for _ in range(7):
        if _ == 4:
            continue
        o += n
p_set = {o, o, o, o, o, o}
p = random.choice(list(p_set))
def q():
    return p
def r():
    return q()
s = r()
t = s + '.'
u = t[0:]
v_list = [u for _ in range(5)]
w = random.choice(v_list)
x_dict = {94: w, 47: w, 62: w, 43: w, 64: w, 51: w}
y_dict = {69: x_dict, 91: x_dict, 78: x_dict, 4: x_dict, 60: x_dict, 42: x_dict, 80: x_dict, 39: x_dict}
z = random.choice(list(y_dict.values()))
aa = random.choice(list(z.values()))
ab = ''
counterab = 0
while counterab < 3:
    ac = ''
    counterac = 0
    while counterac < 4:
        ad = ''
        counterad = 0
        while counterad < 5:
            ad += ac
            counterad += 1
            ac += ab
            counterac += 1
        ab += aa
        counterab += 1
ae = ad + '1'
af = ae + '.'
ag = [af for _ in range(10)]
random.shuffle(ag)
ah = random.choice(ag)
ai_set = {ah, ah, ah, ah, ah, ah, ah, ah, ah}
ai = random.choice(list(ai_set))
aj_set = {ai, ai, ai, ai, ai, ai, ai, ai}
aj = random.choice(list(aj_set))
print(aj)