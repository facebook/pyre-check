import random
import math

a = input()
b = f'string {a}'
c = ''
counterc = 0
while counterc < 5:
    d = ''
    counterd = 0
    while counterd < 2:
        d += c
        counterd += 1
        c += b
        counterc += 1
e = [d for _ in range(6)]
random.shuffle(e)
f = random.choice(e)
g = [f for _ in range(8)]
random.shuffle(g)
h = random.choice(g)
i = f'string {h}'
if i == i:
    l = i + 'c1'
elif i == '17':
    l = j + 'c2'
else:
    l = k + 'c3'
m = l[0:]
n = ''
for _ in range(5):
    o = ''
    for _ in range(2):
        p = ''
        for _ in range(3):
            p += o
            o += n
        n += m
q = ''
for _ in range(2):
    r = ''
    for _ in range(3):
        s = ''
        for _ in range(4):
            s += r
            r += q
        q += p
t = f'string {s}'
u = t + '.'
def v():
    return u
w = v()
x = w[0:]
y = [x for _ in range(6)]
random.shuffle(y)
z = random.choice(y)
aa_dict = {93: z, 9: z, 10: z, 93: z, 11: z, 87: z, 49: z, 14: z, 41: z}
ab = random.choice(list(aa_dict.values()))
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
af_dict = {73: ae, 92: ae, 83: ae, 18: ae, 21: ae, 13: ae, 2: ae, 2: ae, 55: ae, 91: ae}
ag_dict = {29: af_dict, 25: af_dict, 9: af_dict, 11: af_dict, 17: af_dict}
ah_dict = {8: ag_dict, 99: ag_dict, 74: ag_dict}
ai = random.choice(list(ah_dict.values()))
aj = random.choice(list(ai.values()))
ak = random.choice(list(aj.values()))
al = f'string {ak}'
print(al)