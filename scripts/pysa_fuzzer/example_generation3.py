import random

# example recursive structure with depth 5 for generate_for_loop

a = input()
b = ''
for _ in range(4):
    c = ''
    for _ in range(2):
        d = ''
        for _ in range(5):
            e = ''
            for _ in range(5):
                f = ''
                for _ in range(3):
                    f += e
                    e += d
                d += c
            c += b
        b += a
print(f)


# example recursive structure with depth 5 for generate_while_loop

a = input()
b = ''
counterb = 0
while counterb < 4:
    c = ''
    counterc = 0
    while counterc < 4:
        d = ''
        counterd = 0
        while counterd < 3:
            e = ''
            countere = 0
            while countere < 3:
                f = ''
                counterf = 0
                while counterf < 5:
                    f += e
                    counterf += 1
                    e += d
                    countere += 1
                d += c
                counterd += 1
            c += b
            counterc += 1
        b += a
        counterb += 1
print(f)

# example recursive structure with depth 5 for generate_list 

a = input()
b_list = [a for _ in range(8)]
c_list = [b_list for _ in range(3)]
d_list = [c_list for _ in range(8)]
e_list = [d_list for _ in range(4)]
f_list = [e_list for _ in range(2)]
g = random.choice(f_list)
h = random.choice(g)
i = random.choice(h)
j = random.choice(i)
k = random.choice(j)
print(k)


# example recusive structure with depth 5 for generate_dictionary 

a = input()
b_dict = {73: a, 60: a, 50: a}
c_dict = {55: b_dict, 3: b_dict, 86: b_dict, 16: b_dict}
d_dict = {20: c_dict, 91: c_dict}
e_dict = {34: d_dict, 18: d_dict, 39: d_dict, 7: d_dict, 22: d_dict, 52: d_dict, 75: d_dict, 91: d_dict, 35: d_dict, 99: d_dict}
f_dict = {98: e_dict, 100: e_dict, 1: e_dict, 39: e_dict, 50: e_dict}
g = random.choice(list(f_dict.values()))
h = random.choice(list(g.values()))
i = random.choice(list(h.values()))
j = random.choice(list(i.values()))
k = random.choice(list(j.values()))
print(k)

