a = input()
b = ''
for _ in range(5):
    if _ != 1:
        b += a
    else:
        b += '1'

c = ''
for _ in range(5):
    if _ != 1:
        c += b
    else:
        c += '1'

d = ''
for _ in range(3):
    if _ != 1:
        d += c
    else:
        d += '1'

print(d)