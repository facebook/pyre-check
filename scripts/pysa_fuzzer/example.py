

def f0(x):
    import random
    x_list = list(x)
    random.shuffle(x_list)
    return ''.join(x_list)

a = f0(input())


def f1(x):

    return x[::-1]

def f2(x):

    return x[::-1]

b = f2(f1(a))


def f3(x):
    # Compression example: convert to hex
    return ''.join(format(ord(c), 'x') for c in x)

def f4(x):
    # Decompression: convert hex back to string
    return ''.join(chr(int(x[i:i+2], 16)) for i in range(0, len(x), 2))

c = f4(f3(b))


def f5(x):
    # Simple encryption: shift characters by 2
    return ''.join(chr(ord(c) + 2) for c in x)

def f6(x):
    # Decryption: shift characters back by 2
    return ''.join(chr(ord(c) - 2) for c in x)

d = f6(f5(c))


import base64

def f7(x):
    # Encode the string using Base64
    return base64.b64encode(x.encode('utf-8')).decode('utf-8')

def f8(x):
    # Decode the string from Base64
    return base64.b64decode(x.encode('utf-8')).decode('utf-8')

e = f8(f7(d))


def f9(x):
    # Substitute each letter with its opposite
    def substitute(c):
        if c.isalpha():
            offset = ord('a') if c.islower() else ord('A')
            return chr(offset + 25 - (ord(c) - offset))
        return c
    return ''.join(substitute(c) for c in x)

def f10(x):
    # Reverse substitution using the same logic
    return f9(x)

f = f10(f9(e))


def f11(x):
    return x

def f12(x):
    return x

def f13(x):
    return f11(f12(x))


def f14(x):
    # Apply ROT13 transformation
    def rotate(c):
        if c.isalpha():
            offset = ord('a') if c.islower() else ord('A')
            return chr(offset + (ord(c) - offset + 13) % 26)
        return c
    return ''.join(rotate(c) for c in x)

g = f14(f14(f13(f)))


def f15(x):
    # Swap adjacent characters
    result = list(x)
    for i in range(0, len(result) - 1, 2):
        result[i], result[i+1] = result[i+1], result[i]
    return ''.join(result)

def f16(x):
    # Reverse swap using the same logic
    return f15(x)

h = f16(f15(g))


j = [3, 1, 4, 2, 0]  # A simple transposition key

def f17(x, key):
    # Transposition cipher encryption
    n = len(x)
    key = key[:n]  # Adjust key length to match input
    sorted_key = sorted(range(n), key=lambda i: key[i])
    return ''.join(x[i] for i in sorted_key)

def f18(x, key):
    # Transposition cipher decryption
    n = len(x)
    key = key[:n]
    inverse_key = sorted(range(n), key=lambda i: key[i])
    return ''.join(x[inverse_key.index(i)] for i in range(n))

i = f18(f17(h, j), j)


l = 'secret'  # Vigenère cipher keyword

def f19(x, keyword):
    # Vigenère cipher encoding
    encoded_chars = []
    keyword_repeated = (keyword * ((len(x) // len(keyword)) + 1))[:len(x)]
    for i, c in enumerate(x):
        if c.isalpha():
            offset = ord('a') if c.islower() else ord('A')
            keyword_offset = ord(keyword_repeated[i].lower()) - ord('a')
            encoded_char = chr((ord(c) - offset + keyword_offset) % 26 + offset)
        else:
            encoded_char = c
        encoded_chars.append(encoded_char)
    return ''.join(encoded_chars)

def f20(x, keyword):
    # Vigenère cipher decoding
    decoded_chars = []
    keyword_repeated = (keyword * ((len(x) // len(keyword)) + 1))[:len(x)]
    for i, c in enumerate(x):
        if c.isalpha():
            offset = ord('a') if c.islower() else ord('A')
            keyword_offset = ord(keyword_repeated[i].lower()) - ord('a')
            decoded_char = chr((ord(c) - offset - keyword_offset + 26) % 26 + offset)
        else:
            decoded_char = c
        decoded_chars.append(decoded_char)
    return ''.join(decoded_chars)

k = f20(f19(i, l), l)


n = 42  # XOR cipher key

def f21(x, key):
    # XOR cipher encryption
    return ''.join(chr(ord(c) ^ key) for c in x)

def f22(x, key):
    # XOR cipher decryption (same operation)
    return ''.join(chr(ord(c) ^ key) for c in x)

m = f22(f21(k, n), n)


def f23(x):
    # Split string into two halves and interleave them
    mid = len(x) // 2
    first_half = x[:mid]
    second_half = x[mid:]
    scrambled = ''.join(second_half[i:i+1] + first_half[i:i+1] for i in range(mid))
    if len(x) % 2 == 1:
        scrambled += second_half[-1]
    return scrambled

def f24(x):
    # Unscramble by reversing the interleave
    mid = len(x) // 2
    first_half = ''.join(x[i] for i in range(1, len(x), 2))
    second_half = ''.join(x[i] for i in range(0, len(x), 2))
    return second_half[:mid] + first_half

o = f24(f23(m))


def f25(x, rails):
    # Rail Fence cipher encryption
    fence = [[] for _ in range(rails)]
    rail = 0
    direction = 1
    for char in x:
        fence[rail].append(char)
        rail += direction
        if rail == 0 or rail == rails - 1:
            direction *= -1
    return ''.join(''.join(row) for row in fence)

def f26(x, rails):
    # Rail Fence cipher decryption
    rail_lengths = [0] * rails
    rail = 0
    direction = 1
    for _ in x:
        rail_lengths[rail] += 1
        rail += direction
        if rail == 0 or rail == rails - 1:
            direction *= -1
    rail_chars = [''] * rails
    i = 0
    for r in range(rails):
        rail_chars[r] = x[i:i + rail_lengths[r]]
        i += rail_lengths[r]
    result = []
    rail = 0
    direction = 1
    for _ in x:
        result.append(rail_chars[rail][0])
        rail_chars[rail] = rail_chars[rail][1:]
        rail += direction
        if rail == 0 or rail == rails - 1:
            direction *= -1
    return ''.join(result)

p = f26(f25(o, 3), 3)


def f27(x):
    # Shifting Caesar cipher encryption
    return ''.join(
        chr((ord(c) - (ord('a') if c.islower() else ord('A')) + (i % 26)) % 26 + (ord('a') if c.islower() else ord('A')))
        if c.isalpha() else c
        for i, c in enumerate(x)
    )

def f28(x):
    # Shifting Caesar cipher decryption
    return ''.join(
        chr((ord(c) - (ord('a') if c.islower() else ord('A')) - (i % 26) + 26) % 26 + (ord('a') if c.islower() else ord('A')))
        if c.isalpha() else c
        for i, c in enumerate(x)
    )

q = f28(f27(p))


def f29(x):
    # Affine cipher encryption
    def encrypt_char(c):
        if c.isalpha():
            offset = ord('a') if c.islower() else ord('A')
            return chr(((ord(c) - offset) * 5 + 8) % 26 + offset)
        return c
    return ''.join(encrypt_char(c) for c in x)

def f30(x):
    # Affine cipher decryption
    def decrypt_char(c):
        if c.isalpha():
            offset = ord('a') if c.islower() else ord('A')
            a_inv = pow(5, -1, 26)  # Modular inverse of a
            return chr(((ord(c) - offset - 8) * a_inv) % 26 + offset)
        return c
    return ''.join(decrypt_char(c) for c in x)

r = f30(f29(q))


t = 12345  # Seed for LCG

def lcg(seed, a=1664525, c=1013904223, m=2**32):
    # Linear Congruential Generator
    while True:
        seed = (a * seed + c) % m
        yield seed

def f31(x, seed):
    # XOR each byte with pseudo-random byte from LCG
    lcg_gen = lcg(seed)
    return ''.join(
        chr(ord(c) ^ (next(lcg_gen) % 256)) for c in x
    )

def f32(x, seed):
    # XOR each byte with the same pseudo-random byte from LCG
    return f31(x, seed)

s = f32(f31(r, t), t)


def f33(x):
    # Reverse segments of the string
    return ''.join(
        x[i:i+3][::-1] for i in range(0, len(x), 3)
    )

def f34(x):
    # Restore original string by reversing segments again
    return f33(x)

u = f34(f33(s))


def f35(x, levels):
    # Zigzag pattern encoding
    if levels <= 1 or levels >= len(x):
        return x
    zigzag = ['' for _ in range(levels)]
    level = 0
    direction = 1
    for char in x:
        zigzag[level] += char
        level += direction
        if level == 0 or level == levels - 1:
            direction *= -1
    return ''.join(zigzag)

def f36(x, levels):
    # Zigzag pattern decoding
    if levels <= 1 or levels >= len(x):
        return x
    pattern = [0] * len(x)
    level = 0
    direction = 1
    for i in range(len(x)):
        pattern[i] = level
        level += direction
        if level == 0 or level == levels - 1:
            direction *= -1
    decoded = [''] * len(x)
    index = 0
    for l in range(levels):
        for i in range(len(x)):
            if pattern[i] == l:
                decoded[i] = x[index]
                index += 1
    return ''.join(decoded)

v = f36(f35(u, 3), 3)


x = '3142'  # Key for columnar transposition

def f37(x, key):
    # Columnar transposition encryption
    num_cols = len(key)
    num_rows = (len(x) + num_cols - 1) // num_cols
    padded_length = num_rows * num_cols
    padded_text = x.ljust(padded_length)
    cols = [''] * num_cols
    for r in range(num_rows):
        for c in range(num_cols):
            cols[c] += padded_text[r * num_cols + c]
    sorted_key_indices = sorted(range(num_cols), key=lambda k: key[k])
    return ''.join(cols[i] for i in sorted_key_indices)

def f38(x, key):
    # Columnar transposition decryption
    num_cols = len(key)
    num_rows = (len(x) + num_cols - 1) // num_cols
    cols = [''] * num_cols
    col_lengths = [num_rows] * num_cols
    for i in range(len(x) % num_cols):
        col_lengths[i] -= 1
    sorted_key_indices = sorted(range(num_cols), key=lambda k: key[k])
    index = 0
    for i in sorted_key_indices:
        cols[i] = x[index:index + col_lengths[i]]
        index += col_lengths[i]
    decrypted = ''.join(
        ''.join(cols[c][r] for c in range(num_cols) if r < len(cols[c]))
        for r in range(num_rows)
    )
    return decrypted.strip()

w = f38(f37(v, x), x)


z = 'keyword'  # Keyword for Playfair cipher

def create_playfair_square(keyword):
    alphabet = 'abcdefghiklmnopqrstuvwxyz'  # Excludes 'j'
    square = []
    used = set()
    for char in keyword:
        if char not in used and char in alphabet:
            square.append(char)
            used.add(char)
    for char in alphabet:
        if char not in used:
            square.append(char)
            used.add(char)
    return [square[i:i+5] for i in range(0, 25, 5)]

def find_position(char, square):
    for row in range(5):
        for col in range(5):
            if square[row][col] == char:
                return row, col
    return None, None

def f39(x, keyword):
    square = create_playfair_square(keyword)
    x = x.lower().replace('j', 'i').replace(' ', '')
    if len(x) % 2 != 0:
        x += 'x'
    encrypted = []
    for i in range(0, len(x), 2):
        a, b = x[i], x[i+1]
        row_a, col_a = find_position(a, square)
        row_b, col_b = find_position(b, square)
        if row_a == row_b:
            encrypted.extend([square[row_a][(col_a + 1) % 5], square[row_b][(col_b + 1) % 5]])
        elif col_a == col_b:
            encrypted.extend([square[(row_a + 1) % 5][col_a], square[(row_b + 1) % 5][col_b]])
        else:
            encrypted.extend([square[row_a][col_b], square[row_b][col_a]])
    return ''.join(encrypted)

def f40(x, keyword):
    square = create_playfair_square(keyword)
    decrypted = []
    for i in range(0, len(x), 2):
        a, b = x[i], x[i+1]
        row_a, col_a = find_position(a, square)
        row_b, col_b = find_position(b, square)
        if row_a == row_b:
            decrypted.extend([square[row_a][(col_a - 1) % 5], square[row_b][(col_b - 1) % 5]])
        elif col_a == col_b:
            decrypted.extend([square[(row_a - 1) % 5][col_a], square[(row_b - 1) % 5][col_b]])
        else:
            decrypted.extend([square[row_a][col_b], square[row_b][col_a]])
    return ''.join(decrypted)

y = f40(f39(w, z), z)

print(y)