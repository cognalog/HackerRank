import sys;

def maxDivSquare(l, b):
    size = l * b;
    p = 1
    inc = 3
    limit = min(l, b)
    top = 0
    while(p <= size and (inc - 1) / 2 <= limit):
        if size % p == 0 and l % ((inc - 1) / 2) == 0 and b % ((inc - 1) / 2) == 0:
            top = p
        p += inc
        inc += 2
    return size / top

n = int(sys.stdin.readline())
for i in range(n):
    lb = sys.stdin.readline()
    l = int(lb.split(" ")[0])
    b = int(lb.split(" ")[1])
    print maxDivSquare(l, b)
