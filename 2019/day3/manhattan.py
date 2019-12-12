
file = open("input.txt", "r")
data1 = file.readline().strip("\n").split(',')
data2 = file.readline().strip("\n").split(',')
file.close()

world = dict()
world[0] = dict()
world[0][0] = dict();

def update(x, y, i, l):
    if x not in world:
        world[x] = dict()

    tmp = world[x]
    if y not in tmp:
        tmp[y] = dict()

    if i not in tmp[y]:
        tmp[y][i] = l


for d in [(data1, 0), (data2, 1)]:
    x = 0
    y = 0

    data = d[0]
    id = d[1]
    length = 0
    for i in data:
        direction = i[0]
        distance = int(i[1:])
        steps = [i for i in range(1, distance+1)]

        m = 1 if direction in ["U", "R"] else -1

        if direction in ["L", "R"]:
            while distance > 0:
                distance -= 1;
                length += 1
                x += m
                update(x, y, id, length)

        if direction in ["U", "D"]:
            while distance > 0:
                distance -= 1;
                length += 1
                y += m
                update(x, y, id, length)

if False:
    for y in range(5, -5, -1):
        for x in range(-5, 5):
            if x in world.keys():
                if y in world[x].keys():
                    print(world[x][y], end="")
                else:
                    print(".", end="")
            else:
                print(".", end="")
        print()
    print()



crosses = []
least = None
for k1 in world.keys():
    for k2 in world[k1].keys():
        indeces = world[k1][k2].keys()
        if len(indeces) > 1:
            manhattan = abs(k1)+abs(k2)
            length = 0
            for l in world[k1][k2].values():
                length += l
            crosses.append((k1, k2, manhattan, length))
            if not least or length < least:
                least = length

print(crosses)
print(least)
