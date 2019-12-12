import sys

file = open("input.txt", "r")
line = file.readline()
file.close()

orig_ops = [int(x) for x in line.strip('\n').split(',')]

xs = [(a,b) for a in range(0, 99) for b in range(0, 99)]

def run(ops):
    cur = 0
    done = False
    while not done:
        op = ops[cur]
        if op == 1:
            v = ops[ops[cur+1]] + ops[ops[cur+2]]
            ops[ops[cur+3]] = v
            cur += 4
        elif op == 2:
            v = ops[ops[cur+1]] * ops[ops[cur+2]]
            ops[ops[cur+3]] = v
            cur += 4
        elif op == 99:
            done = True
        else:
            print("Wrong op" + ops)
            return []
    return ops

for inputs in xs:
    ops = orig_ops.copy()
    ops[1] = inputs[0]
    ops[2] = inputs[1]
    output = run(ops)

    if output and output[0] == 19690720:
        print(inputs)
        print(output)
        print((100*inputs[0]) + inputs[1])
        sys.exit()
