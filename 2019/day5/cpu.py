import sys

file = open("input.txt", "r")
line = file.readline()
file.close()

orig_ops = [int(x) for x in line.strip('\n').split(',')]

xs = [(a,b) for a in range(0, 99) for b in range(0, 99)]

input_v = 5

argnum = {1: 3,
          2: 3,
          3: 1,
          4: 1,
          5: 2,
          6: 2,
          7: 3,
          8: 3,
          99: 0}

debug = False

def run(ops):
    cur = 0
    done = False
    while not done:
        if debug:
            print("======================")
            print("ops: [%s]" % ', '.join(map(str, ops)))

        opcode = ops[cur]
        op = int(opcode % 100)

        argc = argnum.get(op, 0)
        modes = []
        argv = []

        if debug:
            print("cur: %d  opcode: %s  op: %d  argc: %d" % (cur, opcode, op, argc))

        i = 0
        tmp = int(opcode / 100)
        while tmp > 0:
            modes.append(int(tmp % 10))
            i += 1
            tmp = int(tmp/10)
        while len(modes) < argc:
            modes.append(0)

        if debug:
            print("modes: [%s]" % ', '.join(map(str, modes)))

        for i in range(argc):
            if modes[i] == 0:
                v = ops[ops[cur+1+i]]
            elif modes[i] == 1:
                v = ops[cur+1+i]
            else:
                print("Invalid argument mode [%d]" % modes[i])
                sys.exit()
            argv.append(v)

        if debug:
            print("argv : [%s]" % ', '.join(map(str, argv)))

        if op == 1:
            v = argv[0] + argv[1]
            ops[ops[cur+3]] = v
            cur += 4
        elif op == 2:
            v = argv[0] * argv[1]
            ops[ops[cur+3]] = v
            cur += 4
        elif op == 3:
            ops[ops[cur+1]] = input_v
            cur += 2
        elif op == 4:
            print("OUTPUT: %d" % argv[0])
            cur += 2
        elif op == 5: # jump-if-true
            cur = argv[1] if argv[0] != 0 else cur+3
        elif op == 6: # jump-if-false
            cur = argv[1] if argv[0] == 0 else cur+3
        elif op == 7: # less-than
            ops[ops[cur+3]] = 1 if argv[0] < argv[1]  else 0
            cur += 4
        elif op == 8: # equal
            ops[ops[cur+3]] = 1 if argv[0] == argv[1] else 0
            cur += 4
        elif op == 99:
            done = True
        else:
            print("Wrong op [%d]" %  op)
            sys.exit()
    return ops

for inputs in xs:
    ops = orig_ops.copy()
    output = run(ops)
