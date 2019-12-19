import sys
from itertools import permutations

file = open("input.txt", "r")
line = file.readline()
file.close()

orig_ops = {}
i = 0
for x in line.strip('\n').split(','):
    orig_ops[i] = int(x)
    i += 1

xs = [(a,b) for a in range(0, 99) for b in range(0, 99)]

argnum = {1: 3,
          2: 3,
          3: 1,
          4: 2,
          5: 2,
          6: 2,
          7: 3,
          8: 3,
          9: 1,
          99: 0}

debug = False

def turn_robot(robot_pos, turn):
    if turn == 0:
        tmp = robot_pos - 1
        if tmp < 0:
            tmp = 3
    elif turn == 1:
        tmp = robot_pos + 1
        if tmp > 3:
            tmp = 0
    else:
        print("Invalid turn value: %d" % turn)
        sys.exit()
    return tmp

def move_robot(robot_pos, robot_dir):
    (x, y) = robot_pos
    if robot_dir == 0:
        y = y - 1
    elif robot_dir == 1:
        x = x + 1
    elif robot_dir == 2:
        y = y + 1
    elif robot_dir == 3:
        x = x - 1
    return (x, y)

tiles = {
    0: " ",
    1: "|",
    2: "#",
    3: "_",
    4: "O"
    }

def draw_board(board):
    max_x = 0
    max_y = 0
    for tmp in board.keys():
        x,y = tmp
        if x > max_x:
            max_x = x
        if y > max_y:
            max_y = y

    line = list()
    for y in range(max_y+1):
        for x in range(max_x+1):
            if (x,y) in board:
                line.append(tiles[board[(x,y)]])
            else:
                line.append(" ")
        print("".join(line))
        line = list()


def run(state, inputs):
    output_buffer = list()
    ops = state["ops"]
    cur = state["cur"]
    inited = state["inited"]
    rel_base = state["rel_base"] if "rel_base" in state else 0
    board = dict()
    ball_now = (0,0)
    ball_prev = (0,0)
    paddle_now = (0,0)

    output = None
    done = False
    while not done:
        if debug:
            print("======================")
            #print("ops: [%s]" % ops.values())

        opcode = ops[cur]
        op = int(opcode % 100)

        argc = argnum.get(op, 0)
        modes = []
        read_args = []
        write_args = []

        if debug:
            print("cur: %d  opcode: %s  op: %d  argc: %d rel_base: %d" % (cur, opcode, op, argc, rel_base))

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
            if debug:
                print("ARGS[%d]: %d" % (i, ops[cur+1+i]))
            if modes[i] == 0: # position
                loc = ops[cur+1+i]
                if not loc in ops:
                    ops[loc] = 0
                r = ops[loc]
                w = loc
            elif modes[i] == 1: # immediate
                w = r = ops[cur+1+i]
            elif modes[i] == 2: # relative
                if not rel_base+ops[cur+1+i] in ops:
                    ops[rel_base+ops[cur+1+i]] = 0
                r = ops[rel_base+ops[cur+1+i]]
                w = rel_base+ops[cur+1+i]
            else:
                print("Invalid argument mode [%d]" % modes[i])
                sys.exit()
            read_args.append(r)
            write_args.append(w)

        if debug:
            print("read_args : [%s]" % ', '.join(map(str, read_args)))
            print("write_args : [%s]" % ', '.join(map(str, write_args)))

        if op == 1:
            ops[write_args[2]] = read_args[0] + read_args[1]
            cur += 4
            if debug:
                print("= ADD  %d, %d  => ops[%d]=%d" % (read_args[0], read_args[1], write_args[2], ops[write_args[2]]))
        elif op == 2:
            ops[write_args[2]] = read_args[0] * read_args[1]
            cur += 4
            if debug:
                print("= MULT %d, %d  => ops[%d]=%d" % (read_args[0], read_args[1], write_args[2], ops[write_args[2]]))
        elif op == 3: # Input
            draw_board(board)
            print("Got input")
            ox,oy = ball_prev
            nx,ny = ball_now
            px,py = paddle_now

            if nx < px:
                ops[write_args[0]] = -1
            elif nx > px:
                ops[write_args[0]] = 1

            cur += 2
        elif op == 4: # Output
            output = read_args[0]
            if debug:
                print("= OUTPUT: %s" % output)
            output_buffer.append(output)

            if len(output_buffer) == 3:
                x,y,t = output_buffer
                if x == -1 and y == 0:
                    print("SCORE: %d" % t)
                else:
                    board[(x,y)] = t
                    if t == 4: # ball
                        ball_prev = ball_now
                        ball_now = (x,y)
                    elif t == 3:
                        paddle_now = (x,y)
                output_buffer = list()
            cur += 2

        elif op == 5: # jump-if-true
            cur = read_args[1] if read_args[0] != 0 else cur+3
            if debug:
                print("= JTRU %d  => cur=%d" % (read_args[0], cur))
        elif op == 6: # jump-if-false
            cur = read_args[1] if read_args[0] == 0 else cur+3
        elif op == 7: # less-than
            ops[write_args[2]] = 1 if read_args[0] < read_args[1]  else 0
            cur += 4
            if debug:
                print("= LESS %d, %d  => ops[%d]=%d" % (read_args[0], read_args[1], write_args[2], ops[write_args[2]]))
        elif op == 8: # equal
            ops[write_args[2]] = 1 if read_args[0] == read_args[1] else 0
            cur += 4
            if debug:
                print("= EQUAL %d, %d  => pos[%d]=%d" % (read_args[0], read_args[1], write_args[2], ops[write_args[2]]))
        elif op == 9: # relative base
            rel_base += read_args[0]
            cur += 2
            if debug:
                print("= RELB %d" % rel_base)
        elif op == 99:
            return output_buffer
        else:
            print("Wrong op [%d]" %  op)
            sys.exit()


def chunks(lst, n):
    for i in range(0, len(lst), n):
        yield lst[i:i + n]

state = {"ops": orig_ops.copy(),
         "cur": 0,
         "inited": False}
inputv = 2
output = None
phase = 0

run(state, [inputv])
