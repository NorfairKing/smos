import sys, struct, fcntl, termios

cols = int(sys.argv[1])
rows = int(sys.argv[2])
s = struct.pack('HHHH', rows, cols, 0, 0)
t = fcntl.ioctl(sys.stdout.fileno(), termios.TIOCSWINSZ, s)
# print(struct.unpack('HHHH', t))

