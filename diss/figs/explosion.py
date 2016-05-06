import sys

with open('explosion'+sys.argv[1]+'.gv', 'w') as f:
    def output_edges(sofar, threads):
        for p in range(threads):
            if (not (p in sofar)):
                sf = ''.join(str(x) for x in sofar)
                edge = sf + ' -> ' + sf + str(p)
                if sofar != []:
                    f.write(edge)
                    f.write('\n')
                output_edges(sofar + [p], threads)

    output_edges([], 3)
