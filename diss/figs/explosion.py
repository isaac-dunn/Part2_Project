import sys

with open('explosion'+sys.argv[1]+'.gv', 'w') as f:
    def output_edges(sofar, threads):
        for p in range(threads):
            if (not (p in sofar)):
                sf = ''.join(['root'] + [str(x) for x in sofar])
                edge = '  ' + sf + ' -> ' + sf + str(p) + '[label="' + str(p) + '"];\n'
                f.write(edge)
                output_edges(sofar + [p], threads)

    prelim = 'digraph Explosion {\n  node [shape=circle, fixedsize=true, label=""];\n'
    postlim = '}'
    f.write(prelim)
    output_edges([], int(sys.argv[1]))
    f.write(postlim)
