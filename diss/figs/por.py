import sys

with open('por'+sys.argv[1]+'.gv', 'w') as f:
    def output_edges(sofar, threads):
        for p in range(threads):
            if (not (p in sofar)):
                sf = ''.join(sorted(['0'] + [str(x+1) for x in sofar]))
                sf2 = ''.join(sorted(['0'] + [str(x+1) for x in (sofar + [p])]))
                edge = '  ' + sf + ' -> ' + sf2 + '[label="' + str(p) + '"];\n'
                f.write(edge)
                output_edges(sofar + [p], threads)

    prelim = 'strict digraph Explosion {\n  node [shape=circle, fixedsize=true, label=""];\n'
    postlim = '}'
    f.write(prelim)
    output_edges([], int(sys.argv[1]))
    f.write(postlim)
