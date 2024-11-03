import sys
import r2pipe


def main():
    tagLabelMap = dict()
    osCFG = dict()
    ciCFG = dict()

    bFile = "dump_table.bin"
    with open(bFile, 'r') as fp:
        rLine = fp.readline()
        while rLine:
            if (rLine[0] == ' ' and len(rLine.strip().split(' ')) >= 7):
                items = rLine.strip().split(' ')
                tag = (str(items[2][::-1]) + str(items[1][::-1]))
                tag = ''.join(
                    [tag[x:x + 2][::-1] for x in range(0, len(tag), 2)])
                label = (str(items[4][::-1]) + str(items[3][::-1]))
                label = ''.join(
                    [label[x:x + 2][::-1] for x in range(0, len(label), 2)])
                tagLabelMap[int(tag, 16)] = int(label, 16)
            rLine = fp.readline()
    fp.close()

    eFile =  "stats.bin"
    with open(eFile, 'r') as fp:
        eLine = fp.readline()
        while eLine:
            items = eLine.strip().split('\t')
            if (items[0] == '1'):
                if (len(items) == 5):
                        key = (items[1], items[3], int(items[4], 10) )
                else:
                        key = (items[1], items[3], 0)
                if (not key in osCFG):
                    osCFG[key] = []
                if(int(items[2], 10) in tagLabelMap):
                    osCFG[key].append(tagLabelMap[int(items[2], 10)])
            if (items[0] == '2'):
                key = (items[1])
                if (not key in ciCFG):
                    ciCFG[key] = []
                ciCFG[key].append(tagLabelMap[int(items[2], 10)])

            eLine = fp.readline() 
    fp.close()


    osFile =  "osCFG.bin"
    # cpoint, origin, originctx, target
    fw = open(osFile, "w")
    for k, v in osCFG.iteritems():
        for item in v:
            fw.write(
                str(k[0]) + '\t' + str(item)  + '\t' + str(k[1]) + '\t' + str(k[2]) + '\n')
    fw.close()

    ciFile =  "ciCFG.bin"
    fw = open(ciFile, "w")
    for k, v in ciCFG.iteritems():
        for item in v:
            fw.write(
                str(k[0]) + '\t' + str(item) + '\n')
    fw.close()


if (__name__ == '__main__'):
    main()
