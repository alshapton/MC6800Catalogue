
with open('left.asm') as file:
    leftlines = [line.rstrip() for line in file]
with open('right.asm') as file:
    rightlines = [line.rstrip() for line in file]

for lines in range(0,189):
    lol=len(leftlines[lines])
    extras=" " * (58-lol)
    print(leftlines[lines] + extras +  ' ; ' + rightlines[lines])
    

exit()
with open('educator-11-rom.asm') as file:
    lines = [line.rstrip() for line in file]
    maxlength=0
    for l in lines:
        lol=len(l)
        if lol > maxlength:
            maxlength=lol
maxlength+=5
with open('educator-11-rom.asm') as file:
    lines = [line.rstrip() for line in file]
    for l in lines:
        lol=len(l)
        extras=" " * (maxlength-lol)
        if l[0:1] == "*":
            out = " " * maxlength
            out = out + "; " + l
        else:    
            out = l + extras + "; " + l

        print(out)
