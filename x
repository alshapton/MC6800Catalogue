find . -name *.rst -print | grep @ >tmp1
#echo "./source/Documents/Hardware/ICs/MC6888/@MC6888L.rst" > tmp1
#cat tmp1 | awk  '{print  "sed \'s/:material-regular\:!thumb_down;2em\;sd-text-danger!/\|notpresent\|/g\' " }' 
cat tmp1 | awk  '{print "sed !s/:material-regular:\`thumb_down;2em;sd-text-danger\`/|notpresent|/g! " $1 " > fred.tmp;mv fred.tmp " $1}' | tr ['!'] ["'"] >x2



