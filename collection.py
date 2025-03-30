# Script to collect together all the data from each of the MD files and collect it together to produce a single collection document.

# redirect output to "collection.md"

import glob
import time

# Find all .txt files in the current directory and subdirectories
files = glob.glob('**/*.md', recursive=True)
with open("collection.md","w") as c:
    c.write ('# Collection of Motorola MC6800 artefacts as at '+ time.strftime("%d-%m-%Y") + '\n')

    c.write('\n\n')
    c.write('| File | Description | \n') 
    c.write('|----- |------------ |\n')

    for file in files:
        if (file != "README.md" and 
            file != "collection.md" and
            "@" not in file):
            with open(file) as f:
                for line in f:
                    if ':white_check_mark:' in line:
                        splitline = line.split('|')
                        part_number = splitline[1].strip().replace(':white_check_mark:','')
                        description = splitline[2].strip()
                        c.write('|' + part_number + '|' + description + '|\n')
                            