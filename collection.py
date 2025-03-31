# Script to collect together all the data from each of the MD files and collect it together to produce a single collection document.

# redirect output to "collection.md"

import glob
import time
import os

# Find all .md files in the current directory and subdirectories
files = glob.glob('**/*.md', recursive=True)
with open("collection.md","w") as c:
    c.write ('# Collection of Motorola MC6800 artefacts as at '+ time.strftime("%d-%m-%Y") + '\n')

    c.write('\n\n')
    c.write('| File | Description | Type |\n') 
    c.write('|----- |------------ |--    |\n')

    for file in files:
        if (file != "README.md" and 
            file != "collection.md" and
            "@" not in file):
            
            with open(file) as f:
                type = os.path.dirname(file)
                match type:
                    case "Documents/Reference":
                        doc_type = "Reference Manual"
                    case "Documents/Datasheets":
                        doc_type = "Datasheet"
                    case "Documents/ReferenceCards":
                        doc_type = "Reference Card" 
                    case "Documents/Generic":
                        doc_type = "Generic Document"
                    case "software":
                        doc_type = "Software"
                    case "Hardware/EXORciser":
                        doc_type = "Hardware"
                    case _:
                        doc_type = " "
                doc_type = doc_type + "|"
                for line in f:
                    if ':white_check_mark:' in line:
                        splitline = line.split('|')
                        part_number = splitline[1].strip().replace(':white_check_mark:','')
                        description = splitline[2].strip()
                        c.write('|' + part_number + '|' + description + '|' + doc_type + '\n')
                            