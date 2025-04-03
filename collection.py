# Script to collect together all the data from each of the MD files and collect it together to produce a single collection document.

# redirect output to "collection.md"

import glob
import time
import os

target='sphinx'
#target='github'

CHECK_MARK = ''
OUTPUT_FILE = ''
SUFFIX=''
PREFIX =''
if target=='sphinx':
    # Target is Sphinx
    CHECK_MARK=':material-regular:`verified;2em;sd-text-success`'
    OUTPUT_FILE = 'source/collection.rst'
    SUFFIX = 'rst'
    PREFIX ='source/'
else:
    # Target is Github
    CHECK_MARK = ':white_check_mark:'
    OUTPUT_FILE = 'collection.md'
    SUFFIX = 'md'


# Find all .md files in the current directory and subdirectories
files = glob.glob('**/*.'+SUFFIX, recursive=True)
print(files)
with open(OUTPUT_FILE,"w") as c:
    if target=='sphinx':
        c.write('.. _collection page:\n\n')
        c.write ('Collection of Motorola MC6800 and related artefacts as at '+ time.strftime("%d-%m-%Y") + '\n')
        c.write('====================================================================')
        c.write('\n\n')
        c.write('.. csv-table:: Collection Information\n')
        c.write('\t:header: "Part Number","Description","Type"\n')
        c.write('\t:widths: auto\n\n')
   
    if target=='github':
        c.write ('# Collection of Motorola MC6800 and related artefacts as at '+ time.strftime("%d-%m-%Y") + '\n')
        c.write('\n\n')
        c.write('| Part Number | Description | Type |\n') 
        c.write('|----- |------------ |--    |\n')

    for file in files:
        if (file != "README.md" and 
            "collection" not in file and
            "@" not in file):
            
            with open(file) as f:
                type = os.path.dirname(file).replace(PREFIX,'')
                match type:
                    case "Documents/Reference":
                        doc_type = "Reference Manual"
                    case "Documents/Reference":
                        doc_type = "Reference Manual"
                    case "Documents/Datasheets":
                        doc_type = "Datasheet"
                    case "Documents/ReferenceCards":
                        doc_type = "Reference Card" 
                    case "Documents/Generic":
                        doc_type = "Generic Document"
                    case "Software":
                        doc_type = "Software"
                    case "Hardware/EXORciser":
                        doc_type = "Exorciser Hardware"
                    case "Hardware/Other":
                        doc_type = "Other Hardware"
                    case _:
                        doc_type = " "
                
                for line in f:
                    if CHECK_MARK in line:

                        if target=='sphinx':
                            splitline = line.split('","')
                            part_number = splitline[0].strip().replace(CHECK_MARK,'').replace('""','"')
                            description = splitline[1].strip().replace('""','"')
                            outline = ('\t' + part_number + '","' + description + '","' + doc_type + '"\n').replace('""','"')
                            c.write(outline)
                        if target=='github':
                            doc_type = doc_type + "|"
                            splitline = line.split('|')
                            part_number = splitline[1].strip().replace(CHECK_MARK,'')
                            description = splitline[2].strip()
                            c.write('|' + part_number + '|' + description + '|' + doc_type + '\n')
                    