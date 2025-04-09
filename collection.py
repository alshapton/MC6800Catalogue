# Script to collect together all the data from each of the MD files and collect it together to produce a single collection document.

# redirect output to "collection.md"

import glob
import time
import os


CHECK_MARK=':material-regular:`verified;2em;sd-text-success`'
OUTPUT_FILE = 'source/collection.rst'
SUFFIX = 'rst'
PREFIX ='source/'


def do_collection():
    # Find all .rst files in the current directory and subdirectories
    files = glob.glob('**/*.'+SUFFIX, recursive=True)
    with open(OUTPUT_FILE,"w") as c:

        c.write('.. _collection page:\n\n')
        c.write ('Collection\n')
        c.write('===========')
        c.write('\n')
        c.write('This is the current collection (as at ' + time.strftime("%d-%m-%Y") + ') of the items produced by Motorola in the MC6800 Range of CPUs and their derivatives, support chips and tooling\n')
        c.write('\n\n')
    
        collection=[]


        for file in files:
            if (file not in ("README.md" ,"_static/source/Software/NonResident/software.fragment") and
                "collection" not in file and
                "@" not in file):
                
                with open(file) as f:
                    type = os.path.dirname(file).replace(PREFIX,'')
                    match type:
                        case "Documents/ApplicationNotes":
                            doc_type = "Application Notes"
                        case "Hardware/ICs":
                            doc_type = "ICs"
                        case "Documents/Reference":
                            doc_type = "Reference Manuals"
                        case "Documents/Datasheets":
                            doc_type = "Datasheets"
                        case "Documents/ReferenceCards":
                            doc_type = "Reference Cards" 
                        case "Documents/Generic":
                            doc_type = "Generic Documents"
                        case "Software/NonResident":
                            doc_type = "Software"
                        case "Software/Resident":
                            doc_type = "Software"
                        case "Hardware/EXORciser":
                            doc_type = "Exorciser Hardware"
                        case "Hardware/Other":
                            doc_type = "Other Hardware"
                        case _:
                            doc_type = " "
                    
                    for line in f:
                        if CHECK_MARK in line and 'This item is present in the collection' not in line:
                            splitline = line.split('","')
                            part_number = splitline[0].strip().replace(CHECK_MARK,'').replace('""','"')
                            description = splitline[1].strip().replace('""','"')
                            outline = ('\t' + part_number + '","' + description + '","' + doc_type + '"\n').replace('""','"')
                            thisdict = {"PN"    : part_number, 
                                        "DESC"  : description, 
                                        "DTYPE" : doc_type, 
                                        "OLINE" : outline }
                            collection.append(thisdict)
                            # c.write(outline)
                newlist = sorted(collection, key=lambda d: (d['DTYPE'],d['PN']))  

        HEADING=''

        for i in newlist:
            if HEADING != i['DTYPE']:
                HEADING = i['DTYPE']
                c.write('\n\n.. rubric:: ' + HEADING + '\n\n') 
                c.write('.. csv-table:: \n')
                c.write('\t:header: "Part Number","Description"\n')
                c.write('\t:widths: 30, 70\n\n')  
            c.write(i['OLINE'].replace(',"'+i['DTYPE']+'"\n','\n'))
            
do_collection()