import datetime
import time

import glob
import os

# Script to collect together all the data from each of the MD files and collect it together to produce a single collection document.


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
                        case "Documents/Hardware/ICs":
                            doc_type = "ICs"
                        case "Documents/Reference":
                            doc_type = "Reference Documents"
                        case "Documents/Manuals":
                            doc_type = "Reference Manuals"
                        case "Documents/Datasheets":
                            doc_type = "Datasheets"
                        case "Documents/ReferenceCards":
                            doc_type = "Reference Cards" 
                        case "Documents/Generic":
                            doc_type = "Generic Documents"
                        case "Software/NonResident":
                            doc_type = "NonResident Software"
                        case "Software/Resident":
                            doc_type = "Resident Software"
                        case "Documents/Hardware/EXORciser":
                            doc_type = "Exorciser Hardware"
                        case "Documents/Hardware/Other":
                            doc_type = "Other Hardware"
                        case _:
                            doc_type = "Other"  
                    
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
            


def do_create():
    print("Enter the following information:")
    product_name = input("  Product name: ")
    product_number = input("  Product number: ")
    product_type = input("  Product Type:\n     (A)pplication Note\n     Reference (C)ard\n     (D)atasheet\n     (G)eneric\n     (I)Cs\n     (M)onitors\n     Ma(n)uals\n     (R)eference\n     (E)XORciser hardware\n     (O)ther hardware\n      : ")
    dotdot = '../../'
    images = dotdot + 'images/'
    match product_type:
        case "A":
            location = "Documents/ApplicationNotes"
        case "R":
            location = "Documents/Reference"
        case "D":
            location = "Documents/Datasheets"
        case "C":
            location = "Documents/ReferenceCards"
        case "M":
            location = "Software/Monitors"
        case "N":
            location = "Documents/Manuals"            
        case "G":
            location = "Documents/Generic"
        case "I":
            location = "Documents/Hardware/ICs"   
            images = dotdot + 'images/Hardware/ICs/'
        case "E":
            location = "Documents/Hardware/EXORciser"
            images = dotdot + 'images/Hardware/EXORciser/'
        case "O":
            location = "Documents/Hardware/Other"
            images = dotdot + 'images/Hardware/Other/'
        case _:
            print("Invalid product type")
            exit() 
    orphan = input("Orphan ? (Y/N): ")
    comments = input("Comments: ")
    acquired = input("Acquired ? (Y/N): ")
    if acquired == "Y":
        acquired = True
        index_entry = '":material-regular:`verified;2em;sd-text-success` :ref:`' + product_number + ' <' + product_number + '>`","' + product_name + '","' + comments + '"' 
        acquired_date = input("Acquired date (DD-MON-YYYY): ")
        acquired_status=":material-regular:`verified;2em;sd-text-success` " + acquired_date + "\n\n"
    else:
        acquired = False
        index_entry = '":ref:`' + product_number + ' <' + product_number + '>`","' + product_name + '","' + comments + '"' 
        acquired_status = ":material-regular:`thumb_down;2em;sd-text-danger`"

    links = input("Links ? (Y/N): ")

    OUTPUT_FILE = f"source/{location}/@{product_number}.rst"
    if os.path.exists(OUTPUT_FILE):
        print(f"File {OUTPUT_FILE} already exists")
        exit()

    print(f"Creating file {OUTPUT_FILE}")
    with open(OUTPUT_FILE,"w") as c:
        if orphan == "Y":
            c.write(':orphan:\n\n')
        c.write('.. _' + product_number + ':\n\n')
        c.write(product_name + '\n')
        for i in product_name:
            c.write('=')
        c.write('\n\n')
        c.write('.. image:: '+ images + product_number + '.png\n')
        c.write('   :width: 400\n')
        c.write('   :align: center\n\n')

        c.write('.. rubric:: Collection Information\n\n')
        c.write('.. csv-table:: \n')
        c.write('   :header: "Acquired"\n')
        c.write('   :widths: auto\n\n')     

        c.write('   ' + acquired_status)

        if links == "Y":
            linkdocument = input("Document Name : ")
            c.write('\n\n.. rubric:: Links\n\n')
            c.write(":download:`" + product_name + " <" + dotdot + "_static/" + location + "/"+ linkdocument + ">`")


    return index_entry
    


def getDateRangeFromWeek(p_year,p_week):

    firstdayofweek = datetime.datetime.strptime(f'{p_year}-W{int(p_week )- 1}-1', "%Y-W%W-%w").date()
    lastdayofweek = firstdayofweek + datetime.timedelta(days=6.9)
    #return firstdayofweek, lastdayofweek
    return firstdayofweek.strftime("%d-%b-%Y").upper(),lastdayofweek.strftime("%d-%b-%Y").upper()

while True:
    print('\t1. Get date range from week')
    print('\t2. Create new entry')
    print('\t3. Update collection')
    print('\t0. Exit')
    type = input('Enter choice: ')
    match type:
        case "1":
            y = input('Enter year: ')
            w = input('Enter week: ')
            #Call function to get dates range 
            firstdate, lastdate =  getDateRangeFromWeek(y,w)
            output = 'Date Range for week ' + str(w) + ' in year ' + str(y) + ' is from ' + firstdate + ' to ' +  lastdate
            print(output)
        case "2":
            index_entry = do_create()
            print(index_entry)
        case "3":
            do_collection()
            print('Collection updated')
            os.system("make clean html")
            
        case "0":
            print('Exiting')
            exit()

        case _:
            print('Invalid choice')
            
        


