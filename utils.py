import datetime
import time

import glob
import os
import shutil


import ast


CHECK_MARK=':material-regular:`verified;2em;sd-text-success`'
CROSS_MARK=':material-regular:`thumb_down;2em;sd-text-danger`'
IN_TRANSIT=':material-regular:`local_shipping;2em`'
IN_TRANSIT_SHORT='local_shipping'

OUTPUT_FILE = 'source/collection.rst'
TRANSIT_FILE = 'source/transit.rst'
SUFFIX = 'rst'
PREFIX ='source/'
MOVE='tmp/move'
CAROUSEL='carousel'
NEW_GROUP_TMP_LOC='tmp/'
IC_LOCATIONS = 'source/Documents/Hardware/ICs'


def make_directory(path):
    try:
        os.mkdir(path)
        return True
    except FileExistsError:
        return True
    
def get_loc(file):
    loc = ast.literal_eval('{}')

    filename = file
    got_image=False
    metadata=False
    sta=''
    with open(file) as f:
        for line in f:
            if line.startswith('.. _'):
                ref = line.split('.. _')[1].strip().replace(':','').replace('>','').replace('i','')
                if '.. image:: ' in line and got_image == False:
                    if 'NOIMAGE.png' not in line:
                        image=line.split('.. image::')[1].strip().replace('../../../../i','../../../i')
                        got_image=True         
            if '.. #Metadata' in line:
                metadata=True
                this_loc=line.split('.. #Metadata')[1].strip().replace("{'Info': ",'').replace('}}','}')
                loc = ast.literal_eval(this_loc)
                if ref != '':
                    loc['Ref'] = ref
                if 'Part' not in loc:
                    loc['Part'] = 'N/A'
            
            if CHECK_MARK in line and sta == '':
                sta='YES'
            if CROSS_MARK in line and sta == '':
                sta='NO'
            if IN_TRANSIT in line and sta == '':
                sta='TRANSIT'
            loc['Status'] = sta                        
        return metadata, loc

def do_standard_folders(TABLES_FILE,sorted_folders):
    with open(TABLES_FILE,"w") as c:

        folderloc='0'
        for item in sorted_folders:
            match item['Status']:
                case 'YES':
                    stat = '"' + CHECK_MARK 
                case 'NO':
                    stat = '"' + CROSS_MARK
                case 'TRANSIT':
                    stat = '"' + IN_TRANSIT
                case _:
                    stat = '"N/A'

            if item['Folder'] != folderloc:   
                folder_name='Folder ' + str(item['Folder'])
                if item['Folder'] == 'GITHUB':
                    folder_name='GitHub Repository (See individual items)'
                if item['Folder'] == 'LOCAL':
                    folder_name='See individual items for location information'
                
                folderloc=item['Folder']    
                c.write('\n\n.. rubric:: '+ folder_name + '\n')
                c.write('\n.. csv-table::\n')
                c.write('   :header: "Part Number","Name","Comments"\n')
                c.write('   :widths: 20,80,20 \n')

            c.write('\n   ')
            c.write(stat +' :ref:`' + item['Part'] + ' <'+item['Ref']+ '>`","')
            comments=""
            if "Comments" in item:
                comments = item['Comments']
            c.write(item['Product']+'","'+comments + '"')
        c.write('\n')    

def create_new_group_from_index():
    newgroupname=input("Enter group name: ")
    LOC=NEW_GROUP_TMP_LOC + newgroupname + '.fragment.rst'
    datasheet=input("Include link to datasheet (Y/N): ")
    ds=''
    if datasheet == 'Y':
        ds='\n.. rubric:: Links\n\n'
        ds=ds+':download:`' + newgroupname + ' ' + 'XXXX  <../../../../_static/Documents/Datasheets/' + newgroupname + ".pdf>`\n"
    NEW_LOC=NEW_GROUP_TMP_LOC + newgroupname
    if not os.path.exists(LOC.lower()):
        print('Index file for ' + newgroupname + ' does not exist')
        exit()
    print('Creating new group from index file for IC: ' + newgroupname)

    direc=make_directory(NEW_LOC)

    
    with open(LOC.lower() ,"r") as d:
        lines = d.readlines()

    for line in lines:
        if line.startswith('.. collapse::'):
            group_name = line.split('.. collapse::')[1].strip()
            print('Group name: ' + group_name)
        if not line.startswith('.. collapse::') \
            and line.find('widths') == -1 \
            and line.find('csv-table') == -1 \
            and line.find('header') == -1 \
            and len(line) > 0 : 
            startref=line.find('<')
            endref=line.find('>')
            lengthref=endref - startref
            chip=line[startref+1:endref]
            print(startref,endref)
            print(chip)
            new_file_name = '@' + chip + '.rst'
            new_file = os.path.join(NEW_LOC, new_file_name)
            
            print(new_file)
            if startref == -1 and endref == -1:
                pass
            else:
                print(line)
                info=line.split(',')
                packaging=info[1].strip()
                frequency=info[2].strip()
                temperature=info[3].strip()
                with open(new_file, "w") as c:
                    c.write(':orphan:\n\n')
                    c.write('.. _' + chip + ':\n\n')
                    c.write(".. #None {'Product':'" + chip + "','Storage': 'Storage Box X','Drawer':X,'Row':Y,'Column':Z}\n\n")
                    c.write(chip + ' ' + group_name + '\n')
                    c.write('=' * (len(chip) + len(group_name) + 1) + '\n\n')
                    c.write('.. image:: ../../../../images/NOIMAGE.png\n')
                    c.write('   :width: 400\n')
                    c.write('   :align: center\n\n')
                    c.write('.. rubric:: Specific Information\n\n')
                    c.write('.. csv-table:: \n')
                    c.write('   :widths: auto\n\n')
                    c.write('   "Date Code","TBD"\n')
                    c.write('   "Manufacture Date","TBD"\n')         
                    c.write('   "Packaging",'+packaging+'\n')
                    c.write('   "Status","TBD"\n')
                    c.write('   "Location","TBD"\n')
                    c.write('   "Temperature",'+temperature+'\n')
                    c.write('   "Frequency",'+frequency+'\n')
                    c.write('   "Notes",""\n\n\n')
                    c.write('.. rubric:: Collection Information\n\n')
                    c.write('.. csv-table:: \n')
                    c.write('   :header: "Component","Datasheet"\n') 
                    c.write('   :widths: auto\n\n')
                    c.write('   "'+ CROSS_MARK + '","' + CROSS_MARK + '"\n')
                    ds = ds.replace('XXXX', group_name)
                    c.write(ds)


def create_new_group_index():
    newchipbasename=input("Enter new chip base name (e.g. Asynchronous Adapter): ")
    newgroupname=input("Enter new group name: ")
    chipprefixdefault='MC68'
    chipprefix=input("Enter chip prefix (default by pressing <ENTER> is 'MC68'): ")
    if chipprefix == '':
        chipprefix = chipprefixdefault
    p=input("Enter packaging types (S-CERDIP,P-plastic,L-Ceramic etc)- comma-separated: ")
    packaging=p.split(',')
    chips=[]
    temps=['']
    f=input("Enter extra frequencies (A=1.5 MHz, B=2 MHz) - comma-separated: ")
    frequencies=f.split(',')  
    if len(frequencies) > 0:
        frequencies.append("")
    t=input("Enter extra temperature (C): ")
    if t== 'C':
        temps.append("C")
    if len(frequencies) > 0:
        frequencies.append("")
    
    LOC=NEW_GROUP_TMP_LOC + newgroupname + '.fragment.rst'
    with open(LOC.lower() ,"w") as d:
        for packagetype in packaging:

            for frequency in frequencies:

                for temper in temps:
                    chiptype=packagetype.strip()

                    chip=chipprefix + frequency + newgroupname.replace(chipprefix,'') + temper.strip() + packagetype.strip()
                    if frequency == '':
                        freq = '1 Mhz'
                        chiptype=chiptype + '1'

                    if frequency == 'A':
                        freq = '1.5 Mhz'
                        chiptype=chiptype + '2'
                    if frequency == 'B':
                        freq = '2 Mhz'
                        chiptype=chiptype + '3'
                    
                    if temper.strip() == '':
                        temp = "0-70\\ :sup:`o`\\ C"
                        chiptype=chiptype + '0'

                    if temper.strip() == 'C':
                        temp = "-40-85\\ :sup:`o`\\ C"
                        chiptype=chiptype + '1'

                    pt = ''
                    if packagetype.strip() == 'S':
                        pt = 'CERDIP'
                    if packagetype.strip() == 'P':
                        pt = 'Plastic'
                    if packagetype.strip() == 'L':
                        pt = 'Ceramic'
                    chiptype=chiptype + '|'
                    d.write('       ":material-regular:`thumb_down;2em;sd-text-danger` :ref:`' + chip + ' <' + chip + '>`","'+ pt +'","'+ freq +'","'+temp+'",""\n')

    with open(LOC.lower(), "r") as cf:
        lines = cf.readlines()
    lines = list(set(lines))

    with open(LOC.lower() ,"w") as d:
        d.write('.. collapse::  ' + newchipbasename + '\n\n')
        d.write('   .. csv-table::\n')
        d.write('       :header: "Part Number","Packaging","Frequency","Temperature","Notes" \n')
        d.write('       :widths: auto\n\n')  
        for line in sorted(lines):
            d.write(line)

    print('New group index created in ' + LOC.lower())

def update_carousel():
    files = glob.glob('**/*.'+ CAROUSEL + '.' + SUFFIX, recursive=True)
    for filename in files:   
        i=str(filename)
        images_loc = i.replace('Documents','images').replace('.'+ CAROUSEL + '.' + SUFFIX,'')
        base=os.path.basename(i).replace('.'+CAROUSEL+'.'+SUFFIX,'')
        fullbase = i.replace(os.path.basename(i),'') +  base + os.sep + base + '.'  + CAROUSEL + '.' + SUFFIX
        f=i.count(os.sep)
        dotdot = ''
        for f in range(0,f-1):
            dotdot += '../'
        images_loc_full=dotdot + images_loc.replace('source/','')

        picfiles = os.listdir(images_loc)
        picfiles.sort()


        if ('carousel.properties' in picfiles):
            carouselfile=images_loc + os.sep + 'carousel.properties'
            with open(carouselfile, "r") as cf:
                carousel_properties = cf.readlines()[0]
                cp=ast.literal_eval(carousel_properties)
                cars=cp["Carousels"]
                with open(i ,"w") as d:
                    for car in cars:
                        carousel_number=str(car["Number"])
                        carousel_title=car["Title"]
                        d.write('.. rubric:: ' + carousel_title + '\n\n')
                        d.write('.. card-carousel:: ' + carousel_number + '\n\n')
                        for picfile in picfiles:
                            if picfile.startswith(carousel_number):
                                fullfile=images_loc_full + os.sep + picfile
                                d.write('    .. card::\n\n')
                                d.write('      .. image:: ' + fullfile + '\n')
                                d.write('         :width: 800\n\n')        
        else:
            with open(i ,"w") as d:
                d.write('.. card-carousel:: 2\n\n')
                for picfile in picfiles:
                    if not picfile.startswith('_'):
                        fullfile=images_loc_full + os.sep + picfile
                        d.write('    .. card::\n\n')
                        d.write('      .. image:: ' + fullfile + '\n')
                        d.write('         :width: 800\n\n')
    print('\n\nCarousels updated')

def movefile(old, new):
    shutil.move(old, new)

def get_cols_for_drawer(st,dr,rw, info):
    cols = []

    for i in range(0,len(info)):
        j=ast.literal_eval(info[i])
        k=j['Storage']        
        if k['Name'] == st:
            drws=k['Drawers']
            for d in range(0,len(drws)):
                cd=drws[d]
                if dr == cd['Drawer']:
                    cols = cd['Columns']
    return cols

def update_storage():
    storage=[]
    foldersrefcard=[]
    foldersgeneric=[]
    foldersreference=[]
    folderssoftnon=[]
    folderssoftres=[]
    storage_properties = []
    other_storage = []
    other_products = []
    misc_storage = []

    files = glob.glob('**/*.'+SUFFIX, recursive=True)
    ICLABELSNAME='labels.fragment.rst'
    ICLABELS_FILE='source/Documents/Hardware/ICs/' + ICLABELSNAME
    TABLES_FILE='source/Documents/Hardware/ICs/tables.fragment.rst'
    PROPERTIES_FILE='storage.properties'
    file1 = open(PROPERTIES_FILE, 'r')
    properties = file1.readlines()
    for prop in properties:
        if 'Storage' in prop:
            storage_properties.append(prop)
        if 'Other' in prop:
            other_storage.append(prop)
    file1.close()      
    if len(other_storage) > 0:
        oths = ast.literal_eval(other_storage[0])
        for i in oths["Other"]:
            misc_storage.append(i)
    with open(ICLABELS_FILE,"w") as c:
        for file in files:
            if 'ICs' in file and 'fragment' not in file and 'index' not in file:
                filename = file
                got_image=False
                with open(file) as f:
                    for line in f:
                        if '.. image:: ' in line and got_image == False:
                            if 'NOIMAGE.png' not in line:
                                image=line.split('.. image::')[1].strip().replace('../../../../i','../../../i')
                                got_image=True
                                
                        if '.. #Metadata' in line:
                            this_loc=line.split('.. #Metadata')[1].strip().replace("{'Info': ",'').replace('}}','}')
                            loc = ast.literal_eval(this_loc)
                            is_misc=False
                            for i in oths["Other"]:
                                if loc["Storage"] == i['Name']:
                                    is_misc=True
                            if is_misc:
                                other_products.append(loc)
                            else:
                                storage.append(loc)

                if got_image == True:
                    label = 'i'+filename.split('@')[-1].replace('.rst','')
                    c.write('.. |' + label + '| ' + ' image:: ' + image + '\n')                        
                    c.write('   :width: 200\n')                            
                    c.write('   :class: no-scaled-links\n\n')
            
            if 'ReferenceCards' in file and 'fragment' not in file and 'index' not in file:
                metadata,loc = get_loc(file)                
                if metadata:
                    foldersrefcard.append(loc)

            if 'Generic' in file and 'fragment' not in file and 'index' not in file:
                metadata,loc = get_loc(file)                
                if metadata:
                    foldersgeneric.append(loc)

            if 'Reference' in file and 'ReferenceCards' not in file and 'fragment' not in file and 'index' not in file:
                metadata,loc = get_loc(file)                
                if metadata:
                    foldersreference.append(loc)

            if 'Software/NonResident' in file and 'fragment' not in file and 'index' not in file:
                metadata,loc = get_loc(file)                
                if metadata:
                    folderssoftnon.append(loc)

            if 'Software/Resident' in file and 'fragment' not in file and 'index' not in file:
                metadata,loc = get_loc(file)                
                if metadata:
                    folderssoftres.append(loc)

    sorted_other_products = sorted(other_products, key=lambda x: (x['Storage'],x['Product']))   
    sorted_misc_storage= sorted(misc_storage, key=lambda x: (x['Name'],x['Description']))


    sorted_folders_softres = sorted(folderssoftres, key=lambda x: (x['Folder'],x['Product']))   
    sorted_folders_softnon = sorted(folderssoftnon, key=lambda x: (x['Folder'],x['Product']))   
    sorted_folders_generic = sorted(foldersgeneric, key=lambda x: (x['Folder'],x['Product']))   
    sorted_folders = sorted(foldersrefcard, key=lambda x: (x['Folder'],x['Product']))   
    sorted_folders_reference =sorted(foldersreference, key=lambda x: (x['Folder'],x['Product']))   
    sorted_storage = sorted(storage, key=lambda x: (x['Storage'],x['Drawer'],x['Row'],x['Column']))   
    
    storagebox=''
    drawer=0
    row=0
    column=0
    colcount=0
    with open(TABLES_FILE,"w") as c:

        c.write('.. include:: ./' + ICLABELSNAME)

        for item in sorted_storage:
            if item['Storage'] != storagebox:   
                c.write('\n\n.. rubric:: ' + item['Storage'])
                storagebox=item['Storage']
                drawer=0
                row=0
                column=0
                rowcnt=0
                cols=0
            if item['Drawer'] != drawer:
                if cols > colcount:
                    for i in range(colcount,cols):
                        c.write(',""')
                c.write('\n\n.. collapse:: Drawer ' + str(item['Drawer']) + '\n')
                c.write('\n    .. csv-table::\n')
                c.write('       :header-rows: 0\n')
                cols = get_cols_for_drawer(item['Storage'],item['Drawer'],item['Row'],storage_properties)
                c.write('       :widths: ' + str(cols).replace('[','').replace(']','') +'\n')
                drawer=item['Drawer']
                row=0
                column=0
                first_row = True
            if item['Row'] != row:
                rowcnt += 1
                cols = len(get_cols_for_drawer(item['Storage'],item['Drawer'],item['Row'],storage_properties))
                    
                rowcnt = 0
                if first_row == True:
                    c.write('\n       ')
                    first_row = False
                else:
                    if cols > colcount:
                        for i in range(colcount,cols):
                            c.write(',""')
                    c.write('\n       ')
                            
                row=item['Row']
                column=0
                colcount=0
            if item['Column'] != column:
                prod = item['Product'].strip()
                if item['Column'] == 1:
                    comma = ''
                else:
                    comma = ','
                c.write(comma + '"|i' + prod + '| :ref:`'+ prod + ' <' + prod + '>`"')
                column=item['Column']
                colcount += 1
        if cols > colcount:
            for i in range(colcount,cols):
                c.write(',""')                

    # Append here the Other Storage stuff
        
        if len(other_storage) > 0:
            l=ast.literal_eval(other_storage[0])
            for i in l["Other"]:
                written_title = False

                for j in other_products:

                    if j["Storage"] == i['Name']:
                        if not written_title:
                            c.write('\n\n.. collapse:: ' + i['Description'] + '\n\n')

                            c.write('    .. csv-table::\n')
                            c.write('       :header-rows: 0\n')
                            c.write('       :widths: 50,50\n\n')
                                

                            written_title = True
                        c.write('         |i' + j["Product"] + '|, :ref:`'+ j["Product"] + ' ' + j["Name"] +' <' + j["Product"] +'>`\n')


    #exit()
    print('\nStorage updated')      

    TABLES_FILE='source/Documents/ReferenceCards/tables.fragment.rst'
    do_standard_folders(TABLES_FILE,sorted_folders)

    TABLES_FILE='source/Documents/Generic/tables.fragment.rst'
    do_standard_folders(TABLES_FILE,sorted_folders_generic)

    TABLES_FILE='source/Documents/Reference/tables.fragment.rst'
    do_standard_folders(TABLES_FILE,sorted_folders_reference)
    
    TABLES_FILE='source/Software/NonResident/tables.fragment.rst'
    do_standard_folders(TABLES_FILE,sorted_folders_softnon)

    TABLES_FILE='source/Software/Resident/tables.fragment.rst'
    do_standard_folders(TABLES_FILE,sorted_folders_softres)

    print('\nFolders updated')      

    print('\nLocations fully updated')      

def do_in_transit():
    # Find all .rst files in the current directory and subdirectories
    files = glob.glob('**/*.'+SUFFIX, recursive=True)
    with open(TRANSIT_FILE,"w") as c:

        c.write('.. _transit page:\n\n')
        c.write ('In-Transit\n')
        c.write('===========')
        c.write('\n')
        c.write('This is the current set of items (as at ' + time.strftime("%d-%m-%Y") + ') in transit.\n')
        
    
        intransit=[]


        for file in files:
            if (file not in ("README.md" ,"_static/source/Software/NonResident/software.fragment") and
                "transit.rst" not in file and
                "@" not in file and "carousel" not in file):
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
                            if "/ICs" in type:
                                doc_type = "ICs"
                    
                    for line in f:
                        if IN_TRANSIT_SHORT in line and 'This item is present in the collection' not in line and "Meta" not in line:
                            if 'An item in transit' not in line:
                                splitline = line.split('","')
                                part_number = splitline[0].strip().replace(IN_TRANSIT,'').replace('""','"')
                                try:
                                    description = splitline[1].strip().replace('""','"')
                                except:
                                    description = ''
                                if doc_type == 'ICs':
                                    description = 'ICSTUFF'
                                    cfile=part_number.replace('" :ref:`','').split(' ')[0] + '.' + SUFFIX
                                    chip_file = glob.glob(IC_LOCATIONS + '/**/*' + cfile, recursive=True)[0]
                                    ch=cfile.replace('.' + SUFFIX,'')
                                    this_chip_file = open(chip_file,'r')
                                    lines = this_chip_file.readlines()
                                    filtered = [line for line in lines if line.startswith(ch)]
                                    
                                    description = filtered[0].replace(ch,'').replace('\n','')


                                outline = ('\t' + part_number + '","' + description + '","' + doc_type + '"\n').replace('""','"')
                                
                                thisdict = {"PN"    : part_number, 
                                            "DESC"  : description, 
                                            "DTYPE" : doc_type, 
                                            "OLINE" : outline }
                                if description != '' :
                                    intransit.append(thisdict)
                newlist = sorted(intransit, key=lambda d: (d['DTYPE'],d['PN']))  
        HEADING=''

        for i in newlist:
            if HEADING != i['DTYPE']:
                HEADING = i['DTYPE']
                c.write('\n\n.. rubric:: ' + HEADING + '\n\n') 
                c.write('.. csv-table:: \n')
                c.write('\t:header: "Part Number","Description"\n')
                c.write('\t:widths: 30, 70\n\n')  
            c.write(i['OLINE'].replace(',"'+i['DTYPE']+'"\n','\n'))

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
                "collection" not in file and "transit.rst" not in file and
                "@" not in file and "carousel" not in file):
                
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
                            if "/ICs" in type:
                                doc_type = "ICs"
                    
                    for line in f:
                        if CHECK_MARK in line and 'This item is present in the collection' not in line:
                            
                            splitline = line.split('","')
                            part_number = splitline[0].strip().replace(CHECK_MARK,'').replace('""','"')
                            try:
                                description = splitline[1].strip().replace('""','"')
                            except:
                                description = ''
                            outline = ('\t' + part_number + '","' + description + '","' + doc_type + '"\n').replace('""','"')
                            thisdict = {"PN"    : part_number, 
                                        "DESC"  : description, 
                                        "DTYPE" : doc_type, 
                                        "OLINE" : outline }
                            if description != '':
                                collection.append(thisdict)
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
            images = dotdot + 'images/DataSheets/'
        case "C":
            location = "Documents/ReferenceCards"
        case "M":
            location = "Software/Monitors"
        case "N":
            location = "Documents/Manuals"            
            images = dotdot + 'images/Manuals/'
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

    if links == "Y":
        linkdocument = input("Document Name : ")

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

        original_document = ''
        if links == "Y":
            c.write('\n\n.. rubric:: Links\n\n')
            target_document =  dotdot + '_static/' + location + "/"+ linkdocument
            c.write(":download:`" + product_name + " <" + target_document+ ">`")
            original_document = MOVE + '/' + linkdocument
        original_image = MOVE + '/' + product_number + '.png'
        target_image = images.replace(dotdot,'source/') + product_number + '.png'
        print('Ready to move.....')
        
        if links =="Y":
            target_document =  "source/_static/" + location + "/"+ linkdocument
            movefile(original_document, target_document)
            
        movefile(original_image, target_image)
        print('Moved images and source data')


    return index_entry
    


def getDateRangeFromWeek(p_year,p_week):

    firstdayofweek = datetime.datetime.strptime(f'{p_year}-W{int(p_week )- 1}-1', "%Y-W%W-%w").date()
    lastdayofweek = firstdayofweek + datetime.timedelta(days=6.9)
    #return firstdayofweek, lastdayofweek
    return firstdayofweek.strftime("%d-%b-%Y").upper(),lastdayofweek.strftime("%d-%b-%Y").upper()

while True:
    print('\t1. Get date range from week')
    print('\t2. Create new entry')
    print('\t3. Create new IC group index')    
    print('\t4. Create new IC group from index')    
    print('\t5. Update storage + SOME indexes')
    print('\t6. Update carousels')
    print('\t0. Update ALL ')
    print('\tX. Exit')
    type = input('Enter choice: ')
    match type:
        case "1":

            #Get year and week from user
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
            create_new_group_index()
        case "4":
            create_new_group_from_index()
        
        case "0":
            update_carousel()
            update_storage()
            do_collection()
            print('Collection updated')
            do_in_transit()
            print('In-Transit updated')
            os.system("make clean html")
        case "5":
            update_storage()
            #os.system("make clean html")
        case "6":
            update_carousel()
            #os.system("make clean html")
        
        case "X":
            print('Exiting')
            exit()
        case "x":
            print('Exiting')
            exit()

        case _:
            print('Invalid choice')
            
        


