import time
# Import the filecmp module for file comparison    
import filecmp  
import glob
import os

import ast
import tomllib

from xbuild_support.functions import *
from xbuild_support.file_utilities import *

import sqlite3


CHECK_MARK    = '|present|'
CROSS_MARK    = '|notpresent|'
IN_TRANSIT    = '|intransit|'
UNDER_OFFER   = '|underoffer|'
BIGGER_DOC    = '|document|'

OSSEP         = os.sep

ANYUNDEROFFER = False
ANYINTRANSIT  = False


PREFIX ='source' + OSSEP
SUFFIX = 'rst'

OUTPUT_FILE = PREFIX + 'collection.' + SUFFIX
TRANSIT_FILE = PREFIX + 'transit.' + SUFFIX
UNDEROFFER_FILE = PREFIX + 'underoffer.' + SUFFIX

MOVE='tmp'  + OSSEP + 'move'
CAROUSEL='carousel'
NEW_GROUP_TMP_LOC='tmp' + OSSEP
IC_LOCATIONS = 'source'  + OSSEP + 'Documents'  + OSSEP + 'Hardware'  + OSSEP + 'ICs'


def update_or_not_metadata(filename):    

    if 'carousel' in filename or 'fragment' in filename or 'index' in filename or 'collection' in filename or 'map' in filename or 'transit' in filename:
        return
    diff_file = filename +'.new'
    with open(diff_file, 'w') as newfile:
        with open(filename, 'r') as file:
            found_metadata=False
            metadata_dict=''
            lines = file.readlines()
            
            for line in lines: 
                output_line = line       
                if '.. #Metadata' in line:
                    metadataline = line.replace('.. #Metadata ', '').strip()
                    print(filename)
                    print(metadataline)
                    metadata_dict = eval(metadataline)
                    metadata_line=str(metadata_dict).split(',')
                    if metadata_dict != '':
                    
                        if 'Drawer' in metadata_dict:
                            found_metadata=True

                            storage = metadata_dict['Storage']
                            drawer = metadata_dict['Drawer']
                            row = metadata_dict['Row']
                            column = metadata_dict['Column']
                            sb=storage.replace(' ', '_')
                            dr=str(drawer).replace(' ', '_')
                            reference='<' + sb + '_Drawer_' + dr + '>'
                            metadata_line= f":ref:`{storage}, Drawer {drawer}, Row {row}, Column {column} {reference}`"
                            location_line = '   "Location","' + metadata_line + '"'

                if '"Location",' in line and 'Drawer' in metadata_dict and location_line == '   "Location",":ref:`S, Drawer 0, Row 0, Column 0 <S_Drawer_0>`"':
                    output_line = '   "Location","TBD"\n' 
                else:
                    output_line = line
                newfile.write(output_line)

    # Always write a .new version of the file, compare it with the original file
    # using https://docs.python.org/3/library/filecmp.html
    # If the files are different, copy the .new to the original 

    # Clear filecmp cache
    filecmp.clear_cache()   

    # Compare the binary contents of 'input_file1.txt' and 'input_file2.txt'
    isdifferent=filecmp.cmp(filename, diff_file,shallow=False)
    
    if not isdifferent:  
        copy_and_replace(diff_file, filename)

    os.remove(diff_file)

def get_loc(file):
    loc = ast.literal_eval('{}')

    got_image=False
    metadata=False
    sta=''
    with open(file) as f:
        for line in f:
            if line.startswith('.. _'):
                ref = line.split('.. _')[1].strip().replace(':','').replace('>','').replace('i','')
                if '.. image:: ' in line and got_image == False:
                    if 'NOIMAGE.png' not in line:
                        image=line.split('.. image::')[1].strip().replace('../../../../i','../../../i').replace('/',OSSEP)
                        got_image=True         
            if '.. #Metadata' in line:
                metadata=True
                this_loc=line.split('.. #Metadata')[1].strip().replace("{'Info': ",'').replace('}}','}')
                loc = ast.literal_eval(this_loc)
                if ref != '':
                    loc['Ref'] = ref
                if 'Part' not in loc:
                    loc['Part'] = 'N/A'
            
            if CHECK_MARK in line  and sta == '':
                sta='YES'
            if CROSS_MARK in line and sta == '':
                sta='NO'
            if IN_TRANSIT in line and sta == '':
                sta='TRANSIT'
            if UNDER_OFFER in line and sta == '':
                print(file)
                sta='UNDEROFFER'
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
                case 'UNDEROFFER':
                    stat = '"' + UNDER_OFFER
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
            comments = ""
            if "Comments" in item:
                comments = item['Comments']
            c.write(item['Product'] + '","' + comments + '"')
        c.write('\n')    


def create_new_group_from_index():
    newgroupname=input("Enter group name: ")
    LOC=NEW_GROUP_TMP_LOC + newgroupname + '.fragment.rst'
    datasheet=input("Include link to datasheet (Y/N): ")
    ds=''
    if datasheet == 'Y':
        ds='\n.. rubric:: Links\n\n'
        ds=ds+':download:`' + newgroupname + ' ' + 'XXXX  <../../../../_static/Documents/Datasheets/' + newgroupname + ".pdf>`\n"
    NEW_LOC = NEW_GROUP_TMP_LOC + newgroupname
    if not os.path.exists(LOC.lower()):
        print('Index file for ' + newgroupname + ' does not exist')
        exit()
    print('Creating new group from index file for IC: ' + newgroupname)

    _ = make_directory(NEW_LOC)

    
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
            
            new_file_name = '@' + chip + '.rst'
            new_file = os.path.join(NEW_LOC, new_file_name)
            
            if startref == -1 and endref == -1:
                pass
            else:
                
                info=line.split(',')
                packaging=info[1].strip()
                frequency=info[2].strip()
                temperature=info[3].strip()
                with open(new_file, "w") as c:
                    c.write(':orphan:\n\n')
                    c.write('.. _' + chip + ':\n\n')
                    c.write(".. #Metadata {'Product':'" + chip + "','Storage': 'Storage Box X','Drawer':0,'Row':0,'Column':0}\n\n")
                    c.write(chip + ' ' + group_name + '\n')
                    c.write('=' * (len(chip) + len(group_name) + 1) + '\n\n')
                    c.write('.. image:: ..!..!..!..!images!NOIMAGE.png\n'.replace('!',OSSEP))
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
                    c.write('   :header: "Component"\n') 
                    c.write('   :widths: auto\n\n')
                    c.write('   "'+ CROSS_MARK + '"\n')
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
    
    LOC = NEW_GROUP_TMP_LOC + newgroupname + '.fragment.rst'
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
                    d.write('       "|notpresent| :ref:`' + chip + ' <' + chip + '>`","'+ pt +'","'+ freq +'","'+temp+'",""\n')

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

def update_IC_pre_fragments():
    print('Updating IC pre-fragments')

    yfiles = glob.glob('**/*.pre.fragment', recursive=True)
    for yfile in yfiles:
        #print('     Processing:' + os.path.basename(yfile))

        with open(yfile) as f:
            lines = f.readlines()
            
        with open(yfile) as f:
            data = ''
            line=f.readline()
            while line:
                data +=line
                line = f.readline()
            
            ttop=''
            bbottom=''

            bottom = data.split('##BOTTOM')
            if len(bottom)>1:
                bbottom=bottom[1]

            top = data.split('##TOP')
            if len(top)>1:
                ttop=top[1]
            
        outputfile=os.path.dirname(yfile) + OSSEP + os.path.basename(yfile).replace('pre','new')+ '.' + SUFFIX
        with open(outputfile,'w') as op:
            op.write('\n.. collapse:: ' + lines[0] + '\n\n')
            if len(ttop)>1:
                op.write(ttop)
            op.write('   .. csv-table::\n')
            op.write('      :header: "Part","Packaging","Freq","Temp","Notes"\n')
            op.write('      :widths: auto\n\n')

            targetfiles=os.listdir(os.path.dirname(yfile))
            for tfile in targetfiles:
                if 'fragment' not in tfile :
                    fragfile = os.path.dirname(yfile) + OSSEP + tfile
                    with open(fragfile,'r') as ff: 
                        fflines = ff.readlines()
                    packaging=""
                    frequency=""
                    temperature=""
                    notes=""
                    tag=""
                    posess=""
                    for ffline in fflines:
                        if ffline.startswith('.. _'):
                            tag=ffline.replace('.. _','').split(':')[0]
                        if 'Packaging' in ffline:
                            splitline=ffline.split(',')
                            packaging=splitline[1].replace('"','')[:-1]
                        if 'Frequency' in ffline:
                            splitline=ffline.split(',')
                            frequency=splitline[1].replace('"','')[:-1]
                        if 'Temperature' in ffline:
                            splitline=ffline.split(',')
                            temperature=splitline[1].replace('"','')[:-1]
                        if 'Notes' in ffline:
                            splitline=ffline.split('","')
                            notes=splitline[1].replace('"','')[:-1]
                        if 'thumb_down' in ffline:
                            posess='|notpresent|'
                        if '|notpresent|' in ffline:
                            posess ='|notpresent|'
                        if '|underoffer|' in ffline:
                            posess='|underoffer|'
                        if '|intransit|' in ffline:
                            posess='|intransit|'                    
                        if '|present|' in ffline:
                            posess='|present|'


                    tref=tfile.replace('@','').replace('.rst','').split('.')[0]
                    if '!' in tref:
                        tref=tref.split('!')[1]
                    fileref='"' + posess + ' :ref:`'+ tref + ' <' + tag + '>`","'+packaging+'","'+frequency+'","'+temperature+'","'+notes+'"'
                    op.write('      '+fileref+'\n')  
            if len(bbottom)>1:
                op.write(bbottom)
            fragmentfile=os.path.dirname(yfile) + OSSEP + os.path.basename(yfile).replace('.pre','')+'.rst'

            movefile(outputfile, fragmentfile)

    print('Finished updating IC pre-fragments')

def update_carousel():
    files = glob.glob('**/*.'+ CAROUSEL + '.' + SUFFIX, recursive=True)
    for filename in files:   
        i=str(filename)
        images_loc = i.replace('Documents','images').replace('.'+ CAROUSEL + '.' + SUFFIX,'')
        base = os.path.basename(i).replace('.' + CAROUSEL + '.' + SUFFIX,'')
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





def update_storage():
    print('Cleaning and prepping storage files')
    snippetfiles = glob.glob('**/*.snippet', recursive=True)
    for snippetfile in snippetfiles:
        os.remove(snippetfile)
        
    storage=[]
    foldersrefcard=[]
    foldersgeneric=[]
    foldersdatasheets=[]
    foldersmanuals=[]
    foldersreference=[]
    folderssoftnon=[]
    folderssoftres=[]
    foldersappnotes=[]
    storage_properties = []
    other_storage = []
    other_products = []
    misc_storage = []

    files = glob.glob('**/*.'+SUFFIX, recursive=True)
    ICLABELSNAME='labels.fragment.rst'
    ICLABELS_LOC='source/Documents/Hardware/ICs/'
    ICLABELS_FILE=ICLABELS_LOC + ICLABELSNAME
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
                                image=line.split('.. image:: ')[1].strip().replace('../','').replace('images','/images')
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
                    c.write('.. |' + label + '|  image:: ' + image + '\n')                        
                    c.write('   :width: 200\n')                            
                    c.write('   :class: no-scaled-links\n\n')
            
            if 'ReferenceCards' in file and 'fragment' not in file and 'index' not in file:
                metadata,loc = get_loc(file)                
                if metadata:
                    foldersrefcard.append(loc)
            
            if 'Manuals' in file and 'fragment' not in file and 'index' not in file:
                metadata,loc = get_loc(file)                
                if metadata:
                    foldersmanuals.append(loc)

            if 'Datasheets' in file and 'fragment' not in file and 'index' not in file:
                metadata,loc = get_loc(file)                
                if metadata:
                    foldersdatasheets.append(loc)

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

            if 'ApplicationNotes' in file and  'fragment' not in file and 'index' not in file:
                metadata,loc = get_loc(file)   
                if metadata:
                    foldersappnotes.append(loc)

    sorted_folders_datasheets = sorted(foldersdatasheets, key=lambda x: (x['Folder'],x['Product']))   
    sorted_folders_manuals = sorted(foldersmanuals, key=lambda x: (x['Folder'],x['Product']))   
    sorted_folders_appnotes = sorted(foldersappnotes, key=lambda x: (x['Folder'],x['Product']))   
    sorted_folders_softres = sorted(folderssoftres, key=lambda x: (x['Folder'],x['Product']))   
    sorted_folders_softnon = sorted(folderssoftnon, key=lambda x: (x['Folder'],x['Product']))   
    sorted_folders_generic = sorted(foldersgeneric, key=lambda x: (x['Folder'],x['Product']))   
    sorted_folders = sorted(foldersrefcard, key=lambda x: (x['Folder'],x['Product']))   
    sorted_folders_reference =sorted(foldersreference, key=lambda x: (x['Folder'],x['Product']))   
    sorted_storage = sorted(storage, key=lambda x: (x['Storage'],x['Drawer'],x['Row'],x['Column']))   

    all_folders_storage_sorted =  sorted(sorted_folders_manuals + sorted_folders_datasheets+sorted_folders_appnotes + sorted_folders_softres + sorted_folders_softnon + sorted_folders_generic + sorted_folders + sorted_folders_reference, key=lambda x: (x['Folder'],x['Product']))
    print(sorted_folders_softnon)
    FOLDER_MAP_FILE = 'source'+ OSSEP + 'Documents' + OSSEP + 'folder.map'
    current_folder=''
    with open(FOLDER_MAP_FILE, "w") as fmf:

        for item in all_folders_storage_sorted:
            if item['Folder'] != current_folder:
                write_folder=False
                map_reference = '\n\n.. _' + item['Folder'].replace(' ','_') + '_map_reference:'
                current_folder = item['Folder']

                print('Processing folder: ' + current_folder)
                match item['Folder']:
                    case "GITHUB":
                        write_folder=True
                        fmf.write(map_reference + '\n\n.. rubric:: GitHub Repository (See individual items)\n\n')
                    case "LOCAL":
                        write_folder=True
                        fmf.write(map_reference + '\n\n.. rubric:: See individual items for location information\n\n')
                    case "None":
                        write_folder=False
                    case "In Transit":
                        write_folder=False
                    case _:
                        write_folder=True
                        if '<' not in item['Folder']:
                            fmf.write(map_reference + '\n\n.. rubric:: Folder ' + current_folder + '\n\n')
            
                if write_folder:
                    if '<' in item['Folder']:
                        write_folder=False
                        pass
                    else:
                        fmf.write('.. csv-table::\n')
                        fmf.write('   :header: "Name","Comments"\n')
                        fmf.write('   :widths: 60,40\n\n')
                            
            if write_folder:
                
                fmf.write('    :ref:`' + item['Product'] + ' <' + item['Ref'] + '>`,"')
                if "Comments" in item:
                    fmf.write(item['Comments'] + '"\n')
                else:
                    fmf.write('"\n')
    print('Folder map created in ' + FOLDER_MAP_FILE)


    # Write the labels file
        

    storagebox=''
    drawer=-1
    row=0
    column=0
    colcount=0
    with open(TABLES_FILE,"w") as c:

        c.write('.. include:: .' + OSSEP + ICLABELSNAME)

        for item in sorted_storage:
            if item['Storage'] != storagebox:   
                c.write('\n\n.. #LVL1 ' + item['Storage'])

                c.write('\n\n.. rubric:: ' + item['Storage'])
                storagebox=item['Storage']
                drawer=-1
                row=0
                column=0
                rowcnt=0
                cols=0

            if item['Drawer'] != drawer:

                if cols > colcount:
                    for i in range(colcount,cols):
                        c.write(',""')
                c.write('\n\n.. #LVL2 ' + str(item['Drawer']))

                c.write(construct_drawer_ref(item['Storage'], item['Drawer']))
                c.write('\n\n.. collapse:: Drawer ' + str(item['Drawer']) + '\n') # HERE <a name="chapter4"></a>
            
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
                if cols == []:  
                    print('No columns found for Storage:', item['Storage'], 'Drawer:', item['Drawer'], 'Row:', item['Row'])
                    exit()  
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
                if '!' in prod:
                    prodref = prod.split('!')[1].strip()
                else:
                    prodref=prod
                c.write(comma + '"|i' + prod + '| :ref:`'+ prodref + ' <' + prod + '>`"')
                column=item['Column']
                colcount += 1
        if cols > colcount:
            for i in range(colcount,cols):
                c.write(',""')                

    # Append here the Other Storage stuff
        
        if len(other_storage) > 0:
            l=ast.literal_eval(other_storage[0])
            for i in l["Other"]:
                c.write('\n\n.. #LVL1 ' + i['Description'])
                written_title = False

                for j in other_products:

                    if j["Storage"] == i['Name']:
                        if not written_title:
                            c.write('\n\n.. #LVL2 ' + i['Description'])
                            

                            c.write('\n\n.. collapse:: ' + i['Description'] + '\n\n')

                            c.write('.. csv-table::\n')
                            c.write('   :header-rows: 0\n')
                            c.write('   :widths: 50,50\n\n')
                                

                            written_title = True
                        c.write('       |i' + j["Product"] + '|, :ref:`'+ j["Product"] + ' ' + j["Name"] +' <' + j["Product"] +'>`\n')

    print('Splitting')
    print('LVL1 Splitting')

    with open(TABLES_FILE,"r") as tf:
        data = ''
        line=tf.readline()
        while line:
            data +=line
            line=tf.readline()
    f = data.split('#LVL1')
    cnt=0
    for r in f:
        FN=str(cnt)
        if cnt > 0:
            # Don't bother writing the first component of the split file - irrelevant
            with open(TABLES_FILE + '.' + FN + ".tiny","w") as ci:
                ci.write(r)
        cnt=cnt+1

    print('LVL1 Splitting done')
    # Checking for LVL2 split required.
    print('LVL2 Splitting')

    lvl2files = glob.glob('**/*.tiny', recursive=True)
    properlvl2files=[]
    for lvl2file in lvl2files:
        with open(lvl2file, 'r') as lvl2f:
            file_content = lvl2f.read()
        if file_content.count('LVL2') > 0:
            properlvl2files.append(lvl2file)
    # Now we have a list of files that need splitting again
    for lvl2file in properlvl2files:
        with open(lvl2file,"r") as tf:
            data = ''
            line=tf.readline()
            storagename=line
            while line:
                data +=line
                line = tf.readline()
            f = data.split('#LVL2')
            sname = storagename[1:].replace(' ','_').strip()
            outputfile_base = lvl2file.replace('.tiny','').replace('rst.','')[:-2] + '.' + sname            
            for i in range(1,len(f)):
                minimum = f[i][:-3]
                start = f[i].find('.. collapse::')
                stripped = minimum[start:]
                LOCATIONINSTORAGE=stripped.split('..')[1].strip().replace('collapse:: ','').replace(' ','_')
                if LOCATIONINSTORAGE is  None:
                    LOCATIONINSTORAGE = 'Unknown'
                print('     Processing location: ' + LOCATIONINSTORAGE)
                outputfile = outputfile_base + '.' + LOCATIONINSTORAGE + '.snippet'
                with open(outputfile,"w") as opf:
                    if not stripped.endswith('`>\n'):
                        stripped = stripped + '>`\n'
                    opf.write(stripped)

    # Remove specific known problematic file               
    os.remove('source/Documents/Hardware/ICs/tables.fragment.S.Drawer_0.snippet')                    
    
    # Remove temporary "tiny" working files
    print('Splitting LVL2 files complete')
    print('Removing temporary files')
    for lvl2file in sorted(lvl2files):
        os.remove(lvl2file)
        print('     ' + os.path.basename(lvl2file) + ' removed.')       
    
    snippetfiles = glob.glob('**/*.snippet', recursive=True)
    
    # Changing collapsing into rubrics
    print('Changing collapsing into rubrics')
    for snippetfile in sorted(snippetfiles):
        with open(snippetfile, 'r') as fd:
            content = fd.readlines()
        with open(snippetfile, 'w') as fw:
            for wl in range (0,len(content)):
                fw.write(content[wl].replace('collapse','rubric').replace('    ',''))

        with open(snippetfile) as file_in:
            lines = []
            for line in file_in:
                lines.append(line)
        
        with open(snippetfile, 'w') as fw:
            for pl in lines:
                if not pl.startswith('>'):
                    fw.write(pl)                    
                    


        print('     ' + os.path.basename(snippetfile) + ' updated.')
    
    # Move snippets into snippets folder
    print('Moving snippets into snippets folder')
    snippeticindexfile=IC_LOCATIONS + OSSEP + 'icindex.snippet'
    print('Snippet index file: ' + snippeticindexfile)
    HDR=''
    with open(snippeticindexfile, 'w') as sicif:

        sicif.write('.. include:: Documents!Hardware!ICs!labels.fragment.rst\n\n'.replace('!',OSSEP))
        for snippetfile in sorted(snippetfiles):
            # Formulate tag for start of file
            PREAMBLE='\n\n.. _' + os.path.basename(snippetfile.replace('tables.fragment.','').replace('.snippet','')).replace('.','_') + ':'+'\n\n'
            

            ISOPREAMBLE=PREAMBLE.replace('.. _','').replace(':','').strip()
            POSS_ISOPREAMBLE = ISOPREAMBLE[:int(len(ISOPREAMBLE)/2)] + '_' + ISOPREAMBLE[:int(len(ISOPREAMBLE)/2)]
                

            _=line_prepender(snippetfile, PREAMBLE)
            movefile(snippetfile,  os.path.dirname(snippetfile) + OSSEP + 'snippets' + OSSEP + os.path.basename(snippetfile))
            SF=snippetfile.replace(IC_LOCATIONS + OSSEP + 'tables.fragment.','').replace('.snippet','')
            if POSS_ISOPREAMBLE == ISOPREAMBLE:
                print('     Duplicate tag detected: ' + ISOPREAMBLE)
                SFHEADER=''
            else:
                SFHEADER='.. rubric:: '+ SF.split('.')[0].replace('_',' ')
            if SFHEADER != HDR:
                sicif.write(SFHEADER + '\n\n')
                HDR = SFHEADER
    


            sicif.write('.. include:: Documents'+OSSEP+'Hardware'+OSSEP+'ICs'+OSSEP+'snippets'+OSSEP+ os.path.basename(snippetfile) + '\n\n')

            print('     Moved ' + os.path.basename(snippetfile) + ' to ' + 'snippets')


    print('Cleaning up')
    os.remove(TABLES_FILE)

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

def update_IC_index():
    print('\nUpdating IC index')
    files = glob.glob('**/MC*', recursive=True)
    icfiles=[]
    ic2file=[]
    memfiles=[]
    for file in files:
            if 'source/Documents/Hardware/ICs/' in file:

                fname=file.replace('source/Documents/Hardware/ICs/','')
                if 'MCM' in fname:
                    memfiles.append(fname[3:])
                    icfiles.append(fname[3:])
                else:
                    if len(str(fname[2:])) >4: 
                        ic2file.append(fname[2:])    
                    else:
                        icfiles.append(fname[2:])

    nonnumics2 = []
    for nonnumic in ic2file:
        if nonnumic.isnumeric():
            nonnumics2.append(nonnumic)
        else:
            icfiles.append(nonnumic)

    ics = sorted(icfiles) + sorted(nonnumics2)

    chips=[]
    for chip in ics:
        if chip in memfiles:       
            chips.append('MCM' + chip)
        else:
            chips.append('MC' + chip)
    
    IC_FRAGMENTS_INDEX=IC_LOCATIONS + OSSEP + 'icindex.fragment.rst'
    with open(IC_FRAGMENTS_INDEX,"w") as c:

        for chip in chips:
            c.write('\n.. include:: .' + OSSEP + chip + OSSEP + chip.lower() + '.fragment.rst\n|\n')

    print('\nCompleted updating IC index\n')


def do_underoffer():
    ANYUNDEROFFER=False
    # Find all .rst files in the current directory and subdirectories
    files = glob.glob('source/**/*.fragment.'+SUFFIX, recursive=True)
    with open(UNDEROFFER_FILE,"w") as c:
        c.write(':orphan:\n\n')
        c.write('.. _underoffer:\n\n')
        c.write ('Under Offer\n')
        c.write('===========')
        c.write('\n')
        c.write('This is the current set of items (as at ' + time.strftime("%d-%m-%Y") + ') under offer.\n')
        
    
        underoffer=[]


        for file in files:
            if (file not in ("README.md" ,"_static/source/Software/NonResident/software.fragment") and
                "underoffer.rst" not in file and
                "@" not in file and "carousel" not in file and "snippets" not in file):
                with open(file) as f:
                    type = os.path.dirname(file).replace(PREFIX,'')
                    doc_type=convert_type_to_real_type(type)
                    for line in f:  

                        if UNDER_OFFER in line :
                            ANYUNDEROFFER=True
                            if 'An item in underoffer' not in line:
                                splitline = line.split('","')
                                part_number = splitline[0].strip().replace(UNDER_OFFER,'').replace('""','"')
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
                                    underoffer.append(thisdict)
                newlist = sorted(underoffer, key=lambda d: (d['DTYPE'],d['PN']))  
        HEADING=''
        for i in newlist:
            if HEADING != i['DTYPE']:
                HEADING = i['DTYPE']
                c.write('\n\n.. rubric:: ' + HEADING + '\n\n') 
                c.write('.. csv-table:: \n')
                c.write('\t:header: "Part Number","Description"\n')
                c.write('\t:widths: 30, 70\n\n')  
            c.write(i['OLINE'].replace(',"'+i['DTYPE']+'"\n','\n'))
    return ANYUNDEROFFER

def do_in_transit():
    ANYINTRANSIT=False
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
            if "@" in file:
                with open(file) as f:
                    
                    type = os.path.dirname(file).replace(PREFIX,'')
                    doc_type=type.replace('Documents' + OSSEP,'')
                    if  'Hardware' + OSSEP + 'ICs' in doc_type:
                        doc_type = 'ICs'
                    if  'Hardware' + OSSEP + 'Other' in doc_type:
                        doc_type = 'Other'
                    if  'Hardware' + OSSEP + 'EXORciser' in doc_type:
                        doc_type = 'EXORciser'
                    
                    prevproductname=''                  
                    
                    for line in f:  
                        if line.startswith("====="):
                            productname=prevproductname
                        else:
                            prevproductname=line.strip()
    
                        if line.startswith('.. _'):
                            tag=line.replace('.. _','').replace(':','').strip()
                        
                        if IN_TRANSIT in line:
                            print(doc_type + ' ' + file + ' contains in transit item')
                            link  = ':ref:`' + productname + ' <'+ tag + '>` '

                            outline = ('\t' + tag + ',"' + link + '"\n').replace('""','"')
                            thisdict = {"PN"    : tag, 
                                        "DESC"  : productname, 
                                        "DTYPE" : doc_type, 
                                        "OLINE" : outline }
                            if productname != '' :
                                intransit.append(thisdict)
                newlist = sorted(intransit, key=lambda d: (d['DTYPE'],d['PN']))  

        HEADING=''
        if len(newlist) > 0:
            ANYINTRANSIT=True
        for i in newlist:
            if HEADING != i['DTYPE']:
                HEADING = i['DTYPE']
                c.write('\n\n.. rubric:: ' + HEADING + '\n\n') 
                c.write('.. csv-table:: \n')
                c.write('\t:header: "Part Number","Description"\n')
                c.write('\t:widths: 30, 70\n\n')  
            c.write(i['OLINE'].replace(',"'+i['DTYPE']+'"\n','\n'))

    return ANYINTRANSIT

def collect_metadata():
    print('Collecting location metadata')

    md = []
    # Find all .rst files in the current directory and subdirectories
    files = glob.glob('**/*.'+SUFFIX, recursive=True)
    for file in files:
        if 'fragment' not in file:
            with open(file) as f:
                lines = f.readlines()
                for line in lines:
                    if line.find('.. #Metadata ') != -1:
                        if '.mini' not in file:
                            ref=file.split('@')[1].replace('.' + SUFFIX,'')
                        else:
                            ref=file.split('@')[1].replace('.mini.' + SUFFIX,'')
                        thisdict = {"REFERENCE"  : ref, 
                                    "METADATA"  : line }
                        if line != '':
                            md.append(thisdict)
    print('Location metadata collected')
    return md 





def do_collection():
    # Collect location medatadta
    md = collect_metadata()
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
                "@" not in file  and "carousel" not in file and "snippets" not in file):
                with open(file) as f:
                    #print('Checking for acquired ' + file)
                    type = os.path.dirname(file).replace(PREFIX,'')
                    doc_type=convert_type_to_real_type(type)
                    for line in f:

                        if CHECK_MARK in line  and 'This item is present in the collection' not in line:
                            splitline = line.split('","')
                            part_number = splitline[0].strip().replace(CHECK_MARK,'').replace('""','"')
                            ref = part_number.split('<')[1].split('>')[0].strip().replace('@','')
                            
                            location = get_location(ref,md)
                            try:
                                description = splitline[1].strip().replace('""','"')
                            except:
                                description = ''
                            part_number = part_number.replace('" :ref:','":ref:')
                            outline = ('\t' + part_number + '","' + description + '"\n').replace('""','"')
                            thisdict = {"PN"        : part_number, 
                                        "DESC"      : description, 
                                        "DTYPE"     : doc_type,
                                        "LOCATION"  : location,
                                        "OLINE"     : outline}
                            if description != '':
                                collection.append(thisdict)

                        if BIGGER_DOC in line and "Part of a larger single document" not in line:
                            location = 'TBD'
                            splitline= line.split('","')
                            try:
                                description = splitline[1].strip().replace('""','"')
                            except:
                                description = ''
                            location = splitline[4]
                            part_number = splitline[0]
                            outline = ('\t' + part_number.strip() + '","' + description + '"\n').replace('""','"')
                            thisdict = {"PN"        : part_number, 
                                        "DESC"      : description, 
                                        "DTYPE"     : doc_type,
                                        "LOCATION"  : location,
                                        "OLINE"     : outline}
                            if description != '':
                                collection.append(thisdict)


        newlist = sorted(collection, key=lambda d: (d['DTYPE'],d['PN']))  

        HEADING=''

        doneICs=False
        for i in newlist:
            
            ##
            if i['DTYPE'] == 'ICs' and not doneICs:
                doneICs=True
                update_chip_info(c)
            else:
                if i['DTYPE'] != 'ICs':
                    if HEADING != i['DTYPE']:
                        HEADING = i['DTYPE']
                        c.write('\n\n.. rubric:: ' + HEADING + '\n\n') 
                        c.write('.. csv-table:: \n')
                        c.write('\t:header: "Part Number","Description","Location"\n')
                        c.write('\t:widths: 18, 60, 22\n\n')  
                    
                    
                    location=",\n"
                    dr = ''
                    OUT=i['OLINE'][:-1]+ '\n'
                    if str(i['LOCATION']) != '' :
                        if 'Folder' in str(i['LOCATION']):
                            location = ',"Folder ' + i['LOCATION']['Folder'] + '"\n'
                            lcn = str(location[:-1]).replace('"','')[1:]
                            if lcn == 'Folder LOCAL':
                                lcn = 'Collection'
                            map_reference = i['LOCATION']['Folder'].replace(' ','_') + '_map_reference'

                            location = ',":ref:`' + lcn + ' <'+ map_reference + '>`"\n '

                            OUT=i['OLINE'][:-1]  + str(location) 

                        if 'Storage' in str(i['LOCATION']):
                            location = ',"' + i['LOCATION']['Storage'] 
                            if 'Drawer' in str(i['LOCATION']):
                                dr = ' Drawer ' + str(i['LOCATION']['Drawer'])
                                location = (str(location) + dr).replace(' ','_')
                                sb= i['LOCATION']['Storage'].replace('_',' ')
                                reference = ' ":ref:`' + sb +  '<'+ location[2:] + '>`"'
                            
                            OUT=i['OLINE'][:-1] + "," + reference + '\n'

                        if 'Storage' not in str(i['LOCATION']) and \
                        'Folder' not in str(i['LOCATION']): 
                            OUT=i['OLINE'][:-1]  + ',"' + BIGGER_DOC  + str(i['LOCATION'] )
                        
                    c.write(OUT.replace(',"'+i['DTYPE']+'"\n','\n'))
                
        c.write('\n\n')


def do_create():
    print("Enter the following information:")
    product_name = input("  Product name: ")
    product_number = input("  Product number: ")
    product_type = input("  Product Type:\n     (A)pplication Note\n     Reference (C)ard\n     (D)atasheet\n     (G)eneric\n     (I)Cs\n     (M)onitors\n     Ma(n)uals\n     (R)eference\n     (E)XORciser hardware\n     (O)ther hardware\n      : ")
    orphan = input("Orphan ? (Y/N): ")
    comments = input("Comments: ")
    acquired = input("Status ? (present/notpresent/intransit/underoffer): ")
    if acquired == "present":
        acquired = True
        index_entry = '"|present| :ref:`' + product_number + ' <' + product_number + '>`","' + product_name + '","' + comments + '"' 
        acquired_date = input("Acquired date (DD-MON-YYYY): ")
        acquired_status="|present| " + acquired_date + "\n\n"
    if acquired == "notpresent":
        acquired = False
        index_entry = '":ref:`' + product_number + ' <' + product_number + '>`","' + product_name + '","' + comments + '"' 
        acquired_status = "|notpresent|"
    if acquired == "intransit":
        acquired = False
        index_entry = '":ref:`' + product_number + ' <' + product_number + '>`","' + product_name + '","' + comments + '"' 
        acquired_status = "|intransit|"
    if acquired == "underoffer":
        acquired = False
        index_entry = '":ref:`' + product_number + ' <' + product_number + '>`","' + product_name + '","' + comments + '"' 
        acquired_status = "|underoffer|"

    links = input("Links ? (Y/N): ")

    if links == "Y":
        linkdocument = input("Document Name : ")

    dotdot = '../../'
    images = dotdot + 'images/'
    match product_type:
        case "A":
            location = "Documents/ApplicationNotes"
        case "R":
            location = "Documents/Reference"
            images = dotdot + 'images/Reference/'
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
        original_image = MOVE + '/' + product_number + '.png'
        image_present = True
        if not os.path.exists(original_image):
            c.write('.. image:: '+ dotdot + 'images' + '/NOIMAGE.png\n')
            image_present = False
        else:
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
        
        
        target_image = images.replace(dotdot,'source/') + product_number + '.png'
        print('Ready to move.....')
        
        if links =="Y":
            target_document =  "source/_static/" + location + "/"+ linkdocument
            movefile(original_document, target_document)
            
        if image_present:
            movefile(original_image, target_image)
            print('Moved images and source data')
        else:
            print('No image to move')            

    return index_entry


# DO SETUP
def setup_icons():
    with open("./xbuild_support/setup.toml", "rb") as f:
        data = tomllib.load(f)

    # Prepare new conf.py file
    copy_and_replace('./xbuild_support/conf.master','./xbuild_support/setup.pre')
    with open('./xbuild_support/setup.pre', 'a') as newfile:
        newfile.write("\n")
        newfile.write('rst_prolog = """\n')
        for icon in data["icons"]:
            newfile.write(".. |"+icon["name"].strip()+"| " + '\treplace:: ' + icon["icon"]+'\n')
            #print(icon["name"].strip() + ' icon added')
        newfile.write('"""\n')

    copy_and_replace('./xbuild_support/setup.pre','./source/conf.py')

    with open('./xbuild_support/conventions.rst', 'w') as newfile:
        newfile.write(':orphan:\n\n')
        newfile.write('.. csv-table::')
        newfile.write('   :header: "Symbol","Description"\n')
        newfile.write('   :widths: 14, 86\n')
        newfile.write('   :width: 100\n\n')

   
        for icon in data["icons"]:
            if (icon["tag"] == "conventions"):
                newfile.write("   |"+icon["name"].strip()+"|, " + '"' + icon["desc"]+'"\n')


    with open('source/Software/NonResident/media.inc', 'w') as newfile:
        newfile.write('.. rubric:: Key to Symbols\n\n')
        newfile.write('.. csv-table::\n\n')
        
        for icon in data["icons"]:
            if (icon["tag"] == "media"):
                newfile.write("   |"+icon["name"].strip()+"|, " + '"' + icon["desc"]+'"\n')

def do_timeline():
    print('Updating timeline')
    files = glob.glob('**/*.'+SUFFIX, recursive=True)
    timeline=[]
    invalid_months=False
    for file in files:
        if 'fragment' not in file and 'basic_options' not in file and 'index' not in file and 'conventions' not in file:
            with open(file, 'r') as f:
                prevproductname=''                
                lines = f.readlines()
                for line in lines:
                    if line.startswith("====="):
                        productname=prevproductname
                    else:
                        prevproductname=line.strip()
                    if line.startswith('.. _'):
                        tag=line.replace('.. _','').replace(':','').strip()
                    if line.find('|present') != -1:
                        acquired_date = line.replace("|present|",'').replace('"','').strip()
                        m=acquired_date[3:6].upper()
                        match m:
                            case "JAN":
                                month='01'
                            case "FEB":
                                month='02'
                            case "MAR":
                                month='03'
                            case "APR":
                                month='04'
                            case "MAY":
                                month='05'
                            case "JUN":
                                month='06'
                            case "JUL":
                                month='07'
                            case "AUG":
                                month='08'
                            case "SEP":
                                month='09'
                            case "OCT":
                                month='10'
                            case "NOV":
                                month='11'
                            case "DEC":
                                month='12'
                            case _:
                                print('Invalid month in ' + file)
                                invalid_months=True
                        converted_date=acquired_date[7:11]+month+acquired_date[0:2]
                        dline='{"Date":"' + converted_date + '"' + \
                                ',"File":"' + file + \
                                '","RDate":"' + acquired_date + \
                                '","Tag":"' + tag + \
                                '","Name":"' + productname + \
                                    '"}'
                        timeline.append(dline)
    if invalid_months:
        print('Invalid months detected - timeline not updated')
        return
    else:

        with open('./xbuild_support/timeline.rst', 'w') as f:
            f.write('.. _timeline:\n\n')
            f.write ('Timeline\n')
            f.write('========\n\n')
            f.write('This is the timeline of acquisitions (as at ' + time.strftime("%d-%m-%Y") + ').\n\n')

            f.write('.. csv-table::\n')
            f.write('   :header: "Date","Product" \n\n')
            
            for t in sorted(timeline):
                thisone = ast.literal_eval(t)
                f.write('   ' + thisone['RDate'] + ',:ref:`'+ thisone['Name']+ ' <' + thisone['Tag']+'>`\n')


            
        copy_and_replace('./xbuild_support/timeline.rst','./source/timeline.rst')

            
        print('Timeline updated')

def do_index_contents(ANYUNDEROFFER,ANYINTRANSIT):
    print('Updating index pages')
    if ANYUNDEROFFER == False:
        print('   No items under offer\n')
    if ANYINTRANSIT == False:
        print('   No items in transit\n')
    print('\n')
    with open("./xbuild_support/index.pre", "r") as f:
        lines=f.readlines()
    with open("./xbuild_support/index.master", "w") as f:
        for line in lines:
            if "##UNDEROFFER##" in line or "##INTRANSIT##" in line:
                if "##UNDEROFFER##" in line:
                    if ANYUNDEROFFER:
                        f.write('   Under Offer <underoffer>\n')
                if "##INTRANSIT##" in line:
                    if ANYINTRANSIT:
                        f.write('   In Transit <transit>\n')
            else:             
                f.write(line)
    copy_and_replace('./xbuild_support/index.master','./source/index.rst')



def rebuild_db():
    files = glob.glob('**/*.'+SUFFIX, recursive=True)
    conn = sqlite3.connect('xbuild_support/xbuild.db')
    cursor_obj = conn.cursor()
    cursor_obj.execute("CREATE TABLE IF NOT EXISTS ics     \
                       (    id INTEGER PRIMARY KEY,        \
                            icid                TEXT,      \
                            ic                  TEXT,      \
                            name                TEXT,      \
                            parent              TEXT,      \
                            parent_number       TEXT,      \
                            tag                 TEXT,      \
                            temperature         TEXT,      \
                            packaging           TEXT,      \
                            frequency           TEXT,      \
                            notes               TEXT,      \
                            mask                TEXT,      \
                            status              TEXT,      \
                            date_code           TEXT,      \
                            manufacture_date    TEXT,      \
                            acquired_date       TEXT,      \
                            real_date           TEXT,      \
                            location            TEXT,      \
                            metadata            TEXT,      \
                            filename            TEXT       \
                       );")
    conn.commit()

    cursor_obj.execute("CREATE TABLE IF NOT EXISTS iclinks     \
                       (    id INTEGER PRIMARY KEY,        \
                            icid                TEXT,      \
                            link                TEXT       \
                       );")
    conn.commit()

    cursor_obj.execute("CREATE TABLE IF NOT EXISTS icimages     \
                       (    id INTEGER PRIMARY KEY,        \
                            icid                TEXT,      \
                            image               TEXT       \
                       );")
    conn.commit()

    # Create Document tables
    cursor_obj.execute("CREATE TABLE IF NOT EXISTS documents \
                       (    id INTEGER PRIMARY KEY,          \
                            documenttype        TEXT,        \
                            documentid          TEXT,        \
                            document            TEXT,        \
                            name                TEXT,        \
                            tag                 TEXT,        \
                            notes               TEXT,        \
                            acquired_date       TEXT,        \
                            real_date           TEXT,        \
                            location            TEXT,        \
                            metadata            TEXT,        \
                            filename            TEXT,        \
                            status              TEXT         \
                       );")
    conn.commit()

    cursor_obj.execute("CREATE TABLE IF NOT EXISTS documentlinks \
                       (    id INTEGER PRIMARY KEY,              \
                            documenttype        TEXT,            \
                            documentid          TEXT,            \
                            link                TEXT             \
                       );")
    conn.commit()

    cursor_obj.execute("CREATE TABLE IF NOT EXISTS documentimages \
                       (    id INTEGER PRIMARY KEY,               \
                            documenttype        TEXT,             \
                            documentid          TEXT,             \
                            image               TEXT              \
                       );")
    conn.commit()



    # Clean tables if needed
    cursor_obj.execute("DELETE FROM icimages;")
    cursor_obj.execute("DELETE FROM iclinks;")
    cursor_obj.execute("DELETE FROM ics;")
    conn.commit()

    cursor_obj.execute("DELETE FROM documents;")
    cursor_obj.execute("DELETE FROM documentimages;")
    cursor_obj.execute("DELETE FROM documentlinks;")
    conn.commit()

    for file in files:

        if ('EngineeringNotes' in file or 'Datasheets' in file or 'ApplicationNotes' in file)and 'fragment' not in file and 'index' not in file:
            metadata=''
            tag=''
            acquired_date=''
            converted_date=''
            location=''
            filename = file.split(OSSEP)
            documentid = filename[3].replace('@' ,'').replace('.' + SUFFIX,'')  
            documenttype= ''
            notes=''
            with open(file, 'r') as f:
                prevproductname=''                
                lines = f.readlines()
                if 'Datasheets' in file:
                    documenttype='Datasheets'
                if 'ApplicationNotes' in file:
                    documenttype='ApplicationNotes'
                if 'EngineeringNotes' in file:
                    documenttype='EngineeringNotes'
                for line in lines:
                    if line.startswith('.. image::'):
                        image=line.replace('.. image::','').strip()
                        conn.execute("INSERT INTO documentimages (documenttype,documentid, image) VALUES (?,?,?);", (documenttype,documentid, image.strip()))
                        conn.commit()
                    if  ':ref:`' in line and 'Location' not in line:
                        conn.execute("INSERT INTO documentlinks (documenttype,documentid, link) VALUES (?,?,?);", (documenttype,documentid, line.strip()))
                        conn.commit()
                    if ':download:`' in line :
                        conn.execute("INSERT INTO documentlinks (documenttype,documentid, link) VALUES (?,?,?);", (documenttype,documentid, line.strip()))
                        conn.commit()
                    if '.. #Metadata ' in line:
                        metadata=line.replace('.. #Metadata','').strip()

                        metadataline = line.replace('.. #Metadata ', '').strip()
                        metadata_dict = eval(metadataline)
                        
                        location = metadata_dict["Folder"]
                        location = location.replace("None",'')

                    if line.startswith('.. _'):
                        tag=line.replace('.. _','').split(':')[0]
                    if line.startswith("====="):
                        productname=prevproductname
                    else:
                        prevproductname=line.strip()
                    if  CHECK_MARK in line or \
                        UNDER_OFFER in line or \
                        IN_TRANSIT in line or \
                        CROSS_MARK in line:
                            status = line.split('|')[1].strip()
                    if CHECK_MARK in line:
                            acquired_date = line.split('|')[2].strip().replace('"','')
                            m=acquired_date[3:6].upper()
                            match m:
                                case "JAN":
                                    month='01'
                                case "FEB":
                                    month='02'
                                case "MAR":
                                    month='03'
                                case "APR":
                                    month='04'
                                case "MAY":
                                    month='05'
                                case "JUN":
                                    month='06'
                                case "JUL":
                                    month='07'
                                case "AUG":
                                    month='08'
                                case "SEP":
                                    month='09'
                                case "OCT":
                                    month='10'
                                case "NOV":
                                    month='11'
                                case "DEC":
                                    month='12'
                                case _:
                                    print('Invalid month in ' + file)
                            converted_date=acquired_date[7:11]+month+acquired_date[0:2]
            conn.execute("INSERT INTO documents (documenttype,documentid, document, name, acquired_date, \
                            real_date, tag,notes, location, metadata,filename, status) \
                    VALUES (?,?,?,?,?,?,?,?,?,?,?,?);",
                    (documenttype,documentid, documentid, productname, acquired_date,converted_date,tag,\
                    notes,location,metadata,file,status))
            conn.commit()


        if 'ICs'+OSSEP+'MC' in file and 'fragment' not in file and 'basic_options' not in file and 'index' not in file and 'conventions' not in file and 'packaging' not in file:
            filename = file.split(OSSEP)
            parent = filename[4]
            parent_number = ''
            icid = filename[5].replace('@' ,'').replace('.' + SUFFIX,'')  
            if icid.find('!') == -1:
                ic = icid
            else:
                ic = icid.split('!')[1].strip()

            date_code = ''
            manufacture_date = ''
            acquired_date = ''
            converted_date = ''
            packaging = ''
            temperature = ''
            frequency = ''
            notes = ''
            mask = ''
            metadata = ''
            image = ''
            location = ''
            with open(file, 'r') as f:
                prevproductname=''                
                lines = f.readlines()
                for line in lines:
                    if line.startswith('.. image::'):
                        image=line.replace('.. image::','').strip()
                        conn.execute("INSERT INTO icimages (icid, image) VALUES (?,?);", (icid, image.strip()))
                        conn.commit()
                    if '.. #Metadata ' in line:
                        metadata=line.replace('.. #Metadata','').strip()

                    if  ':ref:`' in line and 'Location' not in line:
                        conn.execute("INSERT INTO iclinks (icid, link) VALUES (?,?);", (icid, line.strip()))
                        conn.commit()
                    if ':download:`' in line :
                        conn.execute("INSERT INTO iclinks (icid, link) VALUES (?,?);", (icid, line.strip()))
                        conn.commit()
                    if '"Date Code"' in line:
                        splitline=line.split(',')
                        date_code=splitline[1].replace('"','')[:-1].strip()
                    if '"Manufacture Date"' in line:
                        splitline=line.split(',')
                        manufacture_date=splitline[1].replace('"','')[:-1].strip()
                    if '"Mask"' in line:
                        splitline=line.split(',')
                        mask=splitline[1].replace('"','')[:-1].strip()
                    if line.startswith('.. _'):
                        tag=line.replace('.. _','').split(':')[0]
                    if '"Packaging"' in line:
                        splitline=line.split(',')
                        packaging=splitline[1].replace('"','')[:-1]
                    if '"Frequency"' in line:
                        splitline=line.split(',')
                        frequency=splitline[1].replace('"','')[:-1]
                    if '"Temperature"' in line:
                        splitline=line.split(',')
                        itemperature=splitline[1].replace('"','')[:-1].replace('\\ :sup:`o`\\ ','°')
                        if itemperature.startswith('-'):
                            ttemp = itemperature.split('-')
                            temperature = '-' + ttemp[1] + '°C to ' + ttemp[2]
                        else:
                            temperature = itemperature.replace('-','°C to ')
                    if '"Notes"' in line:
                        splitline=line.split('","')
                        notes=splitline[1].replace('"','')[:-1]
                    
                    if '"Location"' in line:
                        splitline=line.split('","')
                        location=splitline[1].replace('"','')[:-1]
                    
                    if line.startswith("====="):
                        productname=prevproductname
                    else:
                        prevproductname=line.strip()
                    if  CHECK_MARK in line or \
                        UNDER_OFFER in line or \
                        IN_TRANSIT in line or \
                        CROSS_MARK in line:
                            status = line.split('|')[1].strip()
                    if CHECK_MARK in line:
                            acquired_date = line.split('|')[2].strip().replace('"','')
                            m=acquired_date[3:6].upper()
                            match m:
                                case "JAN":
                                    month='01'
                                case "FEB":
                                    month='02'
                                case "MAR":
                                    month='03'
                                case "APR":
                                    month='04'
                                case "MAY":
                                    month='05'
                                case "JUN":
                                    month='06'
                                case "JUL":
                                    month='07'
                                case "AUG":
                                    month='08'
                                case "SEP":
                                    month='09'
                                case "OCT":
                                    month='10'
                                case "NOV":
                                    month='11'
                                case "DEC":
                                    month='12'
                                case _:
                                    print('Invalid month in ' + file)
                            converted_date=acquired_date[7:11]+month+acquired_date[0:2]
                            parent_number = parent.replace('MCM','').replace('MC','').strip()
            conn.execute("INSERT INTO ics (icid, ic,  parent, parent_number, name, status, acquired_date, real_date, \
                                           tag, packaging, temperature,frequency,mask,notes,date_code,manufacture_date, \
                                           location, metadata,filename) \
                         VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);",
                         (icid, ic, parent,parent_number,productname,status,acquired_date,converted_date,tag,packaging,\
                          temperature,frequency,mask,notes,date_code,manufacture_date,location,metadata,file))
        
            conn.commit()
    conn.close()

def read_db(statement):
    conn = sqlite3.connect('xbuild_support/xbuild.db')
    conn.row_factory = sqlite3.Row
    cursor_obj = conn.cursor()
    cursor_obj.execute(statement)
    output = cursor_obj.fetchall()
    conn.close()
    return output


def update_chip_info(f):
    
    output = read_db("SELECT * FROM ics WHERE status = 'present' order by  parent_number,ic;")

    f.write('\n\n.. rubric:: ICs\n\n')
    f.write('.. csv-table::\n')
    f.write('\t:header: "Part Number","Packaging","Location" \n')
    f.write('\t:widths: 25, 20, 55\n\n')


    for row in output:

        f.write('\t":ref:`' + row["ic"] + ' <' +  row["tag"] + '>`",')
        f.write('"' + row["packaging"] + '",')
        f.write('"' + row["location"] + '"\n')

    f.write('\n')

    
while True:
    print('\t1. Get date range from week ')
    print('\t2. Create new entry  ')
    print('\t3. Create new IC group index')    
    print('\t4. Create new IC group from index')    
    print('\t5. Update IC status (WIP)')
    print('\t6. Update carousels')
    print('\t7. Rebuild DB')
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
            print("\nSetting Up icons")
            setup_icons()
            print('Commencing rebuild of database') 
            rebuild_db()
            print('Completed rebuild of database')     
            print('\n\nCommencing updating storage metadata links')

            rstfiles = glob.glob('**/*.rst', recursive=True)
            for file in rstfiles:
                update_or_not_metadata(file)
            print('Completed updating storage metadata links')
        

            update_carousel()
            update_IC_pre_fragments()
            update_IC_index()
            update_storage()
            do_collection()
            
            print('Collection updated')
            ANYUNDEROFFER=do_underoffer()
            ANYINTRANSIT=do_in_transit()
            do_index_contents(ANYUNDEROFFER,ANYINTRANSIT)
            print('In-Transit updated')
            do_timeline()
            os.system("make clean html")
        case "5":
            ic = input("Enter IC to change status: ")
            statement = "SELECT * FROM ics WHERE icid = '" + ic + "';"
            output = read_db(statement)
            c=0
            for row in output:
               c=c+1 
            if c == 0:
                print('IC ' + ic + ' not found')
                continue
            if c > 1:
                print('IC ' + ic + ' found multiple times - manual update required')
                continue
        
            if c == 1:
                for row in output:    
                    filename = row["filename"]
                    print('Current status of ' + row["icid"] + ' is ' + row["status"])
                    newstatus = input("Enter new status (present, notpresent, underoffer, intransit): ")
                    

                    if newstatus == 'intransit':
                        with open(filename, 'r') as f:
                            lines = f.readlines()
                        
                        with open(filename+'.new', 'w') as f:                        
                            for line in lines:
                                if '|present|' in line or '|notpresent|' in line or '|underoffer|' in line or '|intransit|' in line:
                                    newline = '   |' + newstatus + '| '  + '\n'
                                    f.write(newline)
                                else:
                                    f.write(line)


                    if newstatus == 'notpresent':
                        with open(filename, 'r') as f:
                            lines = f.readlines()
                        
                        with open(filename+'.new', 'w') as f:                        
                            for line in lines:
                                if '|present|' in line or '|notpresent|' in line or '|underoffer|' in line or '|intransit|' in line:
                                    newline = '   |' + newstatus + '| '  + '\n'
                                    f.write(newline)
                                else:
                                    f.write(line)

                    if newstatus== "present":
                        acquired_date = input("Enter acquired date (DD-MON-YYYY): ")
                        newline = '   |' + newstatus + '| ' + acquired_date + '\n'                    
                        y = input('Enter year: ')
                        w = input('Enter week: ')
                        firstdate, lastdate =  getDateRangeFromWeek(y,w)
                        manudate=y[-2:]+w
                        realmanudate=firstdate + ' to ' + lastdate
                        mask=input('Enter mask (if known): ')
                        storagebox=input('Enter Storage Box: ')
                        drawer=input('Enter Drawer: ')
                        rownum=input('Enter Row: ')
                        column=input('Enter Column: ')
                        
                        with open(filename, 'r') as f:
                            lines = f.readlines()
                        for line in lines:
                            maskfound=False
                            metadatafound=False
                            if "Mask" in line:
                                maskfound=True
                            if ".. #Metadata " in line:
                                metadatafound=True
                            
                        metadataskeleton=".. #Metadata {'Product':'XXXXX','Storage': 'Storage Box !','Drawer':@,'Row':$,'Column':%}"
                        metadataline = metadataskeleton.replace('XXXXX',row["icid"]).replace('!',storagebox).replace('@',drawer).replace('$',rownum).replace('%',column )                        

                        with open(filename+'.new', 'w') as f:                        
                            for line in lines:
                                writeln=False
                                if '|present|' in line or '|notpresent|' in line or '|underoffer|' in line or '|intransit|' in line:
                                    newline = '   |' + newstatus + '| ' + acquired_date + '\n'
                                    f.write(newline)
                                    writeln=True

                                if (line.startswith('.. #Metadata '.upper())) or (line.startswith('..#None '.upper())) :
                                    f.write(metadataline)
                                    writeln=True     

                                if line.startswith('.. #None '):
                                    pass

                                if line.startswith('.. _') and not metadatafound:
                                    f.write(line+'\n')
                                    f.write(metadataline + '\n')
                                    print('Added Metadata to IC information')
                                    writeln=True

                                if "Temperature" in line and not maskfound:
                                    f.write(line)
                                    f.write('   "Mask","' + mask + '"\n')
                                    print('Added Mask to IC information')
                                    writeln=True

                                if line.startswith('.. image::'):
                                    ifile='../../../../images/Hardware/ICs/' + row["parent"] + '/' + row["icid"] +'.png'
                                    newimage = '.. image:: ' + ifile + '\n'
                                    f.write(newimage)
                                    if not os.path.exists(newimage):
                                        print('Warning: Image ' + ifile + ' not found')
                                    writeln=True
                                
                                if "Date Code" in line:
                                    newline = '   "Date Code","' + manudate + '"\n'
                                    f.write(newline)
                                    writeln=True

                                if "Manufacture Date" in line:
                                    newline = '   "Manufacture Date","' + realmanudate + '"\n'
                                    f.write(newline)
                                    writeln=True

                                if not writeln:
                                    f.write(line)

                        print('Updated status : Now assess storage metadata')    

        case "6":
            update_carousel()
        case "7":
            print('Commencing rebuild of database') 
            rebuild_db()
            print('Completed rebuild of database') 
        
        case "8":
            print('Commencing finding artefacts with invalid metadata') 
            rstfiles = glob.glob('**/*.rst', recursive=True)
            metadatacount = 0
            none_metadatacount=0
            TBD_metadatacount=0
            illegal_metadata=[]
            for filename in rstfiles:
                if "@" in filename:
                    with open(filename, 'r') as f:
                        lines = f.readlines()                        
                    found_metadata = False
                    for line in lines:
                        if "Metadata" in line:
                            found_metadata=True
                        if "#Metadata" in line and "'Drawer':X" in line:
                            illegal_metadata.append(filename)
                        if "#None" in line:
                            #print("#None metadata : " + filename)
                            found_metadata=True
                            illegal_metadata.append(filename)
                            none_metadatacount+=1
                        if "#TBD" in line:
                            print("#TBD metadata : " + filename)
                            found_metadata=True
                            TBD_metadatacount+=1
                    if not found_metadata:
                        metadatacount+=1
                        if metadatacount == 1:
                            print(" Artefacts with no metadata:")
                        
                        print("No metadata : " + filename)
            
            if metadatacount == 0:
                print("All artefact files have metadata tags.")
            else:
                if none_metadatacount>0:
                            print(str(none_metadatacount) + " Artefacts with #None metadata:")
                if TBD_metadatacount > 0:
                            print(str(TBD_metadatacount) + " Artefacts with #TBD metadata:")                            
                print(str(metadatacount) + " with no metadata.")

            #for fullfilename in illegal_metadata:
            #    print("#None metadata : " + fullfilename)
            #output = read_db("SELECT * FROM documents WHERE (metadata IS NULL or metadata = '') and documenttype='Datasheets';")

            #for row in output:
            #    print('Datasheet ' + row["name"] + ' has no metadata')
                
            #    metadataskeleton=".. #Metadata {'Product':'XXXXX','Folder': '@@'}"
            #    metadataskeleton=".. #Metadata {'Product':'YYYY','Name':'XXXXX','Storage': 'S','Drawer':0,'Row':0,'Column':0}"

            #    metadataline = metadataskeleton.replace('XXXXX',row["name"]).replace('YYYY',row["location"])    
            #    if row["location"].strip() == '':
            #        metadataline = metadataline.replace('@@','None')
            #    else:
            #        metadataline = metadataline.replace('@@',row["location"])    
            #    print(metadataline)
            #    fullfilename = row["filename"]
            #    print('In file: ' + fullfilename)
            #   with open(fullfilename, 'r') as f:
            #            lines = f.readlines()     
            #    with open(fullfilename, 'w') as f:
            #        for line in lines:
            #            
            #            if line.startswith('.. _'):
            #                f.write(line)
            #                f.write('\n' + metadataline + '\n')
            #            else:
            #                f.write(line)
                        
                #       if '#Metadata' in line:
                #            newline=line.replace(':X',':0').replace(':Y',':0').replace(':Z',':0')
                #            f.write(newline)
                #        else:
                #           f.write(line)

                
                
            print('Done') 
            
        case "X":
            print('Exiting')
            exit()
        case "x":
            print('Exiting')
            exit()

        case _:
            print('Invalid choice')
            
        


