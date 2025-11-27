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

# Set up rich environment
import rich
from rich.console import Console
from rich.prompt import Prompt
from rich.theme import Theme
from rich import print
from rich.panel import Panel
custom_theme = Theme({
    "info": "bold green",
    "warning": "bold Blue",
    "danger": "bold red"
})
console = Console(theme=custom_theme)


CHECK_MARK    = '|present|'
CROSS_MARK    = '|notpresent|'
IN_TRANSIT    = '|intransit|'
UNDER_OFFER   = '|underoffer|'
BIGGER_DOC    = '|document|'

OSSEP         = os.sep

DB = 'xbuild_support' + OSSEP + 'xbuild.db'

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
        console.print('Index file for ' + newgroupname + ' does not exist', style="danger")
        exit()
    console.print('Creating new group from index file for IC: ' + newgroupname, style="info")

    _ = make_directory(NEW_LOC)

    
    with open(LOC.lower() ,"r") as d:
        lines = d.readlines()

    for line in lines:
        if line.startswith('.. collapse::'):
            group_name = line.split('.. collapse::')[1].strip()
            console.print('Group name: ' + group_name, style="info")
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

    console.print('New group index created in ' + LOC.lower(),style="info")

def update_IC_pre_fragments():
    console.print('\tCommencing Updating IC pre-fragments',style="info")

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

    console.print('\n\tFinished updating IC pre-fragments',style="info")

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
    console.print('\n\n\tCarousels updated',style="info")





def update_storage():
    console.print('\tCleaning and prepping storage files\n',style="info")
    snippetfiles = glob.glob('**/*.snippet', recursive=True)
    for snippetfile in snippetfiles:
        os.remove(snippetfile)
        
    storage=[]
    foldersrefcard=[]
    foldershardware=[]
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
            
            if 'Hardware/Other' in file and 'fragment' not in file and 'index' not in file:
                if "@" in file:
                    metadata,loc = get_loc(file) 
                    
                    if loc != "{'Status': ''}":
                        if metadata and loc["Status"] == "YES":
                            foldershardware.append(loc)

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

    all_folders_storage_sorted =  sorted(foldershardware + sorted_folders_manuals + sorted_folders_datasheets+sorted_folders_appnotes + sorted_folders_softres + sorted_folders_softnon + sorted_folders_generic + sorted_folders + sorted_folders_reference, key=lambda x: (x['Folder'],x['Product']))
    FOLDER_MAP_FILE = 'source'+ OSSEP + 'Documents' + OSSEP + 'folder.map'
    current_folder=''
    with open(FOLDER_MAP_FILE, "w") as fmf:

        for item in all_folders_storage_sorted:
            if item['Folder'] != current_folder:
                write_folder=False
                map_reference = '\n\n.. _' + item['Folder'].replace(' ','_') + '_map_reference:'
                current_folder = item['Folder']

                console.print('\t\tProcessing folder: ' + current_folder,style="info")
                match item['Folder']:
                    case "Hardware":
                        write_folder=True
                        fmf.write(map_reference + '\n\n.. rubric:: Hardware \n\n')
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
                    case ' ':
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
    console.print('\n\t\tFolder map created in ' + FOLDER_MAP_FILE,style="info")


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
                    console.print('No columns found for Storage:', item['Storage'], 'Drawer:', item['Drawer'], 'Row:', item['Row'],style="danger")
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

    console.print('\tSplitting',style="info")
    console.print('\t\tLVL1 Splitting',style="info")

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

    console.print('\t\tLVL1 Splitting done\n',style="info")
    console.print('\t\tLVL2 Splitting',style="info")

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
                console.print('\t\t\tProcessing location: ' + LOCATIONINSTORAGE,style="info")
                outputfile = outputfile_base + '.' + LOCATIONINSTORAGE + '.snippet'
                with open(outputfile,"w") as opf:
                    if not stripped.endswith('`>\n'):
                        stripped = stripped + '>`\n'
                    opf.write(stripped)

    # Remove specific known problematic file 
    console.print("\n\t\t\tRemove specific known problematic file",style="warning")              
    os.remove('source/Documents/Hardware/ICs/tables.fragment.S.Drawer_0.snippet')                    
    
    # Remove temporary "tiny" working files
    console.print("\n\t\tSplitting LVL2 files complete",style="info")

    console.print("\n\t\tRemoving temporary files",style="info")
    for lvl2file in sorted(lvl2files):
        os.remove(lvl2file)
        console.print('\t\t\t' + os.path.basename(lvl2file) + ' removed.',style="info")       
    
    snippetfiles = glob.glob('**/*.snippet', recursive=True)
    
    # Changing collapsing into rubrics
    console.print('\n\t\tChanging collapsing into rubrics (tables.fragment.<snippet file>):',style="info")
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

        
        tfname = os.path.basename(snippetfile).split('.')
        if tfname[2] == tfname[3]:
            display_tfname = tfname[2] + '.snippet'
        else:
            display_tfname=os.path.basename(snippetfile).replace('tables.fragment.','')
        console.print('\t\t\t' + display_tfname,style="info")
    
    # Move snippets into snippets folder
    console.print('\n\t\tMoving snippets into snippets folder',style="info")

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
                console.print('     Duplicate tag detected: ' + ISOPREAMBLE,style="warning")
                SFHEADER=''
            else:
                SFHEADER='.. rubric:: '+ SF.split('.')[0].replace('_',' ')
            if SFHEADER != HDR:
                sicif.write(SFHEADER + '\n\n')
                HDR = SFHEADER
    


            sicif.write('.. include:: Documents'+OSSEP+'Hardware'+OSSEP+'ICs'+OSSEP+'snippets'+OSSEP+ os.path.basename(snippetfile) + '\n\n')

            console.print('     Moved ' + os.path.basename(snippetfile) + ' to ' + 'snippets',style="info")


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

    console.print('\nFolders updated',style="info")      

    console.print('\nLocations fully updated',style="info")      

def update_IC_index():

    console.print('\n\tUpdating IC index',style="info")

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

    console.print('\n\tCompleted updating IC index\n',style="info")


def do_underoffer():
    ANYUNDEROFFER=False
    underofferinfo=''

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
                                    t=doc_type + '\t\t\t' + file + '\n'
                                    underofferinfo = underofferinfo + t
                newlist = sorted(underoffer, key=lambda d: (d['DTYPE'],d['PN']))  
        if underofferinfo != '':            
            print(Panel(underofferinfo, title="Under Offer items"))

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
    intransitinfo=''

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
                            
                            link  = ':ref:`' + productname + ' <'+ tag + '>` '

                            outline = ('\t' + tag + ',"' + link + '"\n').replace('""','"')
                            thisdict = {"PN"    : tag, 
                                        "DESC"  : productname, 
                                        "DTYPE" : doc_type, 
                                        "OLINE" : outline,
                                        "FILE"  : file }
                            if productname != '' :
                                intransit.append(thisdict)
                                
                newlist = sorted(intransit, key=lambda d: (d['DTYPE'],d['PN']))  

        if intransit != []:            
            
            from rich.table import Table
            table = Table(title="In Transit items")

            table.add_column("Type", justify="left", style="cyan", no_wrap=True)
            table.add_column("File", style="bold green")

            for item in intransit:            
                table.add_row(item["DTYPE"], item["FILE"])
            
            console.print(table)

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
    console.print('Collecting location metadata',style="info")

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
    console.print('Location metadata collected',style="info")
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
                        if 'Folder' in str(i['LOCATION']) :
                            if 'None' in str(i['LOCATION']):
                                print(str(i['LOCATION']))

                    c.write(OUT.replace(',"'+i['DTYPE']+'"\n','\n'))
                
        c.write('\n\n')


def do_create():
    console.print("Enter the following information:",style="info")
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
            console.print("Invalid product type",style="danger")
            exit() 

    OUTPUT_FILE = f"source/{location}/@{product_number}.rst"
    if os.path.exists(OUTPUT_FILE):
        console.print(f"File {OUTPUT_FILE} already exists",style="danger")
        exit()

    console.print(f"Creating file {OUTPUT_FILE}",style="info")
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
        console.print('Ready to move.....',style="info")
        
        if links =="Y":
            target_document =  "source/_static/" + location + "/"+ linkdocument
            movefile(original_document, target_document)
            
        if image_present:
            movefile(original_image, target_image)
            console.print('Moved images and source data',style="info")
        else:
            console.print('No image to move',style="info")            

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
    console.print('Updating timeline',style="info")
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
                                console.print('Invalid month in ' + file,style="warning")
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
        console.print('Invalid months detected - timeline not updated',style="warning")
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

            
        console.print('Timeline updated',style="info")

def do_statistics():
        ot=0
        op=0
        opnp=0
        opit=0
        output = read_db("SELECT * FROM summary where documenttype != 'Software/Resident/EXORset30ROMS' order by  documenttype;")
        e30roms = read_db("SELECT * FROM collection where artfacttype = 'Software/Resident/EXORset30ROMS' order by  artfacttype;")
        e30present=0
        e30notpresent=0
        e30intransit=0
        for r in e30roms:
            if r["status"] == 'present' or r["status"] == '|document|':
                e30present=r["total"]
            if r["status"] == 'notpresent':
                    e30notpresent=r["total"]
            if r["status"] == 'intransit':
                    e30intransit=r["total"]

        with open('./xbuild_support/statistics.rst', 'w') as f:
            f.write('.. _statistics:\n\n')
            f.write('Statistics\n')
            f.write('==========\n\n')
            f.write('A set of statistics of known MC6800 artefacts (as at ' + time.strftime("%d-%m-%Y") + ').\n\n')

            f.write('.. csv-table::\n')
            f.write('   :header: "Document Type","Total","Present","Not Present","In Transit" \n\n')
            
            for row in output:
                
                present = 0
                notpresent = 0
                intransit =0
                items = read_db("SELECT * FROM collection where artfacttype = '" + row["documenttype"] +"';")
                
                for r in items:
                    if r["status"] == 'present' or r["status"] == '|document|':
                        present=r["total"]
                    if r["status"] == 'notpresent':
                            notpresent=r["total"]
                    if r["status"] == 'intransit':
                            intransit=r["total"]


                dtype=insert_spaces_into_document_type(row["documenttype"])
                if dtype == "Resident Software":
                    t=row["total"] + e30present + e30notpresent + e30intransit
                    f.write('   ' + dtype + ',' + str(t) + ',' + str(e30present+present) + ',' + str(e30notpresent + notpresent) + ',' + str(e30intransit + intransit) + '\n')
                    ot=ot+t
                    op=op+e30present+present
                    opnp=opnp+e30notpresent+notpresent
                    opit=opit+e30intransit+intransit
                else:
                    t = row["total"]
                    f.write('   ' + dtype + ',' + str(t) + ',' + str(present) + ',' + str(notpresent) + ',' + str(intransit) + '\n')
                    ot=ot+t
                    op=op+present
                    opnp=opnp+notpresent
                    opit=opit+intransit

            f.write('   ' + 'TOTAL' + ',' + str(ot) + ',' + str(op) + ',' + str(opnp) + ',' + str(opit) + '\n')
        
        copy_and_replace('./xbuild_support/statistics.rst','./source/statistics.rst')

        
        console.print('Statistics updated',style="info")

def do_index_contents(ANYUNDEROFFER,ANYINTRANSIT):
    console.print('Updating index pages',style="info")

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
                        if os.path.exists(TRANSIT_FILE):
                            console.print('Removing redundant TRANSIT_FILE',style="info")
                            os.remove(TRANSIT_FILE) 
            

            else:             
                f.write(line)
    copy_and_replace('./xbuild_support/index.master','./source/index.rst')



def rebuild_db():
    files = glob.glob('**/*.'+SUFFIX, recursive=True)
    conn = sqlite3.connect(DB)
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
                            filename            TEXT,      \
                            storage             TEXT,      \
                            drawer              TEXT,      \
                            row                 TEXT,      \
                            col                 TEXT,      \
                            manufacture_status  TEXT,      \
                            temperature_raw     TEXT       \
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
                            status              TEXT,        \
                            bigger              TEXT         \
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


    cursor_obj.execute("CREATE TABLE IF NOT EXISTS carousels     \
                       (    id INTEGER PRIMARY KEY,              \
                            documenttype        TEXT,            \
                            documentid          TEXT,            \
                            carouselid          TEXT,            \
                            carouselfile        TEXT             \
                       );")
    conn.commit()

    cursor_obj.execute("CREATE VIEW IF NOT EXISTS summary       \
                        (                                       \
                        documenttype,                           \
                        total                                   \
                        )                                       \
                        AS                                      \
                        SELECT documenttype,COUNT(documentid)   \
                        FROM documents                          \
                        GROUP BY documenttype                   \
                        UNION                                   \
                        SELECT 'ICs', COUNT(icid)               \
                        FROM ics                                \
                        ORDER by 1 ASC;")
    conn.commit()
    
    cursor_obj.execute("CREATE VIEW IF NOT EXISTS collection                  \
                        (                                                     \
                        artfacttype,                                          \
                        status,                                               \
                        total                                                 \
                        )                                                     \
                        AS                                                    \
                        SELECT documenttype as artefacttype,status,           \
                                COUNT(documentid) as total                    \
                        FROM documents                                        \
                        GROUP BY status,documenttype                          \
                        UNION                                                 \
                        SELECT 'ICs' as artefacttype, status,                 \
                                COUNT(icid) as total                          \
                        FROM ics                                              \
                        GROUP BY status,artefacttype;")
                        
    conn.commit()
    

    # Clean tables if needed
    cursor_obj.execute("DELETE FROM icimages;")
    cursor_obj.execute("DELETE FROM iclinks;")
    cursor_obj.execute("DELETE FROM ics;")
    conn.commit()

    cursor_obj.execute("DELETE FROM documents;")
    cursor_obj.execute("DELETE FROM documentimages;")
    cursor_obj.execute("DELETE FROM documentlinks;")
    cursor_obj.execute("DELETE FROM carousels;")



    conn.commit()

    for file in files:

        if ('Software' + OSSEP + 'Resident' + OSSEP + 'EXORset30ROMS' + OSSEP + '@' in file or
            'Software' + OSSEP + 'Resident' + OSSEP + '@' in file or
            'Software' + OSSEP + 'NonResident' + OSSEP + '@' in file or
            'Hardware' + OSSEP + 'Other' + OSSEP + '@' in file or
            'Hardware' + OSSEP + 'EXORciser' + OSSEP + '@' in file or 
            'Hardware' + OSSEP + 'EXORciser' + OSSEP + 'Micromodules' + OSSEP + '@' in file or
            'Generic' in file or
            'Manuals' in file or 
            'Reference' in file or 
            'EngineeringNotes' in file or 'Datasheets' in file or 
            'ApplicationNotes' in file) \
            and 'fragment' not in file and 'index' not in file \
            and 'carousel' not in file :
            metadata=''
            tag=''
            acquired_date=''
            converted_date=''
            location=''
            filename = file.split(OSSEP)
            carouselfile=''
            carouselid=1
            documentid = filename[3].replace('@' ,'').replace('.' + SUFFIX,'')  
            documenttype= ''
            notes=''
            status=''
            bigger=''
            with open(file, 'r') as f:

                prevproductname=''                
                lines = f.readlines()
                if 'Manuals' in file:
                    documenttype='Manuals'
                if 'Datasheets' in file:
                    documenttype='Datasheets'
                if 'ApplicationNotes' in file:
                    documenttype='ApplicationNotes'
                if 'EngineeringNotes' in file:
                    documenttype='EngineeringNotes'
                if 'Generic' in file:
                    documenttype='Generic'
                if 'Hardware'+OSSEP+'EXORciser'+OSSEP+'@' in file:
                    documenttype='Hardware/EXORciser'
                    documentid = filename[4].replace('@' ,'').replace('.' + SUFFIX,'')  
                if 'Hardware'+OSSEP+'Other'+OSSEP+'@' in file:
                    documenttype='Hardware/Other'
                    documentid = filename[4].replace('@' ,'').replace('.' + SUFFIX,'')  
                if 'Hardware'+OSSEP+'EXORciser'+OSSEP+'Micromodules' + OSSEP + '@' in file:
                    documenttype='Hardware/EXORciser/Micromodules'
                    documentid = filename[5].replace('@' ,'').replace('.' + SUFFIX,'')  
                if 'Reference' in file:
                        documenttype='Reference'
                if 'ReferenceCards' in file:
                        documenttype='ReferenceCards'
                if 'Software' + OSSEP + 'NonResident' + OSSEP + '@' in file:
                        documenttype='Software/NonResident'
                if 'Software' + OSSEP + 'Resident' + OSSEP + '@' in file:
                        documenttype='Software/Resident'
                if 'Software' + OSSEP + 'Resident' + OSSEP + 'EXORset30ROMS' + OSSEP + '@' in file:
                        documenttype='Software/Resident/EXORset30ROMS'
                        documentid = filename[4].replace('@' ,'').replace('.' + SUFFIX,'')  


                in_notes=False
                notes=''
                for line in lines:
                    if in_notes and line.startswith('.. '):
                        in_notes=False
                    if line.strip().upper().startswith('.. NOTE::'):
                        in_notes=True
                    if in_notes:    
                        notes+=line 


                    if '.. include::' in line and 'carousel in line':
                        carouselfile=line.replace('.. include::','').strip()
                        conn.execute("INSERT INTO carousels (documenttype,documentid, carouselid, carouselfile) VALUES (?,?,?,?);", (documenttype,documentid, carouselid, carouselfile.strip()))
                        conn.commit()
                        carouselid+=1
                    if line.startswith('.. image::'):
                        image=line.replace('.. image::','').strip()
                        conn.execute("INSERT INTO documentimages (documenttype,documentid, image) VALUES (?,?,?);", (documenttype,documentid, image.strip()))
                        conn.commit()
                    if  line.startswith(':ref:`') and 'Location' not in line and BIGGER_DOC not in line:
                        conn.execute("INSERT INTO documentlinks (documenttype,documentid, link) VALUES (?,?,?);", (documenttype,documentid, line.strip()))
                        conn.commit()
                    if line.startswith(':download:`'):
                        conn.execute("INSERT INTO documentlinks (documenttype,documentid, link) VALUES (?,?,?);", (documenttype,documentid, line.strip()))
                        conn.commit()
                    if line.startswith(':extlink-'):
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
                    if BIGGER_DOC in line:
                            status = BIGGER_DOC        
                            acquired_date=line.strip()[len(line.strip())-11:len(line.strip())].strip()
                            bigger = line.replace(BIGGER_DOC,'').replace(acquired_date,'').strip(   )
                    if CHECK_MARK in line or BIGGER_DOC in line:
                            if status != BIGGER_DOC:
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
                                    console.print('Invalid month in ' + file + "(" + m + ")",style="warning")
                            converted_date=acquired_date[7:11]+month+acquired_date[0:2]
            conn.execute("INSERT INTO documents (documenttype,documentid, document, name, acquired_date, \
                            real_date, tag,notes, location, metadata,filename, status,bigger) \
                    VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?);",
                    (documenttype,documentid, documentid, productname, acquired_date,converted_date,tag,\
                    notes,location,metadata,file,status,bigger))
            conn.commit()


        if 'ICs'+OSSEP+'MC' in file and 'fragment' not in file and 'basic_options' not in file and 'index' not in file and 'conventions' not in file and 'packaging' not in file:
            filename = file.split(OSSEP)
            carouselid=1
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
            temperature_raw = ''
            frequency = ''
            notes = ''
            mask = ''
            manufacture_status = ''
            metadata = ''
            image = ''
            location = ''
            storage = ''
            drawer = ''     
            row = ''    
            col = ''
            with open(file, 'r') as f:
                prevproductname=''                
                lines = f.readlines()
                in_notes=False
                notes = ''

                for line in lines:
                    if in_notes and line.startswith('.. '):
                        in_notes=False
                    if line.strip().upper().startswith('.. NOTE::'):
                        in_notes=True
                    if in_notes:    
                        notes+=line 

                    if line.startswith('.. image::'):
                        image=line.replace('.. image::','').strip()
                        conn.execute("INSERT INTO icimages (icid, image) VALUES (?,?);", (icid, image.strip()))
                        conn.commit()
                    if '.. #Metadata ' in line:
                        metadata=line.replace('.. #Metadata','').strip()
                        metadata_dict = eval(metadata)
                        if 'Drawer' in metadata_dict:
                            storage = metadata_dict['Storage']
                            if storage != 'S':
                                drawer = metadata_dict['Drawer']
                                row = metadata_dict['Row']
                                col = metadata_dict['Column']
                            else:
                                storage = ''
                        else:
                            storage = metadata_dict['Storage']                                                            
                    if '.. include::' in line and 'carousel in line':
                        carouselfile=line.replace('.. include::','').strip()
                        conn.execute("INSERT INTO carousels (documenttype,documentid, carouselid, carouselfile) VALUES (?,?,?,?);", (documenttype,documentid, carouselid, carouselfile.strip()))
                        conn.commit()
                        carouselid+=1
                    if  ':ref:`' in line and 'Location' not in line:
                        conn.execute("INSERT INTO iclinks (icid, link) VALUES (?,?);", (icid, line.strip()))
                        conn.commit()
                    if ':download:`' in line :
                        conn.execute("INSERT INTO iclinks (icid, link) VALUES (?,?);", (icid, line.strip()))
                        conn.commit()
                    
                    if '"Status"' in line:
                        splitline=line.split(',')
                        manufacture_status=splitline[1].replace('"','')[:-1].strip()
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
                        temperature_raw = splitline[1].replace('"','')[:-1]
                        itemperature=splitline[1].replace('"','')[:-1].replace('\\ :sup:`o`\\ ','°')
                        
                        if itemperature.startswith('-'):
                            ttemp = itemperature.split('-')
                            temperature = '-' + ttemp[1] + '°C to ' + ttemp[2]
                        else:
                            temperature = itemperature.replace('-','°C to ')

                    if '"Location"' in line:
                        splitline=line.split('","')
                        location=splitline[1].replace('"','')[:-1]
                        if ':ref:' not in location and location != 'TBD':
                            storage = location 

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
                                           location, metadata,filename,storage,drawer,row,col,manufacture_status, \
                                           temperature_raw) \
                         VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);",
                         (icid, ic, parent,parent_number,productname,status,acquired_date,converted_date,tag,packaging,\
                          temperature,frequency,mask,notes,date_code,manufacture_date,location,metadata,file,storage,\
                          drawer,row,col,manufacture_status,temperature_raw))
        
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

def get_links_from_db(documentid,documenttype):
    conn = sqlite3.connect(DB)
    conn.row_factory = sqlite3.Row
    cursor_obj = conn.cursor()
    if documenttype == 'ICs':
        cursor_obj.execute("SELECT * FROM iclinks WHERE icid = ?;", (documentid,))
    else:
        cursor_obj.execute("SELECT * FROM documentlinks WHERE documentid = ? AND documenttype = ?;", (documentid,documenttype))
    output = cursor_obj.fetchall()
    conn.close()
    return output   

def get_images_from_db(documentid,documenttype):
    conn = sqlite3.connect(DB)
    conn.row_factory = sqlite3.Row
    cursor_obj = conn.cursor()
    if documenttype == 'ICs':
        cursor_obj.execute("SELECT * FROM icimages WHERE icid = ?;", (documentid,))
    else:
        cursor_obj.execute("SELECT * FROM documentimages WHERE documentid = ? AND documenttype = ?;", (documentid,documenttype))
    output = cursor_obj.fetchall()
    conn.close()
    return output 

def get_notes_from_db(documentid,documenttype):
    conn = sqlite3.connect(DB)
    conn.row_factory = sqlite3.Row
    cursor_obj = conn.cursor()
    if documenttype == 'ICs':
        cursor_obj.execute("SELECT * FROM ics WHERE icid = ?;", (documentid,))
    else:
        cursor_obj.execute("SELECT * FROM documents WHERE documentid = ? AND documenttype = ?;", (documentid,documenttype))
    output = cursor_obj.fetchall()
    conn.close()
    return output 

def get_carousels_from_db(documentid,documenttype):
    conn = sqlite3.connect(DB)
    conn.row_factory = sqlite3.Row
    cursor_obj = conn.cursor()
    cursor_obj.execute("SELECT * FROM carousels WHERE documentid = ? AND documenttype = ?;", (documentid,documenttype))
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

    
def write_IC(filename,chipinfo):

    chip=chipinfo["ic"] 
    newgroupname=chipinfo["parent"]

    newfilename=filename + ".new.rst"
    with open(newfilename, "w") as c:
        c.write(':orphan:\n\n')
        c.write('.. _' + chipinfo["tag"] + ':\n\n')
        
        locationfull = "TBD"

        if chipinfo["Storage"] == 'S' or chipinfo["Storage"] == '':
            MD=".. #Metadata {'Product':'" + chipinfo["icid"] + "','Name':'" + chipinfo["Name"] + "','Storage': 'S','Drawer':0,'Row':0,'Column':0}\n\n"
            locationfull = "TBD"
        else:
            MD=".. #Metadata {'Product':'" + chipinfo["icid"] + "','Name':'" + chipinfo["Name"] + "','Storage': '" + chipinfo["Storage"] + "','Drawer':" + chipinfo["Drawer"] + ",'Row':" + chipinfo["Row"] + ",'Column':" + chipinfo["Col"] + "}\n\n"
            locationtext=chipinfo["Storage"] + ', Drawer ' + str(chipinfo["Drawer"]) + ', Row ' + str(chipinfo["Row"]) + ', Column ' + str(chipinfo["Col"])
            locationanglebrackets='<' + chipinfo["Storage"] + ', Drawer ' + chipinfo["Drawer"] + '>'
            locationfull=':ref:`'+locationtext  + ' ' + locationanglebrackets.replace(' ','_').replace(',','') + '`'    
        if chipinfo["Storage"] == "Briefcase":
            MD=".. #Metadata {'Product':'" + chipinfo["icid"] + "','Name':'" + chipinfo["Name"] + "','Storage': 'Briefcase'}\n\n"
            locationfull=':ref:`Briefcase <Briefcase_MES6800_Briefcase_MES6800>`'    
        if chipinfo["Storage"] == "MEK6800D2":
            MD=".. #Metadata {'Product':'" + chipinfo["icid"] + "','Name':'" + chipinfo["Name"] + "','Storage': 'MEK6800D2'}\n\n"
            locationfull=':ref:`MEK6800D2 <Components_attached_to_the_MEK6800D2_board_Components_attached_to_the_MEK6800D2_board>`'    
        if 'M68MM' in chipinfo["Storage"]:
            MD=".. #Metadata {'Product':'" + chipinfo["icid"] + "','Name':'" + chipinfo["Name"] + "','Storage': '"+ chipinfo["Storage"] +"'}\n\n"
            locationfull=':ref:`' + chipinfo["Storage"] +' Micromodule <Components_attached_to_the_' + chipinfo["Storage"] + '_Micromodule_Components_attached_to_the_' + chipinfo["Storage"] + '_Micromodule>`'    
            print(locationfull)


        c.write(MD)
        
        c.write(chipinfo["name"] + '\n')
        c.write('=' * len(chipinfo["name"]) + '\n\n')
        imgs=get_images_from_db(chipinfo["icid"],"ICs")
        for img in imgs:
            c.write('.. image:: ' + img["image"] + '\n')
            c.write('   :width: 400\n')
            c.write('   :align: center\n\n')

        if chipinfo["notes"] != '':
            c.write(chipinfo["notes"])

        c.write('.. rubric:: Specific Information\n\n')
        c.write('.. csv-table:: \n')
        c.write('   :widths: auto\n\n')
        c.write('   "Date Code","'+chipinfo["date_code"]+'"\n')
        c.write('   "Manufacture Date","'+chipinfo["manufacture_date"]+'"\n')        
        c.write('   "Mask","'+chipinfo["mask"]+'"\n')
        c.write('   "Packaging","'+chipinfo["packaging"]+'"\n')
        c.write('   "Status","'+chipinfo["manufacture_status"]+'"\n')
        c.write('   "Location","' + locationfull + '"\n')
        c.write('   "Temperature","'+chipinfo["temperature_raw"]+'"\n')
        c.write('   "Frequency","'+chipinfo["frequency"]+'"\n')
        c.write('   "Notes",""\n\n')
        c.write('.. rubric:: Collection Information\n\n')
        c.write('.. csv-table:: \n')
        c.write('   :header: "Acquired"\n') 
        c.write('   :widths: auto\n\n')
        if chipinfo["status"] == 'present':
            c.write('   '+ CHECK_MARK + ' ' + chipinfo["acquired_date"]+'\n')     
        else:
            c.write('   |'+ chipinfo["status"] + '|\n')

        c.write('\n')
        lnks=get_links_from_db(chipinfo["icid"],"ICs")
        if len(lnks) > 1:
            endline='\n'
        else:
            endline=''
        if len(lnks) > 0:
            c.write('.. rubric:: Links\n')
            for lnk in lnks:
                c.write('\n' + lnk["link"] + endline)
    return 

while True:
    console.print("\t  Main Menu\n",style="bold black")
    console.print('\t1 Get date range from week ',style="info")
    console.print('\t2 Create new entry  ',style="info")
    console.print('\t3 Create new IC group index',style="info")    
    console.print('\t4 Create new IC group from index',style="info")    
    console.print('\t5 Update IC status (WIP)',style="warning")
    console.print('\t6 Unused',style="danger")
    console.print('\t7 Rebuild DB',style="info")
    console.print('\t- Delete DB',style="danger")
    console.print('\t0 Update ALL ',style="info")
    console.print('\tX Exit',style="info")
    choicesList=["1", "2","3","4","5","7","-","0","A","X","?","9"]
    type = Prompt.ask("Enter choice: ",choices=choicesList, default="?", case_sensitive=False,show_choices=False)
    match type:
        case "1":
            #Get year and week from user
            y = Prompt.ask("Enter Year e.g. 1977")
            w = Prompt.ask("Enter week e.g. 1-53")
            # Call function to get dates range 
            firstdate, lastdate =  getDateRangeFromWeek(y,w)
            output = 'Date Range for week ' + str(w) + ' in year ' + str(y) + ' is from ' + firstdate + ' to ' +  lastdate
            console.print(output, style="info")
        case "2":
            index_entry = do_create()
            console.print(index_entry,style="info")
        case "3":
            create_new_group_index()
        case "4":
            create_new_group_from_index()        
        case "0":
            import os
            clear = lambda: os.system('clear')
            clear()
            output='\nSetting Up icons\n\n'
            console.print(output, style="info")
            setup_icons()
            output='Commencing rebuild of database'
            console.print(output, style="info")
            rebuild_db()
            console.print('Completed rebuild of database',style="info")     
            console.print('\n\nCommencing updating storage metadata links',style="info")

            rstfiles = glob.glob('**/*.rst', recursive=True)
            for file in rstfiles:
                update_or_not_metadata(file)
        

            update_carousel()
            update_IC_pre_fragments()
            update_IC_index()
            update_storage()
            do_collection()
            do_statistics()

            console.print('Collection updated',style="info")
            ANYUNDEROFFER=do_underoffer()
            ANYINTRANSIT=do_in_transit()
            do_index_contents(ANYUNDEROFFER,ANYINTRANSIT)
            console.print('In-Transit updated',style="info")
            do_timeline()
            console.print('\n\n      Handing control to Sphinx\n\n\n',style="info")
            os.system("make clean html")

        case "5":
            ic = input("Enter IC to change status: ")
            statement = "SELECT * FROM ics WHERE icid = '" + ic + "';"
            output = read_db(statement)
            c=0
            for row in output:
               c=c+1 
            if c == 0:
                console.print('IC ' + ic + ' not found',style="danger")
                break
            if c > 1:
                console.print('IC ' + ic + ' found multiple times - manual update required',style="danger")
                continue
        
            if c == 1:
                for row in output:    
                    filename = row["filename"]
                    console.print('Current status of ' + row["icid"] + ' is ' + row["status"],style="info")
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
                        
                        loc ='"Location",":ref:`Storage Box @, Drawer !, Row #, Column + <Storage_Box_@_Drawer_!>`"'.replace('@',storagebox).replace('!',drawer).replace('#',rownum).replace('+',column)

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
                                    console.print('Added Metadata to IC information',style="info")
                                    writeln=True

                                if "Temperature" in line and not maskfound:
                                    f.write(line)
                                    f.write('   "Mask","' + mask + '"\n')
                                    console.print('Added Mask to IC information',style="info")
                                    writeln=True

                                if line.startswith('.. image::'):
                                    ifile='../../../../images/Hardware/ICs/' + row["parent"] + '/' + row["icid"] +'.png'
                                    newimage = '.. image:: ' + ifile + '\n'
                                    f.write(newimage)
                                    if not os.path.exists(newimage):
                                        console.print('Warning: Image ' + ifile + ' not found',style="warning")
                                    writeln=True
                                
                                if "Date Code" in line:
                                    newline = '   "Date Code","' + manudate + '"\n'
                                    f.write(newline)
                                    writeln=True

                                if "Manufacture Date" in line:
                                    newline = '   "Manufacture Date","' + realmanudate + '"\n'
                                    f.write(newline)
                                    writeln=True

                                if "Location" in line:
                                    if drawer != '':
                                        f.write('   ' + loc + '\n')
                                    else:
                                        f.write('   "Location","TBD"\n')   
                                    writeln=True
                                if not writeln:
                                    f.write(line)

                        console.print('Updated status : Now assess storage metadata',style="info")    


        case "7":
            output='Commencing rebuild of database'
            console.print(output, style="info") 
            rebuild_db()
            output='Completed rebuild of database'
            console.print(output, style="info") 
            
        case "9":
            output=read_db("SELECT * FROM ics WHERE parent = 'MC68488' order by icid asc;")
            for chipinfo in output:
                filename=chipinfo["filename"]
                write_IC(filename,chipinfo)
                newfilename = filename + ".new.rst"
                issame=compare_files(filename,newfilename)
                if issame:
                    #console.print(os.path.basename(filename) + ' is identical to ' + os.path.basename(newfilename),style="info")
                    os.remove(newfilename)
                    pass;
                else:
                    console.print(os.path.basename(filename) + ' is different to ' + os.path.basename(newfilename),style="danger")

            
            
            #output = read_db("SELECT * FROM documents WHERE documenttype='Software/Resident/EXORset30ROMS' order by filename asc;")
            output = read_db("SELECT * FROM documents WHERE documenttype='FRED' order by filename asc;")
            for row in output:
                documenttype=row["documenttype"]
                documentid=row["documentid"]

                newfilename=row["filename"]+'.new.rst'
                filename=row["filename"]

                with open(newfilename, 'w') as f:
                    f.write(':orphan:\n\n')
                    f.write('.. _' + row["tag"] + ':\n\n')        
                    f.write('.. #Metadata '+ row["metadata"] + '\n\n')
                    f.write(row["name"] + '\n')
                    for i in range(0,len(row["name"])):
                        f.write('=')
                    f.write('\n\n')

                    imgs=get_images_from_db(documentid,documenttype)
                    for img in imgs:
                        f.write('.. image:: ' + img["image"] + '\n')
                        f.write('   :width: 400\n')
                        f.write('   :align: center\n\n')

                    carousels=get_carousels_from_db(documentid,documenttype)
                    for carousel in carousels:
                        f.write('.. include:: ' + carousel["carouselfile"] + '\n\n')
                        
                    notes=get_notes_from_db(documentid,documenttype)
                    if len(notes) > 0:
                        f.write(row["notes"] )

                    f.write('.. rubric:: Collection Information\n\n')
                    f.write('.. csv-table:: \n')
                    f.write('   :header: "Acquired"\n')
                    f.write('   :widths: auto\n\n')

                    if row["status"] == BIGGER_DOC:
                        f.write('   ' + row["status"] + ' ' + row["bigger"] + ' ' + row["acquired_date"])
                    else:
                        f.write('   |' + row["status"] + '|')
                        if row["status"] == 'present':
                            f.write(' '+ row["acquired_date"])
                        
                    f.write('\n\n')
                    lnks=get_links_from_db(documentid,documenttype)
                    if len(lnks) > 1:
                        endline='\n'
                    else:
                        endline=''
                    if len(lnks) > 0:
                        f.write('.. rubric:: Links\n')
                        for lnk in lnks:
                            f.write('\n' + lnk["link"] + endline)

                issame=compare_files(filename,newfilename)
                if issame:
                    #console.print(os.path.basename(filename) + ' is identical to ' + os.path.basename(newfilename),style="info")
                    os.remove(newfilename)
                    pass;
                else:
                    console.print(os.path.basename(filename) + ' is different to ' + os.path.basename(newfilename),style="danger")

        case "?":
            pass
        case "-":
                console.print("This is destructive", style="danger")
                choice = Prompt.ask("Do you really want to delete the database?", choices=["Y", "N"], default="N", case_sensitive=True)
                if choice == 'Y':
                    os.remove(DB)
                    console.print("Database deleted", style="danger")

        case "X"|"x":
            console.print("Exiting", style="info")
            exit()
        
        case _:
            console.print("Invalid Choice", style="warning")
            
        


