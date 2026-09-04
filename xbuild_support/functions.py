
def produce_other_snippets_files(IC_LOCATIONS,OSSEP,DB,XBS):
    from .fom import do_get_folders,do_get_others
    from .db import read_db,do_get_artefacts_for_flder
    SOURCE='docs/Documents/Hardware/ICs/snippets/tables.fragment.'
    
    statement="SELECT distinct(location) FROM ics where location like '%<%>%' and location not like '%Storage%' order by location ;"
    output = read_db(statement,DB)
    for row in output:
        location=row["location"]
        loc=location.split('<')
        locright=loc[1][:-2]
        mainlabel=locright[:int(len(locright)/2)]
        FILENAME = SOURCE + mainlabel + '.' + mainlabel + '.snippet'

        with open(FILENAME,"w") as opf:

            opf.write('.. _' + mainlabel + '_' + mainlabel+':\n')
            opf.write('\n')
            opf.write('\n.. rubric:: ' +  mainlabel.replace('_',' ') + '\n')
            opf.write('\n')
            opf.write('.. csv-table::\n')
            opf.write('   :header-rows: 0\n')
            opf.write('   :widths: 50,50\n')
            opf.write('\n')
            
            statement="SELECT icid,name FROM ics where location = '" + location + "' order by icid;"
            ics = read_db(statement,DB)
            for ic in ics:
                opf.write('   |i'+ic["icid"] + '|, :ref:`'+ic["name"]+'<'+ic["icid"]+'>`\n')
                

    with open('mapfile',"w") as opf:

        folders=do_get_folders(DB)    
    
        for folder in folders:
            documents = do_get_artefacts_for_flder(DB,folder)
            number_of_documents = len(documents)
            if number_of_documents > 0:                
                label='\n\n.. _'+folder.replace(' ','_') + '_map_reference:\n'
                opf.write('\n')
                opf.write(label)
                opf.write('\n.. rubric:: Folder ' +  folder + '\n')
                opf.write('\n')
                opf.write('.. csv-table::\n')
                opf.write('   :header: "Name","Comments"\n')
                opf.write('   :widths: 60,40\n')
                opf.write('\n')
                
                for document in documents:
                    opf.write('    :ref:`' + document["name"] + '<' + document["documentid"] + '>`,"' + document["comments"] + '"\n')
                opf.write('\n')
                opf.write('\n')


        others=do_get_others(DB)    
    
        for other in others:
            documents = do_get_artefacts_for_flder(DB,other)
            number_of_documents = len(documents)
            if number_of_documents > 0:
                print('Storage: ' + other + ' has ' + str(number_of_documents))
                
                label='\n\n.. _'+folder.replace(' ','_') + '_map_reference:\n'
                opf.write('\n')
                opf.write(label)
                opf.write('\n.. rubric:: Other ' +  folder + '\n')
                opf.write('\n')
                opf.write('.. csv-table::\n')
                opf.write('   :header: "Name","Comments"\n')
                opf.write('   :widths: 60,40\n')
                opf.write('\n')
                
                for document in documents:
                    opf.write('    :ref:`' + document["name"] + '<' + document["documentid"] + '>`,"' + document["comments"] + '"\n')
                opf.write('\n')
                opf.write('\n')
    #exit()

    # Need to do folder map for hardware here...
    return


def produce_ic_snippets_files(IC_LOCATIONS,OSSEP,DB,XBS):
    from .fom import do_get_storage_drawers,do_get_col_widths_for_drawer,do_get_IC_for_place,do_get_rows_cols_for_drawer,do_count_IC_in_SBs,do_get_rows_col_count_for_drawer,do_count_IC_in_SB_D
    from .db import read_db

    
    SB_PREFIX="Storage Box "
    SOURCE='docs/Documents/Hardware/ICs/snippets/tables.fragment.'
    # "|iMC68701CL-1| :ref:`MC68701CL-1 <MC68701CL-1>`"
    template = '"|i##| :ref:`## <##>`"'
    
    # Need to do folder map for non-storage box stuff here

    sb_drawers=do_get_storage_drawers(DB)

    # Check to see if there are any ICs in this SB first.....
    icsinboxes=do_count_IC_in_SBs(DB)
    for row in icsinboxes:
        if row["ics"]>0:
            
            
            this_sb=row["storage"].replace(SB_PREFIX,'')
            
            # Get number of drawers in this SB
            for sb_drawer in sb_drawers:
                if this_sb == sb_drawer.split('-')[0]:
                    this_draw_in_this_sb = sb_drawer.split('-')[1]
                    c_in_d = do_count_IC_in_SB_D(DB,row["storage"],this_draw_in_this_sb)
                    if c_in_d > 0:
                        rc_for_drawer=do_get_rows_cols_for_drawer(DB,this_sb,this_draw_in_this_sb)
                        columns=do_get_col_widths_for_drawer(DB,sb_drawer.split('-')[0].replace(SB_PREFIX,''),sb_drawer.split('-')[1])

                        # Now we know that there are ics in this drawer/sb combo            
                        # and we have the row/col count
                        # Create blank for this drawer
                        row_count = rc_for_drawer[0].split('-')[0]
                        col_count = rc_for_drawer[0].split('-')[1]
                        storage_row=[]
                        storage_col=[]
                        
                        for rc in range(1,int(row_count)+1):
                            storage_col=[]
                            for cc in range(1,int(col_count)+1):                                
                                IC=''
                                IC = do_get_IC_for_place(DB,row["storage"],this_draw_in_this_sb,str(rc),str(cc))
                                if IC != '':
                                    content=template.replace('##',IC,1)
                                    content=content.replace('##',IC,1).replace('2!','')
                                    content=content.replace('##',IC,1)
                                    storage_col.append(content)
                                else:
                                    storage_col.append('""')
                            storage_row.append(storage_col)

                        widths=columns[0].replace('[','').replace(']','').replace(' ','')
                        outputfile=SOURCE + row["storage"].replace(' ','_') + '.Drawer_' + sb_drawer.split('-')[1] + '.snippet'
                        with open(outputfile,"w") as opf:
                            print(widths,columns[0])

                            label='\n\n.. _'+row["storage"].replace(' ','_') + '_Drawer_' + sb_drawer.split('-')[1]+':\n'
                            opf.write(label)
                            opf.write('\n')
                            opf.write('.. rubric:: Drawer ' +  sb_drawer.split('-')[1] + '\n')
                            opf.write('\n')
                            opf.write('.. csv-table::\n')
                            opf.write('   :header-rows: 0\n')
                            print('widths=',widths)
                            opf.write('   :widths: ' + widths + '\n')
                            opf.write('\n')
                            for rc in range(0,int(row_count)):                            
                                output_row='   '
                                for col in range(len(storage_row[rc])):
                                    output_row=output_row + storage_row[rc][col] + ','
                                output_row=output_row[:-1]
                                if "<" in output_row:
                                    opf.write(output_row + '\n')
           

def cls():
    import os
    clear = lambda: os.system('clear')
    clear()
            
def getDateRangeFromWeek(p_year,p_week):
    import datetime
    firstdayofweek = datetime.datetime.strptime(f'{p_year}-W{int(p_week )- 1}-1', "%Y-W%W-%w").date()
    lastdayofweek = firstdayofweek + datetime.timedelta(days=6.9)
    #return firstdayofweek, lastdayofweek
    return firstdayofweek.strftime("%d-%b-%Y").upper(),lastdayofweek.strftime("%d-%b-%Y").upper()

def convert_type_to_real_type(type):
    doc_type=''
    match type:
        case "Documents/ApplicationNotes":
            doc_type = "Application Notes"
        case "Documents/ApplicationNotes":
            doc_type = "Brochures"            
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
    return doc_type

def insert_spaces_into_document_type(type):
    doc_type=''
    match type:
        case "ApplicationNotes":
            doc_type = "Application Notes"
        case "ICs":    
            doc_type = "ICs"
        case "Reference":
            doc_type = "Reference Documents"
        case "Manuals":
            doc_type = "Reference Manuals"
        case "Hardware/EXORciser/Micromodules":
            doc_type = "Exorciser Micromodules"
        case "Brochures":
            doc_type = "Brochures"
        case "Datasheets":
            doc_type = "Datasheets"
        case "ReferenceCards":
            doc_type = "Reference Cards" 
        case "Generic":
            doc_type = "Generic Documents"
        case "Software/NonResident":
            doc_type = "NonResident Software"
        case "Software/Resident":
            doc_type = "Resident Software"
        case "Hardware/EXORciser":
            doc_type = "Exorciser Hardware"
        case "Hardware/Other":
            doc_type = "Other Hardware"
        case "Hardware/Other":
            doc_type = "Other Hardware"
        case _:
            doc_type = "Other"  
            if "/ICs" in type:
                doc_type = "ICs"
            doc_type=type
    return doc_type

# Storage-related functions

def construct_drawer_ref(st, drawer):
    return '\n.. _' + str(st).replace(" ","") + "Drawer" + str(drawer) + ':\n'

def construct_drawer_reference(st, drawer):
    return ':ref:`' + str(st).replace(" ","") + "Drawer" + str(drawer) + '`'

def get_cols_for_drawer(st,dr,rw, info):
    import ast
    l=ast.literal_eval(str(info))
    for j in l['Storage']:
        cols = []
        for i in range(0,len(j)):

            k=j['Name']        
            if k == st:
                drws=j['Drawers']
                for d in range(0,len(drws)):
                    cd=drws[d]
                    if dr == cd['Drawer']:
                        cols = cd['Columns']
                        return cols                      
    return cols

def get_location(ref,md):
    import ast

    # Get the location of the file from the metadata
    loc = ''
    for item in md:
        if item['REFERENCE'] == ref:
            line = item['METADATA']
            loc  = line.split('.. #Metadata')[1].strip().replace("{'Info': ",'').replace('}}','}')
            loc  = ast.literal_eval(loc)
    return loc

def convert_date_format(date_str: str) -> str:
    """Converts a date string from 'DD-MMM-YYYY' (e.g., '31-JUL-2026')

    to 'YYYY-MM-DD' (e.g., '2026-07-31').
    """
    # %d: 2-digit day
    # %b: Abbreviated month name (case-insensitive in parsing)
    # %Y: 4-digit year
    from datetime import datetime
    parsed_date = datetime.strptime(date_str, "%d-%b-%Y")
    return parsed_date.strftime("%Y-%m-%d")

def convert_MMM_to_MM(month: str) -> str:
    # Convert a 3 character month to a two digit month  e.g. “MAR” -> “03” or “0” if not found
    from datetime import datetime
    try:
            return f"{datetime.strptime(month.upper(), "%b").month:02d}"
    except:
            return "0"

#def convert_MMM_to_MM(m):
#    upperm=m.upper()
#    match upperm:
#        case "JAN":
#            month = '01'
#        case "FEB":
#            month = '02'
#        case "MAR":
#            month = '03'
#        case "APR":
#            month = '04'
#        case "MAY":
#            month = '05'
#        case "JUN":
#            month = '06'
#        case "JUL":
#            month = '07'
#        case "AUG":
#            month = '08'
#        case "SEP":
#            month = '09'
#        case "OCT":
#            month = '10'
#        case "NOV":
#            month = '11'
#        case "DEC":
#            month = '12'
#        case _:
#            month='0'
#    return month

# DO SETUP OF ICONS
def setup_icons(OSSEP,XBS,CONFPY,CONVENTIONSFILENAME,mediaincfilename):

    import tomllib
    from .file_utilities import copy_and_replace as copy_and_replace

    with open("." + OSSEP  + XBS + OSSEP + "setup.toml", "rb") as f:
        data = tomllib.load(f)

    # Prepare new conf.py file
    
    PRESETUPFILE='setup.pre'
    CONFMASTER='conf.master'

    copy_and_replace('.' + OSSEP + XBS + OSSEP + CONFMASTER,'.' + OSSEP + XBS + OSSEP + PRESETUPFILE)
    with open('.' + OSSEP + XBS + OSSEP + PRESETUPFILE, 'a') as newfile:
        newfile.write("\n")
        newfile.write('rst_prolog = """\n')
        for icon in data["icons"]:
            newfile.write(".. |"+icon["name"].strip()+"| " + '\treplace:: ' + icon["icon"]+'\n')
            
        newfile.write('"""\n')

    copy_and_replace('.' + OSSEP + XBS + OSSEP + PRESETUPFILE,'.' + OSSEP + 'docs' + OSSEP + CONFPY)

    with open('.' + OSSEP + XBS + OSSEP + CONVENTIONSFILENAME, 'w') as newfile:
        newfile.write(':orphan:\n\n')
        newfile.write('.. csv-table::')
        newfile.write('   :header: "Symbol","Description"\n')
        newfile.write('   :widths: 14, 86\n')
        newfile.write('   :width: 100\n\n')

        for icon in data["icons"]:
            if (icon["tag"] == "conventions"):
                newfile.write("   |"+icon["name"].strip()+"|, " + '"' + icon["desc"]+'"\n')

    with open(mediaincfilename, 'w') as newfile:
        newfile.write('.. rubric:: Key to Symbols\n\n')
        newfile.write('.. csv-table::\n\n')
        
        for icon in data["icons"]:
            if (icon["tag"] == "media"):
                newfile.write("   |"+icon["name"].strip()+"|, " + '"' + icon["desc"]+'"\n')

def update_carousel(CAROUSEL,SUFFIX):
    import ast
    import os
    import glob
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
        images_loc_full=dotdot + images_loc.replace('docs/','')
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

def write_IC(filename,chipinfo,CHECK_MARK,DB):
    from .db import get_links_from_db, get_images_from_db
    chip=chipinfo["ic"] 
    newgroupname=chipinfo["parent"]

    newfilename=filename + ".new.rst"
    newfilename=filename 
    
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
            MD=".. #Metadata {'Product':'" + chipinfo["icid"] + "','Name':'" + chipinfo["Name"] + "','Storage': 'EDUCATORII'}\n\n"
            locationfull=':ref:`Briefcase <Briefcase_MES6800_Briefcase_MES6800>`'    
        if chipinfo["Storage"] == "EDUCATORII":
            MD=".. #Metadata {'Product':'" + chipinfo["icid"] + "','Name':'" + chipinfo["Name"] + "','Storage': 'Briefcase'}\n\n"
            locationfull=':ref:`EDUCATORII <Educator_II_Microcomputer_Kit_Educator_II_Microcomputer_Kit>`'    
        if chipinfo["Storage"] == "MEK6800D2":
            MD=".. #Metadata {'Product':'" + chipinfo["icid"] + "','Name':'" + chipinfo["Name"] + "','Storage': 'MEK6800D2'}\n\n"
            locationfull=':ref:`MEK6800D2 <Components_attached_to_the_MEK6800D2_board_Components_attached_to_the_MEK6800D2_board>`'    
        if 'M68MM' in chipinfo["Storage"]:
            MD=".. #Metadata {'Product':'" + chipinfo["icid"] + "','Name':'" + chipinfo["Name"] + "','Storage': '"+ chipinfo["Storage"] +"'}\n\n"
            locationfull=':ref:`' + chipinfo["Storage"] +' Micromodule <Components_attached_to_the_' + chipinfo["Storage"] + '_Micromodule_Components_attached_to_the_' + chipinfo["Storage"] + '_Micromodule>`'    


        c.write(MD)
        
        c.write(chipinfo["name"] + '\n')
        c.write('=' * len(chipinfo["name"]) + '\n\n')
        imgs=get_images_from_db(chipinfo["icid"],"ICs",DB)
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
        lnks=get_links_from_db(chipinfo["icid"],"ICs",DB)
        if len(lnks) > 1:
            endline='\n'
        else:
            endline=''
        if len(lnks) > 0:
            c.write('.. rubric:: Links\n')
            for lnk in lnks:
                c.write('\n' + lnk["link"] + endline)
    return 