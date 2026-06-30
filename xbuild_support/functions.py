
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

    l=ast.literal_eval(info[0])
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



def multiline_input(prompt):
    print(prompt + '(Ctrl-D to end):')
    contents = []
    while True:
        try:
            line = input()
        except EOFError:
            break
        contents.append(line)

def convert_MMM_to_MM(m):
    upperm=m.upper()
    match upperm:
        case "JAN":
            month = '01'
        case "FEB":
            month = '02'
        case "MAR":
            month = '03'
        case "APR":
            month = '04'
        case "MAY":
            month = '05'
        case "JUN":
            month = '06'
        case "JUL":
            month = '07'
        case "AUG":
            month = '08'
        case "SEP":
            month = '09'
        case "OCT":
            month = '10'
        case "NOV":
            month = '11'
        case "DEC":
            month = '12'
        case _:
            month='0'
    return month

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
