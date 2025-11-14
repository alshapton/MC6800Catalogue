
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