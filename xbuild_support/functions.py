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