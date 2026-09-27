def do_get_other_storage(DB):
    statement="SELECT name,description from storage where type != 'Folder' and type != 'Box' order by name,description;"
    output = read_db(statement,DB)
    others=[]
    for row in output:
        new_row={'Name':row["Name"],"Description":row["Description"]}
        others.append(new_row)
    return others


def do_get_storage_boxes(DB):
    statement="SELECT name,description from storage where type = 'Box' order by name,description;"
    output = read_db(statement,DB)
    boxes=[]
    for row in output:
        
        box_id=row["Name"]
        desc=row["Description"]
        statement="SELECT sb,drawer,rows,columns from drawers where sb = '"+box_id+"';"
        db_boxes = read_db(statement,DB)
        for box in db_boxes:
            abbrev=[]
            abbrev.append(box["sb"])
            abbrev.append("D")
            abbrev.append("R")
            abbrev.append("C")
            statement="SELECT drawer,rows,columns from drawers where sb = '"+box_id+"' order by drawer;"
            db_drawer = read_db(statement,DB)
            drawers=[]
            for d in db_drawer:
                d_drawer=d["drawer"]
                d_rows=d["rows"]
                d_columns=d["columns"]
                drawers.append({'Drawer':d_drawer,'Row':d_rows,'Columns':d_columns})
        new_row={'Name':desc,"Abbreviations":abbrev,"Drawers":drawers}
        #print(new_row)
        boxes.append(new_row)
    return boxes

def do_get_artefacts_for_flder(DB,folder):
    #from .db import read_db
    statement="SELECT * from documents where location = '" + folder + "' order by name;"
    output = read_db(statement,DB)
    return output

def read_db(statement,DB):
    import sqlite3
    conn = sqlite3.connect(DB)
    conn.row_factory = sqlite3.Row
    cursor_obj = conn.cursor()
    cursor_obj.execute(statement)
    output = cursor_obj.fetchall()
    conn.commit()
    conn.close()
    return output

def update_db(statement,DB):
    import sqlite3
    conn = sqlite3.connect(DB)
    try:
        with sqlite3.connect(DB) as conn:
            cursor = conn.cursor()
            cursor.execute(statement )
            conn.commit()
    except sqlite3.OperationalError as e:
        print(e)

def get_links_from_db(documentid,documenttype,DB):
    import sqlite3
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

def get_images_from_db(documentid,documenttype,DB):
    import sqlite3
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
