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
