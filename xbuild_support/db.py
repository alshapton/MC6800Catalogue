import sqlite3

def read_db(statement,DB):
    conn = sqlite3.connect(DB)
    conn.row_factory = sqlite3.Row
    cursor_obj = conn.cursor()
    cursor_obj.execute(statement)
    output = cursor_obj.fetchall()
    conn.close()
    return output

def get_links_from_db(documentid,documenttype,DB):
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