# https://stackoverflow.com/a/3300514/15239951
def dict_factory(cursor, row):
    d = {}
    for idx, col in enumerate(cursor.description):
        d[col[0]] = row[idx]
    return d

def do_ADMIN_unload_storage(OSSEP,console,DB,XBS,STORAGE_FILENAME):

    import sqlite3
    import json

    tables = ['storage','drawers']
    data = {}

    con = sqlite3.connect(DB)
    con.row_factory = dict_factory
    cursor = con.cursor()

    for table in tables:
        cursor.execute(f'''SELECT * FROM {table}''')
        rows = cursor.fetchall()
        data[table] = rows

    with open(XBS + OSSEP + STORAGE_FILENAME, 'w') as fp:
        json.dump(data, fp, indent=4)
    x=input("SSS")

def do_ADMIN_load_storage(OSSEP,console,DB,XBS,STORAGE_FILENAME):

    import sqlite3
    import json
    conn = sqlite3.connect(DB)

    with open(XBS + OSSEP + STORAGE_FILENAME, "r") as file:
        file_data = json.load(file)

    table_names=file_data.keys()
    for i in table_names:
        
        statement = f'''DELETE FROM {i}'''
        conn.execute(statement)
        conn.commit()
        for j in file_data[i]:
            emptytuple=[]
            columns=''
            qmarks=''
            for col in j.keys():
                if col != 'id':
                    columns = columns + col + ','
                    qmarks = qmarks + '?,'
                    emptytuple.append(j[col])
            columns = columns[:-1]
            qmarks = qmarks[:-1]                
            inserttuple=tuple(emptytuple)
            statement = f'''INSERT INTO {i} ({columns}) VALUES ({qmarks});'''
            conn.execute(statement, inserttuple)
            conn.commit()


    return

def extract_seed_chip_info(chip_seed_file,DB):
    from .db import read_db
    import json
    chips=[]
    chiprows = read_db("SELECT * from iclist;",DB)
    print(len(chiprows))
    for chip in chiprows:
        chipjson={}
        chipjson["ID"]=chip["ic"]
        chipjson["Name"]=chip["name"]
        chipjson["Top"]=chip["ctop"]
        chipjson["Bottom"]=chip["cbottom"]
        chips.append(chipjson)
    chiplist={"Chips":chips}

    with open(chip_seed_file, 'w') as f:
        f.write(json.dumps(chiplist, indent=4))

def do_ADMIN_unload_seed(OSSEP,console,DB,XBS,filename):
    import os
    from .file_utilities import copyfile
    from rich.prompt import Prompt
    BAK_FOLDER='backups' + OSSEP + 'seeds'
    chip_seed_file = XBS + OSSEP + filename
    console.print("This is destructive", style="danger")
    choice = Prompt.ask("Do you really want to overwrite the seed chip file?", choices=["Y", "N"], default="N", case_sensitive=True)
    if choice == 'Y':
        if os.path.exists(chip_seed_file):
            from datetime import datetime
            dt=datetime.now().strftime("%Y-%m-%d_%H:%M:%S")
            backup_file = XBS + OSSEP + BAK_FOLDER + OSSEP + filename.replace('.json','_' + dt + '.bak.json')
            copyfile(chip_seed_file, backup_file)
            console.print("Created a backup file", style="info")
        extract_seed_chip_info(chip_seed_file,DB)   
            