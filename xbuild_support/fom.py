
def do_FOM_swap_IC(OSSEP,console,DB,CHECK_MARK):
    from .db import read_db, update_db
    from rich.prompt import Prompt
    from .functions import write_IC

    output="This is experimental - CTRL-C to exit"
    console.print(output, style="danger")           
    first_IC = input("Enter IC 1: ")
    first_IC = "MC6882AL"
    
    statement = "SELECT * FROM ics WHERE icid = '" + first_IC + "';"

    output = read_db(statement,DB)
    c=0
    for row in output:
        c=c+1 
    if c == 0:
        console.print('First IC ' + first_IC + ' not found',style="danger")
        return
    if c > 1:
        console.print('First IC ' + first_IC + ' found multiple times - manual update required',style="danger")
        return

    if c == 1:
        for row in output:    
            filename1 = row["filename"]
            if (row["status"] != 'present'):
                console.print('Current status of ' + row["icid"] + ' is ' + row["status"],style="info")
                
                return
    status1=row["status"]
    storage1=row["storage"]
    drawer1=row["drawer"]
    row1=row["row"]
    col1=row["col"]
    filename1 = row["filename"]

    second_IC = input("Enter IC 2: ")

    
    second_IC = "MC68A00L"

    statement = "SELECT * FROM ics WHERE icid = '" + second_IC + "';"

    output = read_db(statement,DB)
    c=0
    for row in output:
        c=c+1 
    if c == 0:
        console.print('Second IC ' + second_IC + ' not found',style="danger")
        return
    if c > 1:
        console.print('Second IC ' + second_IC + ' found multiple times - manual update required',style="danger")
        
        return
    status2=row["status"]
    storage2=row["storage"]
    drawer2=row["drawer"]
    row2=row["row"]
    col2=row["col"]
    filename2 = row["filename"]
    if c == 1:
        for row in output:    
            filename2 = row["filename"]
            if (row["status"] != 'present'):
                console.print('Current status of ' + row["icid"] + ' is ' + row["status"],style="info")
                return
            
    console.print('Current status of ' + first_IC + ' is ' + status1 + '[' + storage1 + '/' + drawer1 + '/' + row1 + '/'+ col1 + ']',style="info")
    console.print('Current status of ' + second_IC + ' is ' + status2 + '[' + storage2 + '/' + drawer2 + '/'+row2 + '/'+col2 + ']',style="info")

    
    choice = Prompt.ask("Are these suitable to be swapped?", choices=["Y", "N"], default="N", case_sensitive=True)
    if choice == 'Y':
        # Do Swap

        statement = "UPDATE ics " + \
        "set storage = '" + storage1 + "', " +  \
        "    drawer = '" + drawer1 + "', " +  \
        "    row = '" + row1 + "', " +  \
        "    col = '" + col1 + "' " +  \
        "WHERE icid = '" + second_IC + "';"
        output = update_db(statement,DB)

        statement = "UPDATE ics " + \
        "set storage = '" + storage2 + "', " +  \
        "    drawer = '" + drawer2 + "', " +  \
        "    row = '" + row2 + "', " +  \
        "    col = '" + col2 + "' " +  \
        "WHERE icid = '" + first_IC + "';"
        output = update_db(statement,DB)

        console.print('Swapping has been done at the database.',style="info")
        # Now we can generate the files


        output=read_db("SELECT * FROM ics WHERE icid = '" + first_IC + "';",DB)
        for chipinfo in output:
            write_IC(filename1,chipinfo,CHECK_MARK,DB)
        
        output=read_db("SELECT * FROM ics WHERE icid = '" + second_IC + "';",DB)
        for chipinfo in output:
            write_IC(filename2,chipinfo,CHECK_MARK,DB)
        
        console.print('Swapping has been done at the file level.',style="info")
        console.print('\nSwapping is complete.',style="info")

    return

def rename_files_in_folder(folder_path_str):
    
    from pathlib import Path
    
    renamed_files = []
    error_files   = []

    # Convert the string input to a Path object
    folder_path = Path(folder_path_str)
    
    # Check if the folder actually exists
    if not folder_path.exists():
        raise FileNotFoundError()
        
    if not folder_path.is_dir():
        raise NotADirectoryError()
        
    # Get just the folder name (e.g., /home/user/Pics -> Pics)
    folder_name = folder_path.name
    
    # Initialize the incremental counter
    counter = 1
    
    # Iterate through all items in the folder
    for item in folder_path.iterdir():
        # Only rename files, skip any subfolders
        if item.is_file():
            # Extract the original file extension (e.g., .jpg, .txt)
            file_extension = item.suffix
            
            # Construct the new file name: 1.FOLDERNAME.jpg
            new_name = f"{counter}.{folder_name}{file_extension}"
            
            # Create the full new path
            new_file_path = folder_path / new_name
            
            try:
                # Rename the file
                item.rename(new_file_path)
                filetext = f"Renamed: '{item.name}' -> '{new_name}'"
                renamed_files.append(filetext)
                counter += 1
            except Exception as e:
                errortext = f"Failed to rename '{item.name}': {e}"
                error_files.append(errortext)

    return renamed_files,error_files    

def do_FOM_rename_files(OSSEP,console):
    
    folder_path_str = input("Enter foldername (relative to MC6800Catalogue: ")
    folder_path_str = "." + OSSEP + folder_path_str
    try:
        renamed_files,error_files = rename_files_in_folder(folder_path_str)  
    except FileNotFoundError:
        console.print(f"Error: '{folder_path_str}' does not exist.", style="danger")
    except NotADirectoryError:
        console.print(f"Error: '{folder_path_str}' is not a directory.", style="danger")
    else:
        if len(renamed_files) > 0:
            for file in renamed_files:
                console.print(file,style="info")
        if len(error_files) > 0:
            for file in error_files:
                console.print(file,style="danger")
