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
