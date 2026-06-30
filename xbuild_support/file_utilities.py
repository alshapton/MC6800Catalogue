def copy_and_replace(source_path, destination_path):
    import shutil
    import os
    if os.path.exists(destination_path):
        os.remove(destination_path)
    shutil.copy2(source_path, destination_path)


def remove_lines_between(file_path: str, start_val: str, end_val: str, include_markers: bool = True) -> None:
    from pathlib import Path

    """
    Removes all lines in a file between start_val and end_val, 
    then overwrites the original file.
    
    :param file_path: Path to the target file.
    :param start_val: The string that signals the start of deletion.
    :param end_val: The string that signals the end of deletion.
    :param include_markers: If True, deletes the marker lines themselves. 
                            If False, keeps the marker lines and only deletes what's between them.
    """
    path = Path(file_path)
    if not path.exists():
        raise FileNotFoundError(f"The file {file_path} does not exist.")

    kept_lines = []
    inside_deletion_zone = False

    # Read and process the file lines
    with open(path, 'r', encoding='utf-8') as file:
        for line in file:
            # Strip trailing newlines/whitespace for an accurate match
            stripped_line = line.strip()

            if stripped_line == start_val:
                inside_deletion_zone = True
                if not include_markers:
                    kept_lines.append(line)  # Keep the start marker
                continue  # Skip adding this line if include_markers is True

            if stripped_line == end_val:
                inside_deletion_zone = False
                if not include_markers:
                    kept_lines.append(line)  # Keep the end marker
                continue  # Skip adding this line if include_markers is True

            # If we aren't in the deletion zone, keep the line
            if not inside_deletion_zone:
                kept_lines.append(line)

    # Rewrite the file back with the kept lines
    with open(path, 'w', encoding='utf-8') as file:
        file.writelines(kept_lines)

# --- Example Usage ---
# If you have a file containing:
#   Line 1
#   START_HERE
#   Line 2 (delete me)
#   Line 3 (delete me)
#   END_HERE
#   Line 4
#
# Calling: remove_lines_between("data.txt", "START_HERE", "END_HERE", include_markers=True)
# Results in:
#   Line 1
#   Line 4



def replace_line_in_file(file_path: str, old_line: str, new_line: str) -> None:
    from pathlib import Path

    """
    Finds a specific line in a file and replaces it with a new line,
    then overwrites the original file.
    
    :param file_path: Path to the target file.
    :param old_line: The exact text of the line you want to replace.
    :param new_line: The text you want to put in its place.
    """
    path = Path(file_path)
    if not path.exists():
        raise FileNotFoundError(f"The file '{file_path}' does not exist.")

    updated_lines = []
    match_found = False

    # Read and process the file line by line
    with open(path, 'r', encoding='utf-8') as file:
        for line in file:
            # Strip newline characters to compare the actual text content
            if line.rstrip('\r\n') == old_line:
                # Append the new line (ensuring it ends with a newline character)
                updated_lines.append(new_line + '\n')
                match_found = True
            else:
                updated_lines.append(line)

    if not match_found:
        print(f"Warning: The line '{old_line}' was not found in the file.")
        return

    # Write the updated lines back to the original file
    with open(path, 'w', encoding='utf-8') as file:
        file.writelines(updated_lines)
    



def prepend_file(source_path: str, target_path: str) -> None:
    """
    Prepends the content of source_path to the beginning of target_path.
    """
    from pathlib import Path

    source = Path(source_path)
    target = Path(target_path)
    
    # Read content from both files
    source_content = source.read_text(encoding='utf-8')
    target_content = target.read_text(encoding='utf-8')
    
    # Overwrite target with combined content
    target.write_text(source_content + target_content, encoding='utf-8')

def copyfile(old, new):
    import shutil
    shutil.copyfile(old, new)

def movefile(old, new):
    import shutil
    shutil.move(old, new)

def make_directory(path):
    import os
    try:
        os.mkdir(path)
        return True
    except FileExistsError:
        return False


def line_prepender(filename, line):
    with open(filename, 'r+') as f:
        content = f.read()
        f.seek(0, 0)
        f.write(line.rstrip('\r\n') + '\n\n' + content)

def compare_files(filename, diff_file):
    hash1 = file_hash(filename)
    hash2 = file_hash(diff_file)
    if hash1 == hash2:
        return True
    else:
        return False

def file_hash(path):
    import hashlib

    hasher = hashlib.sha256()
    with open(path, 'rb') as file:
        for chunk in iter(lambda: file.read(4096), b""):
            hasher.update(chunk)
    return hasher.hexdigest()