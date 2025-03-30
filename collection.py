# Script to collect together all the data from each of the MD files and collect it together to produce a single collection document.

# redirect output to "collection.md"

import glob

# Find all .txt files in the current directory and subdirectories
files = glob.glob('**/*.md', recursive=True)
with open("collection.md","w") as c:
    for file in files:
        if (file != "README.md" and 
            file != "collection.md" and
            "@" not in file):
            with open(file) as f:
                for line in f:
                    if ':white_check_mark:' in line:
                        c.write(line)
                        