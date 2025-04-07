# Script to create blank page


import glob
import time
import os

print("Enter the following information:")
product_name = input("  Product name: ")
product_number = input("  Product number: ")
product_type = input("  Product Type:\n      (D)atasheet,\n      (G)eneric,\n      (R)eference,\n      Reference (C)ard.\n      Software (M)onitors,\n      (E)XORciser hardware,\n      (O)ther hardware: ")
images = '../../images/'
match product_type:
    case "R":
        location = "Documents/Reference"
    case "D":
        location = "Documents/Datasheets"
    case "C":
        location = "Documents/ReferenceCards"
    case "M":
        location = "Software/Monitors"
    case "G":
        location = "Documents/Generic"
    case "E":
        location = "Hardware/EXORciser"
    case "O":
        location = "Hardware/Other"
    case _:
        print("Invalid product type")
        exit() 
orphan = input("Orphan ? (Y/N): ")
acquired = input("Acquired ? (Y/N): ")
if acquired == "Y":
    acquired = True
    acquired_date = input("Acquired date (DD-MON-YYYY): ")
    acquired_status=":material-regular:`verified;2em;sd-text-success` '" + acquired_date + '"\n\n'
else:
    acquired = False
    acquired_status = ":material-regular:`thumb_down;2em;sd-text-danger`"

OUTPUT_FILE = f"source/{location}/{product_number}.rst"
if os.path.exists(OUTPUT_FILE):
    print(f"File {OUTPUT_FILE} already exists")
    exit()

print(f"Creating file {OUTPUT_FILE}")
with open(OUTPUT_FILE,"w") as c:
    if orphan == "Y":
        c.write(':orphan:\n\n')
    c.write('.. _' + product_number + ':\n\n')
    c.write(product_name + '\n')
    for i in product_name:
        c.write('=')
    c.write('\n\n')
    c.write('.. image:: '+ images + product_number + '.png\n')
    c.write('   :width: 400\n')
    c.write('   :align: center\n\n')

    c.write('.. rubric:: Collection Information\n\n')
    c.write('.. csv-table:: \n')
    c.write('   :header: "Acquired"\n')
    c.write('   :widths: auto\n\n')     

    c.write('   ' + acquired_status)

    
