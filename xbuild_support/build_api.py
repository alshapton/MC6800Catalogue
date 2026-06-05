import sqlite3
import json
import os

# Connect to your SQLite database
conn = sqlite3.connect('xbuild.db')
cursor = conn.cursor()

# Fetch data from your table (e.g., "items")
cursor.execute("SELECT name, id FROM present")
rows = cursor.fetchall()

items_list = []
for row in rows:
    item = {"id": row[1], "name": row[0]}
    items_list.append(item)
    
# Create the main list endpoint /api/items.json
with open('items.json', 'w') as f:
    json.dump(items_list, f, indent=2)

# Fetch data from your table (e.g., "items")
cursor.execute("SELECT name, id FROM notpresent")
rows = cursor.fetchall()

items_list = []
for row in rows:
    item = {"id": row[1] + ' ' +  row[0]}
    items_list.append(item)
    
# Create the main list endpoint /api/items.json
with open('notpresent.json', 'w') as f:
    json.dump(items_list, f, indent=2)


conn.close()

print("Static API generated successfully!")