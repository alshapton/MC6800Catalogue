import sqlite3
import json
import os

# Connect to your SQLite database
conn = sqlite3.connect('xbuild.db')
cursor = conn.cursor()

# Create an output directory for your API endpoints
os.makedirs('../docs/build/html/api', exist_ok=True)

# Fetch data from your table (e.g., "items")
cursor.execute("SELECT name, id FROM present")
rows = cursor.fetchall()

items_list = []
for row in rows:
    item = {"id": row[1], "name": row[0]}
    items_list.append(item)
    
    # Optional: Create individual endpoint files like /api/items/1.json
    os.makedirs('../docs/build/html/api/items', exist_ok=True)
    with open(f'../docs/build/html/api/items/{row[1]}.json', 'w') as f:
        json.dump(item, f, indent=2)

# Create the main list endpoint /api/items.json
with open('../docs/build/html/api/items.json', 'w') as f:
    json.dump(items_list, f, indent=2)

conn.close()
print("Static API generated successfully!")