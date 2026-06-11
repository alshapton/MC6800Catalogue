import fitz  # PyMuPDF

# Open your 250-page PDF file
doc = fitz.open("1979_Microcomputer_Development_Systems.pdf")

# Define the base URL path provided
base_url = "https://alshapton.github.io/MC6800Catalogue/_downloads/1397b33225e3385b9c05b23549440833/1979_Microcomputer_Development_Systems.pdf"

# Define the link area hotspot: Rect(X1, Y1, X2, Y2) in points
# This places a clickable box near the top left header area
link_rect = fitz.Rect(50, 30, 250, 60)

# Loop through every page using its index number
for page_num, page in enumerate(doc):
    # Determine the human-readable page number (starting at page 1)
    actual_page = page_num + 1

    # Formulate the unique link per page by adding a page query parameter
    # Example output: ...Systems.pdf?page=1, ...Systems.pdf?page=2
    unique_url = f"{base_url}?page={actual_page}"

    # Apply the unique link properties to the page link dictionary structure
    page.insert_link(
        {"kind": fitz.LINK_URI, "from": link_rect, "uri": unique_url}
    )

# Save the newly linked file to disk under a new name
doc.save("1979_Microcomputer_Development_Systems_LINKS.pdf")
print("Process complete! All unique page links successfully created.")
