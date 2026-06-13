import os
import cv2
import fitz  # PyMuPDF
import numpy as np
from pypdf import PdfReader, PdfWriter


def find_image_on_page(page_image_bytes, template_path, threshold=0.8):
    # Convert page bytes to OpenCV image
    nparr = np.frombuffer(page_image_bytes, np.uint8)
    page_img = cv2.imdecode(nparr, cv2.IMREAD_GRAYSCALE)

    # Load template
    template = cv2.imread(template_path, cv2.IMREAD_GRAYSCALE)
    if template is None:
        raise FileNotFoundError(
            f"Template image not found at {template_path}"
        )

    page_h, page_w = page_img.shape[:2]
    temp_h, temp_w = template.shape[:2]

    # --- AUTOMATIC RESIZE FOR HIGH-RES TEMPLATES ---
    if temp_w > page_w or temp_h > page_h:
        # Calculate how much we need to shrink it to fit (e.g., max 80% of page width)
        scale_w = (page_w * 0.8) / temp_w
        scale_h = (page_h * 0.8) / temp_h
        scale = min(scale_w, scale_h)

        new_w = int(temp_w * scale)
        new_h = int(temp_h * scale)

        # Resize the template to fit inside the page boundaries
        template = cv2.resize(
            template, (new_w, new_h), interpolation=cv2.INTER_AREA
        )
    # -----------------------------------------------

    # Run template matching
    result = cv2.matchTemplate(page_img, template, cv2.TM_CCOEFF_NORMED)
    _, max_val, _, _ = cv2.minMaxLoc(result)

    return max_val >= threshold

def split_pdf_by_image_delimiter(
    input_pdf_path, template_image_path, output_dir, threshold=0.5
):
    """Splits a PDF into separate files every time a specific image delimiter is found."""
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)

    # Open the PDF with PyMuPDF for visual rendering
    doc = fitz.open(input_pdf_path)
    # Open the PDF with pypdf for extraction/saving
    reader = PdfReader(input_pdf_path)

    writer = PdfWriter()
    pdf_count = 1

    print("Processing PDF pages...")

    for page_num in range(len(doc)):
        page = doc[page_num]

        # Render page to an image (PNG) in memory
        pix = page.get_pixmap(dpi=300)
        image_bytes = pix.tobytes("png")

        # Check if the delimiter image is on this page
        is_delimiter = find_image_on_page(
            image_bytes, template_image_path, threshold
        )

        # If it's a delimiter and we already have accumulated pages, save the previous chunk
        if is_delimiter and len(writer.pages) > 0:
            output_filename = os.path.join(
                output_dir, f"split_part_{pdf_count}.pdf"
            )
            with open(output_filename, "wb") as f:
                writer.write(f)
            print(f"Saved: {output_filename}")
            pdf_count += 1
            writer = PdfWriter()  # Reset writer for the next chunk

        # Add the current page to the active writer
        writer.add_page(reader.pages[page_num])

    # Save the final chunk remaining in the writer
    if len(writer.pages) > 0:
        output_filename = os.path.join(
            output_dir, f"split_part_{pdf_count}.pdf"
        )
        with open(output_filename, "wb") as f:
            writer.write(f)
        print(f"Saved: {output_filename}")

    print("PDF splitting complete!")


# --- Execution ---
if __name__ == "__main__":
    INPUT_PDF = "input.pdf"  # Path to your main PDF
    DELIMITER_IMAGE = "delimiter.png"  # Path to your delimiter image crop
    OUTPUT_FOLDER = "output"  # Where to save the split PDFs

    split_pdf_by_image_delimiter(
        INPUT_PDF, DELIMITER_IMAGE, OUTPUT_FOLDER, threshold=0.25
    )