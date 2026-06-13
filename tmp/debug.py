import cv2
import fitz
import numpy as np


def debug_match_quality(input_pdf_path, template_path):
    doc = fitz.open(input_pdf_path)
    template = cv2.imread(template_path, cv2.IMREAD_GRAYSCALE)

    if template is None:
        print(f"Error: Could not load template image from {template_path}")
        return

    temp_h, temp_w = template.shape[:2]
    print(f"Template Size: {temp_w}x{temp_h} pixels")
    print("--- Scanning Pages ---")

    for page_num in range(len(doc)):
        page = doc[page_num]
        # Using 150 DPI. If your template is large, try changing this to 300
        pix = page.get_pixmap(dpi=300)
        nparr = np.frombuffer(pix.tobytes("png"), np.uint8)
        page_img = cv2.imdecode(nparr, cv2.IMREAD_GRAYSCALE)

        page_h, page_w = page_img.shape[:2]

        # Check if template is too big for the page
        if temp_h > page_h or temp_w > page_w:
            print(
                f"Page {page_num+1}: ERROR - Template ({temp_w}x{temp_h}) is LARGER than the page ({page_w}x{page_h})."
            )
            continue

        # Run template matching
        result = cv2.matchTemplate(page_img, template, cv2.TM_CCOEFF_NORMED)
        _, max_val, _, _ = cv2.minMaxLoc(result)

        print(
            f"Page {page_num+1} (Size {page_w}x{page_h}): Best match score = {max_val:.4f}"
        )


# Run the debug
INPUT_PDF = "input.pdf"
DELIMITER_IMAGE = "delimiter.png"
debug_match_quality(INPUT_PDF, DELIMITER_IMAGE)