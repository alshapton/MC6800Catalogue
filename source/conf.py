# Configuration file for the Sphinx documentation builder.
#
# For the full list of built-in configuration values, see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Project information -----------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#project-information



project = 'Motorola MC6800 Family Product Catalogue'
copyright = '2024-2026, Andrew Shapton'
author = 'Andrew Shapton'
release = '0.2'

# -- General configuration ---------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#general-configuration

extensions = [
        'sphinx.ext.autosectionlabel',
        'sphinx_collapse',
        'sphinx_design',
        'linuxdoc.rstFlatTable',
        'sphinx.ext.extlinks',
]

autosectionlabel_prefix_document = True
templates_path = ['_templates']
exclude_patterns = []
root_prefix = '/Users/andrew/MyProjects/Motorola-6800/MC6800Catalogue'
source_suffix = ['.rst']


# ExtLinks configuration:
extlinks = {'extlink-bitsavers': ('https://bitsavers.org/bits/Motorola/Exorcisor/%s',
                      'extlink-bitsavers %s')}

# -- Options for HTML output -------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#options-for-html-output

html_theme = 'alabaster'

html_static_path = ['_static']

html_css_files = [
    'css/custom.css',
    'https://fonts.googleapis.com/css?family=Material+Icons|Material+Icons+Outlined|Material+Icons+Round',
    'https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.1.1/css/all.min.css'
]
rst_prolog = """
.. |cancelled| 	replace:: :material-regular:`remove_shopping_cart;2em`
.. |intransit| 	replace:: :material-regular:`local_shipping;2em;`
.. |present| 	replace:: :material-regular:`verified;2em;sd-text-success`
.. |notpresent| 	replace:: :material-regular:`thumb_down;2em;sd-text-danger`
.. |advanceinfo| 	replace:: :material-regular:`lock_open;2em;sd-text-danger`
.. |confirminfo| 	replace:: :material-regular:`lock;2em;sd-text-success`
.. |2ndsource| 	replace:: :octicon:`list-ordered;2em`
.. |underoffer| 	replace:: :material-regular:`gavel;2em;sd-text-danger`
.. |magnetictape| 	replace:: :material-regular:`voicemail;2em;`
.. |floppydisc| 	replace:: :material-regular:`save;2em;`
.. |punchedcard| 	replace:: :material-regular:`margin;2em;`
.. |datacartridge| 	replace:: :material-regular:`storage;2em;`
.. |document| 	replace:: :material-regular:`document_scanner;2em;`
"""
