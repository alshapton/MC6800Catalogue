# Configuration file for the Sphinx documentation builder.
#
# For the full list of built-in configuration values, see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Project information -----------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#project-information

project = 'Motorola MC6800 Family Product Catalogue'
copyright = '2025, Andrew Shapton'
author = 'Andrew Shapton'
release = '0.1'

# -- General configuration ---------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#general-configuration

extensions = [
        'sphinx.ext.autosectionlabel',
        'sphinx_collapse',
        'sphinx_design',
        'linuxdoc.rstFlatTable'
]

autosectionlabel_prefix_document = True

templates_path = ['_templates']
exclude_patterns = []
root_prefix = '/Users/andrew/MyProjects/Motorola-6800/MC6800Catalogue'



# -- Options for HTML output -------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#options-for-html-output

html_theme = 'alabaster'

html_static_path = ['_static']

html_css_files = [
    'css/custom.css',
    'https://fonts.googleapis.com/css?family=Material+Icons|Material+Icons+Outlined|Material+Icons+Round'
      
]


