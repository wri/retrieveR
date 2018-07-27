#!/usr/bin/env python
# -*- coding: utf-8 -*-

# This requires ghostscript and imagemagick
# you can brew install imagemagick
# brew install ghostscript
# the python dependencies Image and pytesseract can be installed with
# pip3 install pytesseract
# pip3 install Image

import os
import sys 
import shutil
from tika import parser

def convert_pdf(filename, output_path, resolution=150):
    """ Convert a PDF into images.

        All the pages will give a single png file with format:
        {pdf_filename}-{page_number}.png

        The function removes the alpha channel from the image and
        replace it with a white background.
    """
    outfile = os.path.splitext(os.path.basename(filename))[0]
    #print(outfile)
    texttemp = parser.from_file(filename)
    texttemp = texttemp.get('content')
    if texttemp:
        texttemp = texttemp.encode("utf-8")
    if not texttemp:
        texttemp = "NA"
    #print(texttemp)
    outfile = os.path.splitext(os.path.basename(filename))[0]
    f= open('/Users/johnbrandt/Documents/GitHub/wri-text-analysis/word2vec/prep/background-texts/' + outfile + ".txt","w+")
    f.write(texttemp)
    f.close()
        

begin = '/Users/johnbrandt/Documents/GitHub/wri-text-analysis/word2vec/prep/background-files/'
path1 = '/Users/johnbrandt/Documents/GitHub/wri-text-analysis/word2vec/prep/background-texts/'

init_files = os.listdir(begin)
print(init_files)
pdf_files = []

for idx in init_files:
    if "pdf" in idx.lower():
        pdf_files.append(idx)

for i in range(len(pdf_files)):
    convert_pdf(begin + pdf_files[i], path1)



#convert_pdf(begin + "twocolumn.pdf", path1)