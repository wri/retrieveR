#!/usr/bin/env python
# -*- coding: utf-8 -*-

# This requires ghostscript and imagemagick
# you can brew install imagemagick
# brew install ghostscript
# the python dependencies Image and pytesseract can be installed with
# pip3 install pytesseract
# pip3 install Image

from PyPDF2 import PdfFileWriter, PdfFileReader
import os
import sys 
import shutil
import re
from tika import parser

begin = '/Users/johnbrandt/Documents/GitHub/wri-text-analysis/ocr/begin/'
path1 = '/Users/johnbrandt/Documents/GitHub/wri-text-analysis/ocr/results/'

init_files = os.listdir(begin)
pdf_files = []

for idx in init_files:
	if "pdf" in idx.lower():
		pdf_files.append(idx)
print(pdf_files)

out_folders = lst = [None] * len(pdf_files)
for idx, file in enumerate(pdf_files):
	print(idx)
	out_folders[idx] = re.sub("[.]", "", pdf_files[idx])

print(out_folders)
for idx, file in enumerate(out_folders):
	os.mkdir(path1+out_folders[idx])

def convert_pdf(idx):
	inputpdf = PdfFileReader(open(begin+pdf_files[idx], "rb"), strict=False)
	for i in range(inputpdf.numPages):
		output = PdfFileWriter()
		output.addPage(inputpdf.getPage(i))
		with open(path1 + out_folders[idx] + "/" + "%s.pdf" % str(int(i) + 1), "wb") as outputStream:
			output.write(outputStream)

def extract_text(idx, resolution=150):
	files = os.listdir(path1 + out_folders[idx] + "/")
	for file in range(len(files)):
		print(files[file])
		outfile=os.path.splitext(os.path.basename(files[file]))[0]
		texttemp = parser.from_file(path1 + out_folders[idx] + "/" + files[file])
		texttemp = texttemp.get('content')
		if texttemp:
			texttemp = texttemp.encode("utf-8")
		if not texttemp:
			texttemp = "NA"
		f= open(path1 + out_folders[idx] + "/" + outfile + ".txt","w+")
		f.write(texttemp)
		f.close()
	pdfs = os.listdir(path1 + out_folders[idx] + "/")
	for i in pdfs:
		if "pdf" in i.lower():
			os.remove(path1 + out_folders[idx] + "/" + i)

for idx in range(len(pdf_files)):
	print(idx)
	convert_pdf(idx)
	extract_text(idx)



#convert_pdf(begin + "twocolumn.pdf", path1)