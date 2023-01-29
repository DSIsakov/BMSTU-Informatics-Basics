#!/usr/bin/env python3
import os
import sys
import argparse

dcount = 0
fcount = 0
text = ''

def tree(path, directoryonly, output, curdepth):
	global dcount, fcount, text
	for i in os.listdir(path):
		if os.path.isdir(os.path.join(path, i)):		
			if output:
				text += '|   ' * curdepth + '|--' + i + '\n'
			else:
				print('|   ' * curdepth + '|--' + i)
			dcount += 1
			try:
				tree(os.path.join(path, i), directoryonly, output, curdepth + 1)
			except PermissionError:
				pass
		else:
			fcount += 1
			if not directoryonly:
				if output:
					text += '|   ' * curdepth + '|--' + i + '\n'
				else:
					print('|   ' * curdepth + '|--' + i)
	
	
		
		
if __name__ == "__main__":
	parser = argparse.ArgumentParser()
	parser.add_argument('-d', action='store_true')
	parser.add_argument('-o')

	args = parser.parse_known_args()[0]
	path = os.getcwd()
	
	for i in sys.argv[1:]:
		if i[0] != '-':
			path = i
	
	tree(path, args.d, args.o, 0)
	
	
	if args.o:
		if args.d:
			text += f'\n{dcount} directories'
		else:
			text += f'\n{dcount} directories, {fcount} files'
	else:
		if args.d:
			print(f'\n{dcount} directories')
		else:
			print(f'\n{dcount} directories, {fcount} files')
			
	if args.o:
		f = open(output, 'w')
		f.write(text)
		f.close()