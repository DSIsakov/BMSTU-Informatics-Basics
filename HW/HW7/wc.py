#!/usr/bin/env python3
import sys
import os
import argparse


def wc(args, files):
	if files:
		for file in files:
			if not os.path.exists(file):
				sys.stderr.write('wc: ' + file + ': No such file or directory\n')
				continue
				
			with open(file, 'r') as f:
				lines = 0
				words = 0
				chars = 0
				
				for line in f:
					lines += 1
					words += len(line.split())
					chars += len(line)
					
			if args.l:
				print(lines, end='\t')
			if args.w:
				print(words, end='\t')
			if args.c:
				print(chars, end='\t')
			if args.m:
				print(chars, end='\t')
			if not (args.l or args.w or args.c or args.m):
				print(lines, words, chars, end='\t', sep='\t')

				
			print(file)

	else:
		lines = 0
		words = 0
		chars = 0
		
		for l in sys.stdin: 
			lines = 0
			words = 0
			chars = 0
			
			for line in l:
				lines += 1
				words += len(line.split())
				chars += len(line)
				
		if args.l:
			print(lines, end='\t')
		if args.w:
			print(words, end='\t')
		if args.c:
			print(chars, end='\t')
		if args.m:
			print(chars, end='\t')
		if not (args.l or args.w or args.c or args.m):
			print(lines, words, chars, end='\t', sep='\t')
			
			
if __name__ == "__main__":
	parser = argparse.ArgumentParser()
	parser.add_argument('-l', action='store_true')
	parser.add_argument('-w', action='store_true')
	parser.add_argument('-c', action='store_true')
	parser.add_argument('-m', action='store_true')
	
	files = []
	for i in sys.argv[1:]:
		if i[0] != '-':
			files.append(i)
	
	args = parser.parse_known_args()
	
	wc(args[0], files)