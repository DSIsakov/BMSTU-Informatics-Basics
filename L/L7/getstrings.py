import sys, random


def function(count, length):
	s = ''
	for i in range(count):
		for j in range(length):
			s += chr(random.randint(33, 127))
		print(s)
		s = ''


sys.modules[__name__] = function
