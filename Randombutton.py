#This code generates a list of the whole numbers 1-20 and a random button which
#causes a random number from that list to be printed and then removed from the list.
#The button can be used until all 20 numbers have been picked


import random
from tkinter import *

def randomize():
	num = random.choice(lizt)
	print(num)
	lizt.remove(num)

lizt = []
for number in range(1, 21):
	lizt.append(number)

root = Tk()
button = Button(root, text = 'randomize', command = randomize)
button.pack()

root.mainloop()