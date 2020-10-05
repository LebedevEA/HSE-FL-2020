import os

directory = 'tests/'

for filename in os.listdir(directory):
    if not filename.endswith(".out"):
        os.system("python3 src/parse.py " + directory + filename)
