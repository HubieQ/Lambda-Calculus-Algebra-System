import os

# prints all files within a directory containing a string
def search_file(directory_path, special_string):

    file_list = []
    for root, dir, files in os.walk(directory_path):
        for file_name in files:
            if special_string in file_name:
                 file_list.append(os.path.join(root, file_name))
    return file_list
