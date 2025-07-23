def movefile(old, new):
    import shutil
    shutil.move(old, new)

def make_directory(path):
    import os
    try:
        os.mkdir(path)
        return True
    except FileExistsError:
        return True
        