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


def line_prepender(filename, line):
    with open(filename, 'r+') as f:
        content = f.read()
        f.seek(0, 0)
        f.write(line.rstrip('\r\n') + '\n\n' + content)


  