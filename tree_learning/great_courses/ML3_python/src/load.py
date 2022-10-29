def load ():
    import os
    cwd = os.getcwd()
    print ('cwd: ', cwd)
    os.chdir(os.path.join (cwd, 'src'))
    cwd = os.getcwd()
    print ('cwd: ', os.getcwd())
    os.listdir(os.getcwd())
    import Tree2
    cwd = os.path.join (cwd, 'Tree2')
    os.chdir(cwd)
    os.listdir(os.getcwd())
    import base
    import classes
