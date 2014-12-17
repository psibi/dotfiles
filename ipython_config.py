# Location: ~/.ipython/profile_default/ipython_config.py

c = get_config()

c.InteractiveShellApp.extensions = ['autoreload']
c.InteractiveShellApp.exec_lines = ['%autoreload 2']
