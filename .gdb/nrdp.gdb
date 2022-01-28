# Load libgibbon symbols automatically
python
import os

def new_objfile_handler(event):
    filename = event.new_objfile.filename
    if os.path.basename(filename) == "netflix":
        symbols = os.path.dirname(filename) + "/libgibbon.so"
        if os.path.exists(symbols):
            gdb.execute("add-symbol-file " + symbols)

gdb.events.new_objfile.connect(new_objfile_handler)
end
