import re
import gdb

def lookup_function (val):
    "Look-up and return a pretty-printer that can print val."
    # Get the type.
    type = val.type

    # If it points to a reference, get the reference.
    if type.code == gdb.TYPE_CODE_REF:
        type = type.target ()

    # Get the unqualified type, stripped of typedefs.
    type = type.unqualified ().strip_typedefs ()

    # Get the type name.    
    typename = type.tag

    if typename == None:
        return None

    # Iterate over local dictionary of types to determine
    # if a printer is registered for that type.  Return an
    # instantiation of the printer if found.
    for function in sorted(pretty_printers_dict):
       if function.match (typename):
           return pretty_printers_dict[function] (val)

    # Cannot find a pretty printer.  Return None.
    return None


class GenericPrinter:
    def __init__(self, val):
        self.val = val

    def to_string(self):
        return "Generic object with the following members:"

    def children(self):
        for k, v in process_kids(self.val, self.val):
            for k2, v2 in printer(k, v): yield k2, v2


def process_kids(state, PF):
    for field in PF.type.fields():
        if field.artificial or field.type == gdb.TYPE_CODE_FUNC or \
        field.type == gdb.TYPE_CODE_VOID or field.type == gdb.TYPE_CODE_METHOD or \
        field.type == gdb.TYPE_CODE_METHODPTR or field.type == None: continue
        key = field.name
        if key is None: continue
        try: state[key]
        except RuntimeError: continue
        val = PF[key]
        if field.is_base_class and len(field.type.fields()) != 0:
            for k, v in process_kids(state, field):
                yield key + " :: " + k, v
        else:
            yield key, val


def printer(key, val):
    if val.type.code == gdb.TYPE_CODE_PTR or val.type.code == gdb.TYPE_CODE_MEMBERPTR:
        if not val: yield key, "NULL"
        else:
            try:
                str(val.dereference())
                yield key, val.dereference()
            except RuntimeError: 
                yield key, "Cannot access memory at address " + str(val.address)
    elif val.type.code == gdb.TYPE_CODE_INT:
        yield key, int(val)
    elif val.type.code == gdb.TYPE_CODE_FLT or val.type.code == gdb.TYPE_CODE_DECFLOAT:
        yield key, float(val)
    elif val.type.code == gdb.TYPE_CODE_STRING or val.type.code == gdb.TYPE_CODE_ARRAY:
        yield key, str(val)
    else: yield key, val

# register the pretty-printer
pretty_printers_dict = {}
pretty_printers_dict[re.compile ('.*Generic.*')] = GenericPrinter
gdb.pretty_printers.append(lookup_function)

