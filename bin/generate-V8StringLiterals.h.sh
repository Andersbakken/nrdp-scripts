#!/bin/bash

if [ ! -d "$1" ] || [ ! -d "$2" ]; then
    echo "$0 [path-to-src-dir] [path-to-jsc-builddir]" >&2
    exit 1
fi

echo "/* (c) 2020 Netflix, Inc. Do not copy or use without prior written permission from Netflix, Inc. */"
echo ""
echo "#ifndef SCRIPT_V8_V8STRINGLITERALS_H"
echo "#define SCRIPT_V8_V8STRINGLITERALS_H"
echo ""
echo "#include <v8.h>"
echo ""
echo "#define V8_STRING_LITERALS(V) \\"

TMPFILE=$(mktemp)
find "$1" "$2" -name "*.cpp" | xargs grep NRDP_SCRIPT_IDENTIFIER | sed -e 's,^.*(\([^)]*\).*$,    V(\1) \\,' | sort -u > $TMPFILE
BYTES=$(wc -c "$TMPFILE" | cut -d" " -f 1)
head -c$(expr $BYTES - 2) "$TMPFILE"
rm "$TMPFILE"

echo
echo "namespace netflix {"
echo "namespace script {"
echo "class V8StringLiterals"
echo "{"
echo "public:"
echo "    explicit V8StringLiterals(v8::Isolate* isolate)"
echo "        : mIsolate(isolate) {}"
echo ""
echo "#define GET_LITERAL(Name) \\"
echo "    v8::Local<v8::String> Get_##Name() \\"
echo "    { \\"
echo "        if (m_##Name.IsEmpty()) \\"
echo "            m_##Name.Set(mIsolate, v8::String::NewFromUtf8Literal(mIsolate, #Name)); \\"
echo "        return m_##Name.Get(mIsolate); \\"
echo "    }"
echo "    V8_STRING_LITERALS(GET_LITERAL)"
echo "#undef GET_LITERAL"
echo ""
echo "private:"
echo "    v8::Isolate* mIsolate;"
echo "#define DEFINE_LITERAL(Name) \\"
echo "    v8::Eternal<v8::String> m_##Name;"
echo "    V8_STRING_LITERALS(DEFINE_LITERAL)"
echo "#undef DEFINE_LITERAL"
echo "};"
echo "}  // namespace script"
echo "}  // namespace netflix"
echo ""
echo "#undef V8_STRING_LITERALS"
echo "#endif  // SCRIPT_V8_V8STRINGLITERALS_H"
