s%IMPORTLIST%[ varDecl(GOROUTINE_LIBNAME, Call(Identifier("require"), [String("\\"\\"../js/goroutine.js\\"\\"")])), varDecl(ARRAYLIB_NAME, Call(Identifier("require"), [String("\\"\\"../js/go-array.js\\"\\"")])), varDecl(POINTERLIB_NAME, Call(Identifier("require"), [String("\\"\\"../js/go-pointer.js\\"\\"")])), varDecl(STRUCTLIB_NAME, Call(Identifier("require"), [String("\\"\\"../js/go-struct.js\\"\\"")])) ]%g
s%GOROUTINE_LIBNAME%"routine"%g
s%GOROURTINE_GOMETHOD%"go"%g
s%GOROUTINE_MAKECHANNELMETHOD%"newChannel"%g

s%GODEFER_NEWDEFERMETHOD%"newDeferList"%g
s%GODEFER_ADDPARAMMETHOD%"addParam"%g
s%GODEFER_ADDDEFERMETHOD%"add"%g
s%GODEFER_EXECECUTEMETHOD%"cleanUp"%g
s%GODEFER_SETRETMETHOD%"setCallback"%g
s%GODEFER_RETURNMETHOD%"ret"%g

s%ARRAYLIB_NAME%"__GO_ARRAYLIB__"%g
s%ARRAYLIB_NEWARRAYMETHOD%"newArray"%g
s%ARRAYLIB_LOOKUPMETHOD%"getVal"%g
s%ARRAYLIB_CREATESLICEMETHOD%"getSlice"%g
s%ARRAYLIB_GETSTARTPOSTMETHOD%"getStartPos"%g

s%POINTERLIB_NAME%"__GO_POINTERLIB__"%g
s%POINTERLIB_NEWPOINTER%"newPointer"%g
s%POINTERLIB_GET%"get"%g
s%POINTERLIB_SET%"set"%g

s%STRUCTLIB_NAME%"__GO_STRUCTLIB__"%g
s%STRUCTLIB_DEFSTRUCTMETHOD%"defStruct"%g
s%STRUCTLIB_GETNEWMETHOD%"getNew"%g