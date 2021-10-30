genNativeTypeCollector =
function(filename= NA) {
    cursors = integer()
    files = character()
    names = character()
    lines = character()        
    update = function(cur, parent) {
        kind = getCursorKind(cur)

        # Detect an Unexposed element that is not extern "C" and that is in the target file. 
        # e.g., MAKE_R_eraseFromParent(BasicBlock) in Rllvm/src/Block.cpp
        # The last check is so that we don't do this for elements in header files.
        if(!is.na(filename) && kind == CXCursor_UnexposedDecl && !isExternC(cur) && getFileName(cur) == filename) 
            return(update(children(cur)[[1]]))

        
        if(kind == CXCursor_UnexposedDecl && isExternC(cur)) 
            return(update(children(cur)[[1]]))

        cursors[[ length(cursors) + 1L]] <<- kind
        files[[ length(files) + 1L]] <<- getFileName(cur)
        names[[ length(names) + 1L]] <<- getName(cur)
        lines[[ length(lines) + 1L]] <<- getLocation(cur)$location["line"]
        
        #if(kind %in% c(8, 9)) browser()
        CXChildVisit_Continue
    }

    list(update = update, result = function() data.frame(type = mapCursorTypes(cursors), file = files, name = names, lineNumber = lines, kind = cursors))
}

isExternC =
function(cur)
{
    toks = getCursorTokens(cur)
    length(toks) > 2 && all(toks[1:2] == c("extern", '"C"'))
}

mapCursorTypes =
function(vals)
    names(CXCursorKind)[match(vals, CXCursorKind)]


getToplevelNativeInfo =
function(x, ..., col = genNativeTypeCollector(getFileName(x)))
{
    if(is.character(x) && file.exists(x))
        x = createTU(x, ...)
    
    visitTU(x, col$update)
    d = col$result()
    d[ d$file == getFileName(x), ]
}

if(FALSE) {
    xml.includes = includes = c("/usr/local/include/libxml2", "~/R/R-new/build3/include", "/Users/duncan/LLVM/clang+llvm-13.0.0-x86_64-apple-darwin/lib/clang/13.0.0/include")
    xml.args = c("-DUSER_OBJECT_=SEXP", "-DLIBXML", "-DUSE_EXTERNAL_SUBSET=1", "-DROOT_HAS_DTD_NODE=1", "-DDUMP_WITH_ENCODING=1", "-DUSE_XML_VERSION_H=1", "-DXML_ELEMENT_ETYPE=1", "-DXML_ATTRIBUTE_ATYPE=1", "-DNO_XML_HASH_SCANNER_RETURN=1", "-DLIBXML_NAMESPACE_HAS_CONTEXT=1", "-DHAVE_R_CETYPE_T=1", "-DHAVE_XML_WITH_ZLIB=1", "-DHAVE_XML_HAS_FEATURE=1", "-DUSE_R=1", "-D_R_=1", "-DHAVE_VALIDITY=1", "-DXML_REF_COUNT_NODES=1", "-DLIBXML2=1")        
    tu = createTU("~/GitWorkingArea/XML/src/DocParse.c", includes = xml.includes, args = xml.args)

    cfiles = xpdFileNames("~/GitWorkingArea/XML/src", pattern = "\\.c")
    nativeInfo = lapply(cfiles, getToplevelNativeInfo, includes = xml.includes, args = xml.args)
}

if(FALSE)
{
    # doesn't find 'new' - file not found
    Rllvm.includes = "~/LLVM/clang+llvm-11.0.0-x86_64-apple-darwin/include"
    Rllvm.args = c("-xc++", "-D__STDC_CONSTANT_MACROS", "-D__STDC_FORMAT_MACROS", "-D__STDC_LIMIT_MACROS", "-DLLVM_VERSION=11", "-DLLVM_MINOR_VERSION=0", "-DNEW_LLVM_ATTRIBUTES_SETUP=1", "-DLLVM_DATALAYOUT_H_IN_IR=1")
    tu = createTU("~/GitWorkingArea/Rllvm/src/Module.cpp", includes = Rllvm.includes, args = Rllvm.args)

    z = getToplevelNativeInfo("~/GitWorkingArea/Rllvm/src/Module.cpp", includes = Rllvm.includes, args = Rllvm.args)

    files = xpdFileNames("~/GitWorkingArea/Rllvm/src", pattern = "\\.(cpp|C|c)$")
    z2 = lapply(files, getToplevelNativeInfo, includes = Rllvm.includes, args = Rllvm.args)
    names(z2) = files
    # gets a 42x3 data.frame  But the types and names are mostly UnexposedDecl and ""
    # These are extern "C" declarations.

    z3 = do.call(rbind, z2)

    tu2 = createTU("~/GitWorkingArea/Rllvm/src/Block.cpp", includes = Rllvm.includes, args = Rllvm.args)
    k = children(getTranslationUnitCursor(tu2))
    ki = sapply(k, getCursorKind)
}
