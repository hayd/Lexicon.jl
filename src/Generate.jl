"""
    Generate
"""
module Generate

using Base.Meta
using LightGraphs

import Docile

export makedocs

function makedocs(pkg; docs_dir="docs", gen_dir="_generated")
    # Firstly have to load in pkg
    eval(Expr(:toplevel, Expr(:using, symbol(pkg))))

    pddir = joinpath(Pkg.dir(string(pkg)), docs_dir)
    md_files = filter(file -> ismatch(r"\.(md|markdown)", file), readdir(pddir))
    makedocs(pkg, md_files...; docs_dir=docs_dir, gen_dir=gen_dir)
end
function makedocs(pkg, fns...; docs_dir="docs", gen_dir="_generated")
    pddir = joinpath(Pkg.dir(string(pkg)), docs_dir)
    for fn in fns
        loadfile(joinpath(pddir, fn), joinpath(pddir, gen_dir, fn))
    end
end

"""Expand braces of the markdown file `path` and save to the `out`."""
function loadfile(path :: AbstractString, out :: AbstractString)
    storage = task_local_storage()
    prev, path = getpath(path)
    storage[:SOURCE_PATH] = path
    try
        include_string(template(path, out))
    finally
        prev == nothing ? delete!(storage, :SOURCE_PATH) : (storage[:SOURCE_PATH] = prev)
    end
end

function getpath(p)
    prev = Base.source_path(nothing)
    path = prev == nothing ? abspath(p) : joinpath(dirname(prev), p)
    prev, path
end

function template(path, out)
    """
    Lexicon.Generate.@file "$(out)",
    \"\"\"
    $(escape(readall(path)))
    \"\"\"
    """
end

escape(text) = replace(text, "\"\"\"", "\\\"\\\"\\\"")

macro file(args...) atfile(args...) end

function atfile(out, text)
    quote
        open(path(@__FILE__(), $(out)), "w") do io
            map(writer(io), $(esc(build(text))))
        end
        nothing
    end
end
atfile(x) = isexpr(x, :tuple) ? atfile(x.args...) : error("Invalid syntax.")

path(file, out) = joinpath(dirname(file), out)

writer(io :: IO)                   = x -> writer(io, x)
writer(io :: IO, x)                = print(io, x)
writer(io :: IO, x :: Markdown.MD) = writemime(io, "text/plain", x) # text/markdown
writer(io :: IO, x :: Vector)      = for each in x writer(io, each) end

function build(expr :: Expr)
    isexpr(expr, :string) || return expr
    out = Expr(:vcat)
    for each in expr.args
        push!(out.args, build(each))
    end
    out
end

function build(text :: AbstractString)
    out = Expr(:vcat)
    for (n, part) in enumerate(split(text, r"{{|}}"))
        push!(out.args, isodd(n) ? part : parsebracket(part))
    end
   out
end

immutable Directive{name} end

macro directive_str(text) :(Directive{$(quot(symbol(text)))}) end

const DIRECTIVE_MARKER = r"^(\w+)(:|\s*\n)((?s).*)$"

const default = Ref{Directive}(directive"docs"())

function withdefault(f :: Function, dir :: Symbol)
    original = default.x
    default.x = Directive{dir}()
    f()
    default.x = original
    nothing
end

function getdirective(text)
    m = match(DIRECTIVE_MARKER, text)
    (m == nothing) && return default.x, text
    Directive{symbol(m[1])}(), m[3]
end

parsebracket(text :: AbstractString) = parsebracket(getdirective(text)...)
parsebracket{D}(dir :: Directive{D}, text) = error("Unknown directive: \"$D\"")

function parsebracket(:: directive"docs", text)
    out = Expr(:vcat)
    for each in split(text, "\n")
        # isempty(begin s = strip(each) end) || push!(out.args, :(@doc($(parse(s)))))
        isempty(begin s = strip(each) end) || push!(out.args, :($(do_docs(parse(s)))))
    end
    out
end

function parsebracket(:: directive"repl", text)
    out = Expr(:vcat)
    push!(out.args, :("```\n"))
    for (i, each) in enumerate(split(text, "\n"))
        isempty(begin s = strip(each) end) || push!(out.args, :($(do_repl(s))))
    end
    push!(out.args, :("```\n"))
    out
end

function do_repl(code)
    mat = match(r"^\s*>(!?) (.+)$", code)
    hidden = mat[1] == "!"
    code = mat[2]
    out = IOBuffer()
    hidden || println(out, "julia> ", code)
    old_stderr, old_stdout = STDERR, STDOUT
    #hidden ? redirect_stderr() : redirect_stderr(out)
    #hidden ? redirect_stdout() : redirect_stdout(out)
    #hidden && (redirect_stderr(), redirect_stdout())
    try
        res = eval(parse(code))
        hidden || ismatch(r";\s*$", code) || println(out, res)
    catch e
        # Note: this is printed even if hidden is true.
        showerror(out, e)
        println(out)
    #finally
    #    redirect_stderr(old_stderr)
    #    redirect_stdout(old_stdout)
    end
    takebuf_string(out)
end

function do_docs(f::Expr)
    do_docs(eval(f))
end
function do_docs(f::Union{Function, Module})
    out = IOBuffer()
    println(typeof(f))
    docstring = local_doc(f; from=LightGraphs)
    if isa(f, Function)
        println(out, "### ", first(methods(f)).func.code.name)
        println(out)
    end
    if has_h1_to_h3(docstring)
        println(STDOUT, "WARN: docstring for ", f, " contains h1 - h3. ",
              "This may confuse formatting.")
    end
    writemime(out, "text/plain", docstring)
    println(out)
    if isa(f, Function)
        md_methodtable(out, f, LightGraphs)
    end
    takebuf_string(out)
end

"""Collect the signatures and source-links to the methods of f defined in m."""
function md_methodtable(io, f, m::Module)
    println(io, "```")
    # We only consider methods with are defined in the parent (project) directory
    pd = joinpath(Pkg.dir(), string(module_name(m)))
    meths = filter(x -> startswith(string(x.func.code.file), pd), methods(f))
    for (i, meth) in enumerate(meths)
        md_method(io, meth, i, m)
    end
    println(io, "```")
    print(io, "*Source:")
    for (i, meth) in enumerate(meths)
        print(io, " [", i, "](", method_link(meth, m), ")")
    end
    println(io, "*.")
    println(io)
end

"""Print the signature of the method meth.

If meth is not defined in `m` then nothing will be printed.
"""
function md_method(io, meth, i, m::Module)
    # We only print methods with are defined in the parent (project) directory
    pd = joinpath(Pkg.dir(), string(module_name(m)))
    if !(startswith(string(meth.func.code.file), pd))
        return
    end
    print(io, i, "  ",  meth.func.code.name)
    tv, decls, file, line = Base.arg_decl_parts(meth)
    if !isempty(tv)
        Base.show_delim_array(io, tv, '{', ',', '}', false)
    end
    print(io, "(")
    print_joined(io, [isempty(d[2]) ? "$(d[1])" : "$(d[1])::$(d[2])" for d in decls],
                 ", ", ", ")
    print(io, ")")
    println(io)
end


# The following is for extracting the local documenation

function local_doc(func::Symbol; from = Main, include_submodules = true)
    local_doc(from.(func); from=from, include_submodules=include_submodules)
end
function local_doc(func::Expr; from = Main, include_submodules = true)
  local_doc(eval(func); from=from, include_submodules=include_submodules)
end
function local_doc(m::Module; from = Main, include_submodules=include_submodules)
  return from.__META__[m]
end
function local_doc(func::Function; from = Main, include_submodules = true)
  if isa(func, Module)
      # TODO work with submodules
      return from.__META__[func]
  end
  out = IOBuffer()
  for m in (include_submodules ? submodules(from) : Set([from]))
      if isdefined(m, :__META__)
          meta = m.__META__
          if haskey(meta, func)
              if meta[func].main != nothing
                  writemime(out, "text/plain", meta[func].main)
                  println(out)
                  println(out)
              end
              for each in meta[func].order
                  writemime(out, "text/plain", Base.Docs.doc(func, each))
                  # two lines may be required to end the block.
                  # TODO I think this is printing too many blank lines
                  # since often the writemime above does nothing.
                  println(out)
                  println(out)
              end
          end
      end
  end
  plain_docstring = takebuf_string(out)
  if plain_docstring == ""
      println("WARN: No docstring found for ", func)
  end
  return Markdown.parse(plain_docstring)

end

"""Returns the Set of submodules of module `m`."""
function submodules(m::Module)
 out = Set([m])
 for name in names(m, true)
     if isdefined(m, name)
         object = getfield(m, name)
         validmodule(m, object) && union!(out, submodules(object))
     end
 end
 out
end

validmodule(m::Module, object::Module) = object ≠ m && object ≠ Main
validmodule(::Module, other) = false  


# Check markdown formatting


"""Warn if a docstring has a h1-h3. These can mess up formatting which
makes assumptions on the layout of the documentation based on the levels
of the headers.

h4s should be preferred to denote sections in docstrings.
"""
function has_h1_to_h3(md::Markdown.MD)
    for i in 1:length(md)
        s = md[i]
        if isa(s, Markdown.Header) && typeof(s).parameters[1] <= 3
            return true
        end
    end
    return false
end


# Utils

"""Return github link to the line-number where the method is defined.
Note: This references the current commit sha.
"""
function method_link(meth::Method, m::Module)
    u, commit, root = module_url(meth, m)
    file = relpath(string(meth.func.code.file), root)
    line = meth.func.code.line

    return "https://github.com/$u/tree/$commit/$file#L$line"
end

const _URL_CACHE = Dict{Module, Any}()
"""Return the github account/project, the current commit, and the project
root; as a tuple.
"""
function module_url(meth::Method, m::Module)
    found = get(_URL_CACHE, m, nothing)
    if found != nothing
        return found
    end
    d = dirname(string(meth.func.code.file))
    u = Pkg.Git.readchomp(`config remote.origin.url`, dir=d)
    u = match(Pkg.Git.GITHUB_REGEX,u).captures[1]

    root = cd(d) do # dir=d confuses --show-toplevel, apparently
        Pkg.Git.readchomp(`rev-parse --show-toplevel`)
    end
    root = @windows? replace(root, "/", "\\") : root

    commit = Pkg.Git.readchomp(`rev-parse HEAD`, dir=root)
    return _URL_CACHE[m] = (u, commit, root)
end


end
