local lang_from_ext = {
  r = "r", R = "r", Rprofile = "r",
  py = "python", python = "python",
  lua = "lua",
  js = "javascript", ts = "typescript",
  sh = "bash", bash = "bash",
  qmd = "markdown", md = "markdown",
  yml = "yaml", yaml = "yaml",
  sql = "sql",
  css = "css", scss = "scss",
  typ = "typst",
}

local function kw(kwargs, name, default)
  local v = kwargs[name]
  if v == nil then return default end
  local s = pandoc.utils.stringify(v)
  if s == "" then return default end
  return s
end

local function parse_lines(spec)
  if not spec then return nil, nil end
  local s, e = spec:match("^(%d*)%-(%d*)$")
  if s == "" then s = nil end
  if e == "" then e = nil end
  if not s and not e and spec:match("^%d+$") then
    s = spec
  end
  return s, e
end

local function dedent(line, n)
  return line:sub(1, n):gsub(" ", "") .. line:sub(n + 1)
end

local function read_file(filepath, start_line, end_line, dedent_n)
  local fh = io.open(filepath)
  if not fh then
    io.stderr:write("script shortcode: cannot open " .. filepath .. "\n")
    return nil
  end
  local content = ""
  local n = 1
  local start = start_line and tonumber(start_line) or 1
  local stop = end_line and tonumber(end_line) or nil
  for line in fh:lines("L") do
    if dedent_n then line = dedent(line, dedent_n) end
    if n >= start and (not stop or n <= stop) then
      content = content .. line
    end
    n = n + 1
  end
  fh:close()
  return content
end

local js_registered = false
local function ensure_js()
  if js_registered then return end
  quarto.doc.add_html_dependency({
    name = "add-code-files",
    version = "1.0.0",
    scripts = { { path = "add-code-files.js", afterBody = true } },
  })
  js_registered = true
end

return {
  ["script"] = function(args, kwargs)
    if #args < 1 then
      error("script shortcode: a path is required, e.g. {{< script path/to/file.R >}}")
    end
    local path = pandoc.utils.stringify(args[1])

    local ext          = (path:match("%.([^.]+)$") or "")
    local default_lang = lang_from_ext[ext] or lang_from_ext[ext:lower()] or ""

    local lang     = kw(kwargs, "lang", default_lang)
    local filename = kw(kwargs, "filename", path)
    local numbers  = kw(kwargs, "numbers", "true")
    local dedent_s = kw(kwargs, "dedent", nil)
    local suffix   = kw(kwargs, "suffix", nil)
    local s, e     = parse_lines(kw(kwargs, "lines", nil))

    if suffix then filename = filename .. " " .. suffix end

    local content = read_file(path, s, e, dedent_s and tonumber(dedent_s) or nil)
    if content == nil then
      return pandoc.Div({ pandoc.Para({ pandoc.Str("[script: file not found: " .. path .. "]") }) })
    end

    ensure_js()

    local classes = { "cell-code" }
    if lang ~= "" then table.insert(classes, 1, lang) end
    if numbers == "true" then table.insert(classes, "number-lines") end
    table.insert(classes, "cw-auto")

    local cb_attrs = {}
    if s then cb_attrs.startFrom = s end
    if lang ~= "" then cb_attrs.filename = lang end

    local code = pandoc.CodeBlock(content, pandoc.Attr("", classes, cb_attrs))

    return pandoc.Div(
      { code },
      pandoc.Attr("", {}, { ["code-filename"] = filename })
    )
  end,
}
