-- strip-math.lua
function Math(elem)
  -- 1. Wrap the raw TeX code back into an inline math block string
  local math_string = "$" .. elem.text .. "$"
  -- 2. Read it as markdown so Pandoc initializes its texmath engine
  local doc = pandoc.read(math_string, "markdown")
  -- 3. Write it out specifically to plain text format to force Unicode rendering
  local plain_text = pandoc.write(doc, "plain")
  -- 4. Strip the trailing newline that the plain text writer automatically appends
  plain_text = plain_text:gsub("%s+$", "")
  -- 5. Explicitly remove narrow spaces
  -- This strips the thin math gaps inside numbers and converts special whitespaces into regular ones.
  plain_text = plain_text:gsub("\u{00a0}", " ")
  plain_text = plain_text:gsub("\u{2001}", " ")
  plain_text = plain_text:gsub("\u{2004}", " ")
  plain_text = plain_text:gsub("\u{2005}", " ")
  plain_text = plain_text:gsub("\u{2006}", "")
  plain_text = plain_text:gsub("\u{200a}", "")
  -- 6. Return it as a normal plain markdown text string
  return pandoc.Str(plain_text)
end
