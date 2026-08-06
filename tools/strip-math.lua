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
  -- This strips the thin math gaps inside numbers.
  plain_text = plain_text:gsub("\u{2006}", "")
  -- 6. Return it as a normal plain markdown text string
  return pandoc.Str(plain_text)
end
