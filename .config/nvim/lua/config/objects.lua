O = {}

O.ViModes = {
  ["n"]     = "normal",
  ["no"]    = "normal",
  ["nov"]   = "normal",
  ["noV"]   = "normal",
  ["no\22"] = "normal",
  ["niI"]   = "normal",
  ["niR"]   = "normal",
  ["niV"]   = "normal",
  ["nt"]    = "normal",
  ["ntT"]   = "normal",
  ["v"]     = "visual",
  ["vs"]    = "visual",
  ["V"]     = "visual",
  ["Vs"]    = "visual",
  ["\22"]   = "visual",
  ["\22s"]  = "visual",
  ["s"]     = "select",
  ["S"]     = "select",
  ["\19"]   = "insert",
  ["i"]     = "insert",
  ["ic"]    = "insert",
  ["ix"]    = "insert",
  ["R"]     = "replace",
  ["Rc"]    = "replace",
  ["Rx"]    = "replace",
  ["Rv"]    = "replace",
  ["Rvc"]   = "replace",
  ["Rvx"]   = "replace",
  ["c"]     = "command",
  ["cv"]    = "command",
  ["ce"]    = "command",
  ["r"]     = "...",
  ["rm"]    = "M",
  ["r?"]    = "?",
  ["!"]     = "󰩌",
  ["t"]     = "terminal",
}

O.Icons = {
  mode = "",
  git_branch = " ",
  error = " ",
  warn = " ",
  info = " ",
  hint = " ",
  added = " ",
  modified = "󰝤 ",
  removed = " "
}

return O
