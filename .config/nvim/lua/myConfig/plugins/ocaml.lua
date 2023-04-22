-- ## added by OPAM user-setup for vim / base ## 93ee63e278bdfc07d1139a748ed3fff2 ## you can edit, but keep this line

local opam_share_dir = vim.fn.system("opam config var share")
opam_share_dir = opam_share_dir:gsub('[\r\n]*$', '')

local opam_configuration = {}

local function OpamConfOcpIndent()
  vim.cmd("set rtp^=" .. opam_share_dir .. "/ocp-indent/vim")
end
opam_configuration['ocp-indent'] = OpamConfOcpIndent

local function OpamConfOcpIndex()
  vim.cmd("set rtp+=" .. opam_share_dir .. "/ocp-index/vim")
end
opam_configuration['ocp-index'] = OpamConfOcpIndex

local function OpamConfMerlin()
  local dir = opam_share_dir .. "/merlin/vim"
  vim.cmd("set rtp+=" .. dir)
end
opam_configuration['merlin'] = OpamConfMerlin

local opam_packages = { "ocp-indent", "ocp-index", "merlin" }
local opam_check_cmdline = { "opam list --installed --short --safe --color=never" }
for _, package in ipairs(opam_packages) do
  table.insert(opam_check_cmdline, package)
end

local opam_available_tools = vim.split(vim.fn.system(table.concat(opam_check_cmdline, " ")), "\n")
for _, tool in ipairs(opam_packages) do
  -- Respect package order (merlin should be after ocp-index)
  if vim.fn.index(opam_available_tools, tool) >= 0 then
    opam_configuration[tool]()
  end
end

-- ## end of OPAM user-setup addition for vim / base ## keep this line
-- ## added by OPAM user-setup for vim / ocp-indent ## 4544d20c51c96cb14f626683d47680f6 ## you can edit, but keep this line
if vim.fn.index(opam_available_tools, "ocp-indent") < 0 then
  vim.cmd("source /home/tibor/.opam/5.0.0/share/ocp-indent/vim/indent/ocaml.vim")
end
-- ## end of OPAM user-setup addition for vim / ocp-indent ## keep this line

