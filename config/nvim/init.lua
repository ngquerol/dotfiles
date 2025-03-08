require("core.options")
require("core.keymaps")
require("core.autocmds")
require("core.ui")
require("core.lazy")

if vim.g.neovide then
  require("core.neovide")
end
