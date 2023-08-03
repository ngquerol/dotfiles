vim.g.mapleader = " "
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

vim.opt.number = true
vim.opt.showmatch = true
vim.opt.splitright = true
vim.opt.splitbelow = true
vim.opt.autowrite = true
vim.opt.autochdir = true

vim.opt.mouse = "a"
vim.opt.clipboard = "unnamedplus"
vim.opt.swapfile = false
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.completeopt = "menu,menuone,noselect"

vim.opt.undofile = true
vim.opt.undodir = vim.fn.stdpath("data") .. "undo"

vim.opt.expandtab = true
vim.opt.shiftwidth = 4
vim.opt.tabstop = 4
vim.opt.autoindent = true
vim.opt.wrap = true
vim.opt.formatoptions:append("tcrqnj")
vim.opt.joinspaces = false

vim.opt.signcolumn = "yes"
vim.opt.cursorline = false
vim.opt.termguicolors = true
vim.opt.signcolumn = "yes"
vim.opt.laststatus = 3
vim.opt.showmode = false
vim.opt.shortmess:append("TISsq")

vim.diagnostic.config({
  severity_sort = true,
  underline = false,
  virtual_text = {
    spacing = 4,
    source = "if_many",
    prefix = " ●",
    suffix = " "
  },
  float = {
    border = "rounded",
    source = "always",
  },
})

for type, icon in pairs({ Error = "🚫", Warn = "⚠️ ", Info = "ℹ️ ", Hint = "💡" }) do
  local hl = "DiagnosticSign" .. type
  vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
end

vim.o.guifont = "Iosevka:h13"
vim.g.neovide_refresh_rate = 60
