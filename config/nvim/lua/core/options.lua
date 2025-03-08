vim.g.mapleader = vim.keycode(" ")

vim.opt.autochdir = true
vim.opt.autowrite = true
vim.opt.number = true
vim.opt.showmatch = true
vim.opt.splitbelow = true
vim.opt.splitright = true

vim.opt.clipboard = vim.env.SSH_TTY and "" or "unnamedplus"
vim.opt.completeopt = "menu,menuone,noselect"
vim.opt.ignorecase = true
vim.opt.mouse = "a"
vim.opt.smartcase = true
vim.opt.smartindent = true
vim.opt.swapfile = false
vim.opt.wildmode = "longest:full,full"

vim.opt.diffopt:append("vertical,algorithm:histogram,indent-heuristic,hiddenoff,linematch:60")
vim.opt.undofile = true
vim.opt.undolevels = 10000

vim.opt.autoindent = true
vim.opt.expandtab = true
vim.opt.formatoptions:append("tcrqnj")
vim.opt.joinspaces = false
vim.opt.shiftwidth = 4
vim.opt.tabstop = 4
vim.opt.wrap = false

vim.opt.foldcolumn = "0"
vim.opt.foldenable = true
vim.opt.foldlevelstart = 99
vim.opt.foldnestmax = 10
vim.opt.foldtext = ""

vim.opt.cmdheight = 0
vim.opt.cursorline = true
vim.opt.laststatus = 3
vim.opt.scrolloff = 4
vim.opt.shortmess:append("TISsq")
vim.opt.showmode = false
vim.opt.sidescrolloff = 8
vim.opt.signcolumn = "yes"
vim.opt.termguicolors = true

vim.opt.timeout = true
vim.opt.timeoutlen = 300

vim.opt.fillchars = {
  diff = "╱",
  fold = " ",
  foldsep = " ",
  eob = " ",
}
vim.opt.list = true
vim.opt.listchars = {
  tab = "⇥ ",
  trail = "·",
  nbsp = "⎵",
  extends = "⋯",
  precedes = "⋯",
}
