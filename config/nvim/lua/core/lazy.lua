local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end

vim.opt.rtp:prepend(lazypath)

local ok, lazy = pcall(require, "lazy")
if not ok then
  return
end

vim.keymap.set("n", "<leader>L", lazy.show, { noremap = true })

-- TODO
-- ○ nvim-surround
-- ○ other.nvim
-- ○ todo-comments.nvim
-- ○ vim-illuminate

lazy.setup(
  "plugins", {
    defaults = { version = "*" },
    ui = {
      border = "rounded",
      icons = {
        cmd = "▶️",
        config = "🛠",
        event = "📅",
        ft = "📂",
        init = "⚙️",
        keys = "⌨️",
        plugin = "🔌",
        runtime = "💻",
        source = "📄",
        start = "🚀",
        task = "📌",
        lazy = "💤 ",
      },
    },
  })
