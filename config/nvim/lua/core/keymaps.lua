-- Exit insert mode with jj
vim.keymap.set("i", "jj", "<esc>")

-- Center search matches on screen
vim.keymap.set("n", "n", "nzzzv")
vim.keymap.set("n", "N", "Nzzzv")
vim.keymap.set("c", "<cr>", function()
  return vim.fn.getcmdtype() == "/" and "<cr>zzzv" or "<cr>"
end, { expr = true })

-- Shorter window movement using <ctrl> and arrow keys
vim.keymap.set("n", "<c-left>", "<c-w><left>", { desc = "go to left window", noremap = true })
vim.keymap.set("n", "<c-right>", "<c-w><right>", { desc = "go to right window", noremap = true })
vim.keymap.set("n", "<c-up>", "<c-w><up>", { desc = "go to upper window", noremap = true })
vim.keymap.set("n", "<c-down>", "<c-w><down>", { desc = "go to lower window", noremap = true })

-- Delete buffer quickly
vim.keymap.set("n", "<leader>q", "<cmd>:bwipeout<cr>", { noremap = true, desc = "delete buffer" })
