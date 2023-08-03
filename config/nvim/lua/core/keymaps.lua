vim.keymap.set("n", "<c-left>", "<c-w><left>", { noremap = true })
vim.keymap.set("n", "<c-right>", "<c-w><right>", { noremap = true })
vim.keymap.set("n", "<c-up>", "<c-w><up>", { noremap = true })
vim.keymap.set("n", "<c-down>", "<c-w><down>", { noremap = true })

vim.keymap.set("n", "<leader>e", "<cmd>:exe 'edit '.stdpath('config').'/init.lua'<cr>", { noremap = true })

vim.keymap.set("n", "<leader>i", vim.diagnostic.open_float, { noremap = true })
