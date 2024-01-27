return {
	"akinsho/toggleterm.nvim",
	keys = {
		{ "<leader>tf", "<cmd>ToggleTerm direction=float<cr>", desc = "Open terminal (floating)" },
		{ "<leader>tv", "<cmd>ToggleTerm direction=vertical<cr>", desc = "Open terminal (vertical)" },
		{ "<leader>ts", "<cmd>ToggleTerm direction=horizontal<cr>", desc = "Open terminal (horizontal)" },
		{ "<leader>tt", "<cmd>TermSelect<cr>", desc = "Select terminal" },
		{
			"<leader>tg",
			function()
				if vim.fn.executable("lazygit") ~= 1 then
					vim.api.nvim_feedkeys("<leader>tg", "n", true)
				else
					local Terminal = require("toggleterm.terminal").Terminal
					local lazygit = Terminal:new({ cmd = "lazygit", silent = true })
					lazygit:toggle()
				end
			end,
			desc = "Open lazygit (floating)",
		},
	},
	opts = {
		close_on_exit = true,
		direction = "float",
		float_opts = { border = "rounded" },
		hide_numbers = true,
		persist_size = true,
		shade_terminals = true,
		shading_factor = 2,
		shell = vim.fn.executable("fish") == 1 and "fish" or vim.o.shell,
		start_in_insert = true,
		size = function(term)
			if term.direction == "horizontal" then
				return vim.o.lines * 0.3
			elseif term.direction == "vertical" then
				return vim.o.columns * 0.5
			end
		end,
	},
}
