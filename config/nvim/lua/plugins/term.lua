return {
	"akinsho/toggleterm.nvim",
	keys = {
		{ "<leader>tf", "<cmd>ToggleTerm direction=float<cr>" },
		{ "<leader>tv", "<cmd>ToggleTerm direction=vertical<cr>" },
		{ "<leader>ts", "<cmd>ToggleTerm direction=horizontal<cr>" },
		{ "<leader>tt", "<cmd>TermSelect<cr>" },
	},
	opts = {
		close_on_exit = true,
		direction = "float",
		float_opts = { border = "rounded" },
		hide_numbers = true,
		persist_size = true,
		shade_terminals = true,
		shading_factor = 2,
		shell = vim.o.shell,
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
