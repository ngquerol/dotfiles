return {
	"catppuccin/nvim",
	name = "catpuccin",
	lazy = false,
	priority = 1000,
	opts = {
		flavour = "mocha",
		background = {
			light = "latte",
			dark = "mocha",
		},
		term_colors = true,
		custom_highlights = function(colors)
			return {
				LspInlayHint = { bg = colors.none },
			}
		end,
	},
	config = function(_, opts)
		require("catppuccin").setup(opts)
		vim.cmd.colorscheme("catppuccin")
	end,
}
