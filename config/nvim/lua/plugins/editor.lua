return {
	{
		"windwp/nvim-autopairs",
		event = "InsertEnter",
		opts = {},
	},
	{
		"numToStr/Comment.nvim",
		main = "Comment",
		keys = {
			{ "gc", mode = { "n", "v" }, desc = "Comment toggle linewise" },
			{ "gb", mode = { "n", "v" }, desc = "Comment toggle blockwise" },
		},
	},
	{
		"ethanholz/nvim-lastplace",
		config = true,
	},
	{
		"romainl/vim-cool",
		keys = { "/", "?", "*", "#", "g*", "g#", "n", "N" },
	},
	{
		"kylechui/nvim-surround",
		event = "VeryLazy",
		opts = {},
	},
	{
		"rgroli/other.nvim",
		main = "other-nvim",
		keys = { { "<leader>gO", "<cmd>Other<cr>", desc = "open alternative file" } },
		opts = {
			mappings = { "golang" },
			rememberBuffers = false,
			showMissingFiles = false,
			style = {
				border = "rounded",
				separator = "|",
			},
		},
	},
	{
		"L3MON4D3/LuaSnip",
		dependencies = {
			"rafamadriz/friendly-snippets",
			config = function()
				require("luasnip.loaders.from_vscode").lazy_load()
			end,
		},
		opts = {
			history = true,
			delete_check_events = "TextChanged",
		},
		keys = {
			-- {
			--     "<tab>",
			--     function()
			--         return require("luasnip").jumpable(1)
			--             and "<Plug>luasnip-jump-next"
			--             or "<tab>"
			--     end,
			--     expr = true,
			--     silent = true,
			--     mode = "i",
			-- },
			-- { "<tab>",   function() require("luasnip").jump(1) end,  mode = "s" },
			-- { "<s-tab>", function() require("luasnip").jump(-1) end, mode = { "i", "s" } },
		},
	},
	{
		"lewis6991/gitsigns.nvim",
		enabled = function()
			return vim.fn.executable("git") == 1
		end,
		event = "VeryLazy",
		config = function(_, opts)
			require("gitsigns").setup(opts)
		end,
	},
	{
		"folke/flash.nvim",
		keys = {
			{
				"<leader>s",
				mode = { "n", "x", "o" },
				function()
					require("flash").jump()
				end,
				desc = "Flash",
			},
			{
				"<leader>S",
				mode = { "n", "x", "o" },
				function()
					require("flash").treesitter()
				end,
				desc = "Flash Treesitter",
			},
		},
		opts = {
			labels = "qsdfghjklazertyuiopwxcvbn",
			rainbow = {
				enabled = true,
			},
		},
	},
	{
		"stevearc/conform.nvim",
		event = { "BufWritePre", "BufNewFile" },
		cmd = { "ConformInfo" },
		keys = {
			{
				"<leader>gF",
				function()
					require("conform").format({
						async = true,
						lsp_fallback = true,
					})
				end,
				mode = "",
				desc = "format buffer",
			},
		},
		opts = {
			formatters_by_ft = {
				c = { "clang_format" },
				cpp = { "clang_format" },
				objc = { "clang_format" },
				javascript = { { "prettierd", "prettier" } },
				lua = { "stylua" },
				python = { "isort", "black" },
				sh = { "shfmt" },
			},
			format_on_save = {
				timeout_ms = 500,
				lsp_fallback = true,
			},
		},
		init = function()
			vim.o.formatexpr = "v:lua.require'conform'.formatexpr()"
		end,
	},
	{
		"hrsh7th/nvim-cmp",
		version = false, ---@diagnostic disable-line: assign-type-mismatch
		event = { "InsertEnter", "CmdLineEnter" },
		dependencies = {
			"hrsh7th/cmp-buffer",
			"hrsh7th/cmp-cmdline",
			"hrsh7th/cmp-nvim-lsp",
			-- "hrsh7th/cmp-nvim-lsp-signature-help"
			"hrsh7th/cmp-nvim-lua",
			"hrsh7th/cmp-path",
			"saadparwaiz1/cmp_luasnip",
		},
		config = function()
			local cmp = require("cmp")
			local cmp_autopairs = require("nvim-autopairs.completion.cmp")
			cmp.event:on("confirm_done", cmp_autopairs.on_confirm_done())

			local luasnip = require("luasnip")

			cmp.setup({
				mapping = {
					["<c-n>"] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior, count = 1 }),
					["<c-p>"] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior, count = 1 }),
					["<c-f>"] = cmp.mapping.scroll_docs(4),
					["<c-b>"] = cmp.mapping.scroll_docs(-4),
					["<c-space>"] = cmp.mapping.complete(),
					["<c-e>"] = cmp.mapping.abort(),
					["<cr>"] = cmp.mapping.confirm({ select = true }),
					["<s-cr>"] = cmp.mapping.confirm({
						behavior = cmp.ConfirmBehavior.Replace,
						select = true,
					}),
					["<tab>"] = cmp.mapping(function(fallback)
						if cmp.visible() then
							cmp.select_next_item()
						elseif luasnip.expand_or_jumpable() then
							luasnip.expand_or_jump()
						else
							fallback()
						end
					end, { "i", "s" }),
					["<s-tab>"] = cmp.mapping(function(fallback)
						if cmp.visible() then
							cmp.select_prev_item()
						elseif luasnip.jumpable(-1) then
							luasnip.jump(-1)
						else
							fallback()
						end
					end, { "i", "s" }),
				},
				sources = cmp.config.sources({
					{ name = "nvim_lsp" },
					{ name = "nvim_lua" },
					{
						name = "luasnip",
						max_item_count = 5,
					},
					{
						name = "buffer",
						keyword_length = 5,
					},
					{ name = "path" },
					-- { name = 'nvim_lsp_signature_help' }
				}),
				snippet = {
					expand = function(args)
						require("luasnip").lsp_expand(args.body)
					end,
				},
				window = {
					completion = cmp.config.window.bordered(),
					documentation = cmp.config.window.bordered(),
				},
				performance = { max_view_entries = 20 },
				experimental = { ghost_text = false },
			})

			cmp.setup.cmdline({ "/", "?" }, {
				mapping = cmp.mapping.preset.cmdline(),
				sources = {
					{ name = "buffer" },
				},
			})

			cmp.setup.cmdline(":", {
				mapping = cmp.mapping.preset.cmdline(),
				sources = cmp.config.sources({
					{ name = "path" },
					{ name = "cmdline" },
				}),
			})
		end,
	},
}
