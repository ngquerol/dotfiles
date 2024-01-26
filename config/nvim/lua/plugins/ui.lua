return {
	{
		"nvim-telescope/telescope.nvim",
		version = false, ---@diagnostic disable-line: assign-type-mismatch
		dependencies = {
			{
				"ahmedkhalf/project.nvim",
				main = "project_nvim",
				opts = {
					patterns = {
						".git",
						"_darcs",
						".hg",
						".bzr",
						".svn",
						"Makefile",
						"package.json",
						"go.mod",
					},
				},
			},
			{ "nvim-lua/plenary.nvim" },
			{ "nvim-telescope/telescope-file-browser.nvim" },
			{ "nvim-telescope/telescope-ui-select.nvim" },
			{ "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
		},
		keys = {
			{ "<leader>fb", "<cmd>Telescope buffers<cr>", desc = "buffers" },
			{ "<leader>fd", "<cmd>Telescope diagnostics bufnr=0<cr>", desc = "diagnostics" },
			{ "<leader>fD", "<cmd>Telescope diagnostics<cr>", desc = "workspace diagnostics" },
			{ "<leader>ff", "<cmd>Telescope find_files<cr>", desc = "files" },
			{ "<leader>fF", "<cmd>Telescope file_browser<cr>", desc = "browse files" },
			{ "<leader>fg", "<cmd>Telescope git_files<cr>", desc = "files (git)" },
			{ "<leader>fh", "<cmd>Telescope help_tags<cr>", desc = "help tags" },
			{ "<leader>fk", "<cmd>Telescope keymaps<cr>", desc = "keymaps" },
			{ "<leader>fm", "<cmd>Telescope man_pages<cr>", desc = "man pages" },
			{ "<leader>fo", "<cmd>Telescope oldfiles<cr>", desc = "old files" },
			{
				"<leader>fp",
				vim.schedule_wrap(function()
					require("telescope").extensions.projects.projects({})
				end),
				desc = "projects",
			},
			{ "<leader>ft", "<cmd>Telescope<cr>", desc = "telescope" },
			{ "<leader>fr", "<cmd>Telescope registers<cr>", desc = "registers" },
			{ "<leader>fs", "<cmd>Telescope live_grep<cr>", desc = "live grep" },
			{ "<leader>fS", "<cmd>Telescope grep_string<cr>", desc = "grep word at cursor" },
			-- LSP stuff
			{ "<leader>flt", "<cmd>Telescope lsp_type_definitions<cr>", desc = "type definition" },
			{ "<leader>fli", "<cmd>Telescope lsp_implementations<cr>", desc = "go to implementation(s)" },
			{ "<leader>flr", "<cmd>Telescope lsp_references<cr>", desc = "references" },
			{ "<leader>fli", "<cmd>Telescope lsp_incoming_calls<cr>", desc = "incoming calls" },
			{ "<leader>flo", "<cmd>Telescope lsp_outgoing_calls<cr>", desc = "outgoing calls" },
			{ "<leader>fls", "<cmd>Telescope lsp_document_symbols<cr>", desc = "document symbols" },
			{ "<leader>flS", "<cmd>Telescope lsp_dynamic_workspace_symbols<cr>", desc = "workspace symbols" },
		},
		opts = function(_, _)
			-- local actions = require("telescope.actions")
			return {
				defaults = {
					-- mappings = {
					-- 	i = { ["<CR>"] = actions.select_default + actions.center },
					-- },
					layout_config = {
						prompt_position = "top",
						height = { 0.5, min = 25, max = 100 },
						width = { 0.7, min = 35, max = 120 },
					},
					path_display = { "truncate" },
					sorting_strategy = "ascending",
					dynamic_preview_title = true,
					file_ignore_patterns = {
						"node_modules/",
						".git/",
						".cargo/",
						".m2/",
					},
				},
				pickers = {
					buffers = {
						theme = "dropdown",
						previewer = false,
					},
					find_files = {
						hidden = true,
					},
					oldfiles = {
						theme = "dropdown",
						previewer = false,
					},
					diagnostics = {
						theme = "dropdown",
						previewer = false,
					},
				},
				extensions = {
					file_browser = {
						dir_icon = "",
						grouped = true,
						layout_strategy = "vertical",
						layout_config = {
							mirror = true,
							width = { 0.5, min = 30, max = 120 },
						},
					},
				},
			}
		end,
		config = function(_, opts)
			require("telescope").setup(opts)
			require("telescope").load_extension("fzf")
			require("telescope").load_extension("ui-select")
			require("telescope").load_extension("file_browser")
			require("telescope").load_extension("projects")
		end,
	},
	{
		"nvim-lualine/lualine.nvim",
		event = "VeryLazy",
		opts = function()
			return {
				options = {
					globalstatus = vim.opt.laststatus:get() == 3,
					icons_enabled = false,
					theme = "auto",
					component_separators = { left = "╱", right = "╱" },
					section_separators = { left = "", right = "" },
					disabled_filetypes = { "lazy", "NvimTree" },
				},
				sections = {
					lualine_b = {
						"branch",
						"diff",
						{
							"diagnostics",
							sources = {
								"nvim_lsp",
								"nvim_diagnostic",
							},
							symbols = {
								error = "✖ ",
								warn = "▲ ",
								info = "● ",
								hint = "🞷 ",
							},
						},
					},
					lualine_c = {
						{ "filename", path = 3 },
					},
					lualine_y = {
						"searchcount",
						"selectioncount",
						"progress",
					},
				},
				extensions = {
					"toggleterm",
				},
			}
		end,
	},
	{
		"folke/noice.nvim",
		event = "VeryLazy",
		dependencies = { "MunifTanjim/nui.nvim" },
		opts = {
			cmdline = {
				format = {
					cmdline = { icon = ">" },
					search_down = { icon = "⌄" },
					search_up = { icon = "⌃" },
					filter = { icon = "$" },
					lua = { icon = "🌝" },
					help = { icon = "?" },
				},
			},
			lsp = {
				override = {
					["vim.lsp.util.convert_input_to_markdown_lines"] = true,
					["vim.lsp.util.stylize_markdown"] = true,
					["cmp.entry.get_documentation"] = true,
				},
			},
			format = {
				level = {
					icons = {
						error = "✖",
						warn = "▲",
						info = "●",
					},
				},
			},
			popupmenu = {
				kind_icons = false,
			},
			presets = {
				long_message_to_split = true,
				lsp_doc_border = true,
			},
			routes = {
				{
					filter = {
						event = "msg_show",
						kind = "search_count",
					},
					opts = { skip = true },
				},
			},
		},
		config = function(_, opts)
			require("noice").setup(opts)

			vim.keymap.set({ "n", "i", "s" }, "<c-f>", function()
				if not require("noice.lsp").scroll(4) then
					return "<c-f>"
				end
			end, { silent = true, expr = true })

			vim.keymap.set({ "n", "i", "s" }, "<c-b>", function()
				if not require("noice.lsp").scroll(-4) then
					return "<c-b>"
				end
			end, { silent = true, expr = true })
		end,
	},
	{
		"folke/which-key.nvim",
		event = "VeryLazy",
		opts = {
			show_help = false,
			window = {
				border = "rounded",
			},
		},
	},
	{
		"nvim-tree/nvim-tree.lua",
		keys = {
			{ "<leader>n", "<cmd>NvimTreeToggle<cr>", desc = "Toggle NeoTree" },
		},
		opts = {
			filters = { dotfiles = true },
			renderer = {
				group_empty = true,
				add_trailing = true,
				indent_markers = {
					enable = true,
					icons = { corner = "╰" },
				},
				icons = {
					show = {
						file = false,
						folder_arrow = false,
						git = false,
						modified = false,
					},
					glyphs = {
						modified = "●",
						folder = {
							default = "▶",
							open = "▼",
							arrow_closed = "▶",
							arrow_open = "▼",
							empty = "▷",
							empty_open = "▽",
							symlink = "↪",
							symlink_open = "⤥",
						},
						git = {
							deleted = "",
							ignored = "◌",
							renamed = "➜",
							staged = "✓",
							unmerged = "",
							unstaged = "✗",
							untracked = "✲",
						},
					},
				},
			},
		},
	},
	{
		"akinsho/bufferline.nvim",
		enabled = false, -- see incline.nvim below
		opts = {
			options = {
				separator_style = "thin",
				diagnostics = "nvim_lsp",
				show_buffer_close_icons = false,
				show_buffer_icons = false,
				show_close_icon = false,
			},
		},
	},
	{
		"b0o/incline.nvim",
		event = "FileType",
		opts = {
			hide = {
				only_win = true,
			},
		},
	},
	{
		"folke/zen-mode.nvim",
		dependencies = {
			"folke/twilight.nvim",
			opts = {
				expand = {
					"function",
					"method",
					"class",
					"function_definition",
				},
			},
		},
		cmd = { "ZenMode" },
		keys = {
			{ "<leader>z", "<cmd>ZenMode<cr>", desc = "Toggle Zen mode" },
		},
		opts = {},
	},
}
