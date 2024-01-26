return {
	{
		"neovim/nvim-lspconfig",
		version = false, ---@diagnostic disable-line: assign-type-mismatch
		event = { "BufReadPre", "BufNewFile" },
		dependencies = {
			{ "folke/neodev.nvim", opts = {} },
			{
				"williamboman/mason.nvim",
				cmd = "Mason",
				keys = {
					{ "<leader>M", "<cmd>Mason<cr>", desc = "Mason" },
				},
				opts = {
					ui = {
						border = "rounded",
						icons = {
							package_installed = "✓",
							package_pending = "➜",
							package_uninstalled = "✗",
						},
					},
				},
			},
		},
		keys = {
			{ "<leader>ga", vim.lsp.buf.code_action, desc = "code action(s)" },
			{ "<leader>gr", vim.lsp.buf.rename, desc = "rename symbol" },
			{ "<leader>gi", vim.lsp.buf.implementation, desc = "go to implementation" },
			{ "<leader>gd", vim.lsp.buf.definition, desc = "go to definition" },
			{ "<leader>gD", vim.lsp.buf.declaration, desc = "go to declaration" },
		},
		config = function()
			local lspconfig = require("lspconfig")

			local capabilities = vim.tbl_deep_extend(
				"force",
				lspconfig.util.default_config.capabilities,
				require("cmp_nvim_lsp").default_capabilities()
			)

			local on_attach = function(client, bufnr)
				if client.server_capabilities.inlayHintProvider then
					vim.lsp.inlay_hint.enable(bufnr, true)
				end
			end

			lspconfig.lua_ls.setup({
				capabilities = capabilities,
				on_attach = on_attach,
				settings = {
					Lua = {
						runtime = { version = "LuaJIT" },
						diagnostics = { globals = { "vim" } },
						workspace = { checkThirdParty = false },
						telemetry = { enable = false },
					},
				},
			})

			lspconfig.clangd.setup({
				capabilities = capabilities,
				on_attach = on_attach,
			})

			lspconfig.gopls.setup({
				capabilities = capabilities,
				on_attach = on_attach,
				settings = {
					gopls = {
						gofumpt = true,
						analyses = {
							fieldalignment = true,
							nilness = true,
							unusedparams = true,
							unusedwrite = true,
							useany = true,
						},
						hints = {
							assignVariableTypes = true,
							compositeLiteralFields = true,
							compositeLiteralTypes = true,
							constantValues = true,
							functionTypeParameters = true,
							parameterNames = true,
							rangeVariableTypes = true,
						},
					},
				},
			})
		end,
	},
	{
		"j-hui/fidget.nvim",
		enabled = false,
		event = "LspAttach",
		opts = {
			notification = {
				window = { border = "rounded" },
			},
			integration = {
				["nvim-tree"] = { enable = true },
			},
		},
		config = function(_, opts)
			require("fidget").setup(opts)
		end,
	},
}
