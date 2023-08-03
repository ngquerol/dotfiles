return {
  {
    "neovim/nvim-lspconfig",
    version = false, ---@diagnostic disable-line: assign-type-mismatch
    event = { "BufReadPre", "BufNewFile" },
    dependencies = {
      { "folke/neodev.nvim", opts = {} },
    },
    config = function()
      local lspconfig = require("lspconfig")

      local capabilities = vim.tbl_deep_extend(
        "force",
        lspconfig.util.default_config.capabilities,
        require("cmp_nvim_lsp").default_capabilities()
      )

      local on_attach = function(client, bufnr)
        if not client.server_capabilities.documentFormattingProvider then
          return
        end

        ---@diagnostic disable-next-line: redefined-local
        local filter = function(client)
          return client.name ~= "tsserver"
        end

        vim.api.nvim_create_autocmd("BufWritePre", {
          group = vim.api.nvim_create_augroup("Format", { clear = true }),
          buffer = bufnr,
          callback = function()
            vim.lsp.buf.format({
              timeout_ms = 1000,
              filter = filter
            })
          end
        })
      end

      lspconfig.lua_ls.setup({
        capabilities = capabilities,
        on_attach = on_attach,
        settings = {
          Lua = {
            runtime = { version = "LuaJIT" },
            diagnostics = { globals = { "vim" } },
            workspace = {
              library = vim.api.nvim_get_runtime_file("", true),
              checkThirdParty = false,
            },
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
      })

      lspconfig.rust_analyzer.setup({
        capabilities = capabilities,
        on_attach = on_attach,
      })
    end
  },
  {
    "nvimdev/lspsaga.nvim",
    event = "LspAttach",
    dependencies = { "nvim-treesitter/nvim-treesitter" },
    opts = {
      symbol_in_winbar = { enable = false },
      lightbulb = { virtual_text = false },
      ui = {
        border = "rounded",
        devicon = false,
        expand = "▶",
        collapse = "▼",
        lines = { "╰", "├", "│", "─", "╭" }
      },
    },
    keys = {
      { "<leader>ll", "<cmd>Lspsaga code_action<cr>",                desc = "Code Action(s)" },
      { "<leader>lf", "<cmd>Lspsaga finder<cr>",                     desc = "Find Symbol" },
      { "<leader>lr", "<cmd>Lspsaga rename<cr>",                     desc = "Rename Symbol" },
      { "<leader>lh", "<cmd>Lspsaga hover_doc<cr>",                  desc = "Show Hover Doc" },
      { "<leader>lg", "<cmd>Lspsaga goto_definition<cr>",            desc = "Go to Definition" },
      { "<leader>lp", "<cmd>Lspsaga peek_definition<cr>",            desc = "Peek Definition" },
      { "<leader>lG", "<cmd>Lspsaga goto_type_definition<cr>",       desc = "Go to Type Definition" },
      { "<leader>lP", "<cmd>Lspsaga peek_type_definition<cr>",       desc = "Peek Type Definition" },
      { "<leader>ld", "<cmd>Lspsaga show_buf_diagnostics<cr>",       desc = "Buffer Diagnostics" },
      { "<leader>lD", "<cmd>Lspsaga show_workspace_diagnostics<cr>", desc = "Workspace Diagnostics" },
    },
    config = function(_, opts)
      require("lspsaga").setup(opts)
      for _, hl in pairs({ "SagaBorder", "HoverBorder" }) do
        vim.api.nvim_set_hl(0, hl, { bg = nil })
      end
    end
  },
  {
    "j-hui/fidget.nvim",
    branch = "legacy",
    event = "LspAttach",
    opts = {
      timer = { fidget_decay = 250 },
      window = { border = "rounded" },
      text = { spinner = "dots" }
    },
    config = function(_, opts)
      require("fidget").setup(opts)
    end
  }
}
