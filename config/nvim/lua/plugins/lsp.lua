return {
  {
    "neovim/nvim-lspconfig",
    event = { "BufReadPost", "BufNewFile", "BufWritePre" },
    dependencies = {
      { "williamboman/mason.nvim" },
      { "williamboman/mason-lspconfig.nvim" },
      {
        "folke/lazydev.nvim",
        ft = "lua",
        opts = {
          library = {
            { path = "${3rd}/luv/library", words = { "vim%.uv" } },
            { path = "snacks.nvim", words = { "Snacks" } },
          },
        },
      },
      { "saghen/blink.cmp" },
    },
    keys = {
      { "ga", vim.lsp.buf.code_action, desc = "Code action(s)" },
      { "gR", vim.lsp.buf.rename, desc = "Rename symbol" },
      {
        "<leader>gh",
        function()
          vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled({ bufnr = 0 }), { bufnr = 0 })
        end,
        desc = "Toggle inlay hints",
      },
    },
    opts = {
      enable_inlay_hints = false,
      servers = {
        lua_ls = {
          settings = {
            Lua = {
              runtime = { version = "LuaJIT" },
              diagnostics = { globals = { "vim" } },
              workspace = { checkThirdParty = false },
              telemetry = { enable = false },
            },
          },
        },
      },
    },
    config = function(_, opts)
      local lspconfig = require("lspconfig")
      local mlsp = require("mason-lspconfig")
      local blink = require("blink.cmp")

      local on_attach = function(client, bufnr)
        if client.server_capabilities.inlayHintProvider then
          vim.lsp.inlay_hint.enable(opts.enable_inlay_hints, { bufnr = bufnr })
        end
      end

      local setup = function(server)
        if not opts.servers[server] then
          return
        end

        local server_opts = opts.servers[server] or {}
        local capabilities = blink.get_lsp_capabilities(server_opts.capabilities or {}, true)

        local config = vim.tbl_deep_extend("keep", server_opts, {
          capabilities = capabilities,
          on_attach = on_attach,
        })

        lspconfig[server].setup(config)
      end

      local ensure_installed = {}
      for server in pairs(opts.servers) do
        ensure_installed[#ensure_installed + 1] = server
      end

      ---@diagnostic disable-next-line: missing-fields
      mlsp.setup({
        ensure_installed = ensure_installed,
        handlers = { setup },
      })
    end,
  },
  {
    "folke/snacks.nvim",
    keys = {
      {
        "gd",
        function()
          Snacks.picker.lsp_definitions()
        end,
        desc = "Go to Definition",
      },
      {
        "gD",
        function()
          Snacks.picker.lsp_declarations()
        end,
        desc = "Go to Declaration",
      },
      {
        "gr",
        function()
          Snacks.picker.lsp_references()
        end,
        nowait = true,
        desc = "References",
      },
      {
        "gI",
        function()
          Snacks.picker.lsp_implementations()
        end,
        desc = "Go to Implementation",
      },
      {
        "gy",
        function()
          Snacks.picker.lsp_type_definitions()
        end,
        desc = "Go to T[y]pe Definition",
      },
      {
        "<leader>ss",
        function()
          Snacks.picker.lsp_symbols()
        end,
        desc = "LSP Symbols",
      },
      {
        "<leader>sS",
        function()
          Snacks.picker.lsp_workspace_symbols()
        end,
        desc = "LSP Workspace Symbols",
      },
    },
  },
  {
    "kosayoda/nvim-lightbulb",
    version = false,
    event = "LspAttach",
    opts = {
      autocmd = { enabled = true },
      code_lenses = true,
      sign = { enabled = false },
      status_text = { enabled = true },
      virtual_text = { enabled = false },
    },
  },
  {
    "j-hui/fidget.nvim",
    event = "VeryLazy",
    opts = {
      notification = {
        window = {
          align = "top",
          border = "rounded",
        },
      },
    },
  },
}
