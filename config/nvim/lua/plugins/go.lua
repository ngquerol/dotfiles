return {
  {
    "nvim-treesitter/nvim-treesitter",
    opts = {
      ensure_installed = { "go", "gomod", "gowork", "gosum" },
    },
  },
  {
    "williamboman/mason.nvim",
    opts = {
      ensure_installed = { "golangci-lint", "goimports", "gofumpt" },
    },
  },
  {
    "stevearc/conform.nvim",
    opts = {
      formatters_by_ft = {
        go = { "goimports", "gofumpt" },
      },
    },
  },
  {
    "mfussenegger/nvim-lint",
    opts = {
      linters_by_ft = {
        go = { "golangcilint" },
      },
    },
  },
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        gopls = {
          settings = {
            gopls = {
              directoryFilters = {
                "-.git",
                "-.vscode",
                "-.idea",
                "-.vscode-test",
                "-node_modules",
              },
              gofumpt = vim.fn.executable("gofumpt") == 1,
              hints = {
                assignVariableTypes = true,
                constantValues = true,
                functionTypeParameters = true,
                parameterNames = true,
                rangeVariableTypes = true,
              },
              templateExtensions = { "tmpl", "gohtml" },
              usePlaceholders = true,
            },
          },
        },
      },
    },
  },
  {
    "nvim-neotest/neotest",
    dependencies = { "nvim-neotest/neotest-go" },
    opts = {
      adapters = {
        ["neotest-go"] = { recursive_run = true },
      },
    },
  },
}
