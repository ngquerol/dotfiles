return {
  {
    "nvim-treesitter/nvim-treesitter",
    opts = {
      ensure_installed = { "ninja", "python", "rst", "toml" },
    },
  },
  {
    "williamboman/mason.nvim",
    opts = {
      ensure_installed = { "ruff" },
    },
  },
  {
    "stevearc/conform.nvim",
    opts = {
      formatters_by_ft = {
        python = { "ruff" },
      },
    },
  },
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        pyright = {},
      },
    },
  },
  {
    "nvim-neotest/neotest",
    dependencies = { "nvim-neotest/neotest-python" },
  },
  {
    "linux-cultist/venv-selector.nvim",
    cmd = "VenvSelect",
    branch = "regexp",
    keys = {
      { "<leader>cv", "<cmd>VenvSelect<cr>", desc = "Select virtualenv" },
      { "<leader>cV", "<cmd>VenvSelectCached<cr>", desc = "Select cached virtualenv" },
    },
    opts = {
      auto_refresh = true,
      name = { "venv", ".venv", "env", ".env" },
    },
  },
}
