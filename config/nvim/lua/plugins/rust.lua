return {
  {
    "nvim-treesitter/nvim-treesitter",
    opts = {
      ensure_installed = { "ron", "rust" },
    },
  },
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        rust_analyzer = {
          settings = {
            ["rust-analyzer"] = {
              completion = {
                autoimport = { enable = true },
                postfix = { enable = true },
              },
              inlayHints = {
                chainingHints = { enable = true },
                closureStyle = { enable = "rust_analyzer" },
                closureReturnTypeHints = { enable = "always" },
                expressionAdjustmentHints = { enable = "never" },
                implicitDrops = { enable = false },
                lifetimeElisionHints = {
                  enable = "always",
                  useParameterNames = true,
                },
                renderColons = true,
              },
              procMacro = { enable = true },
            },
          },
        },
      },
    },
  },
  {
    "nvim-neotest/neotest",
    dependencies = { "rouge8/neotest-rust" },
    opts = {
      adapters = {
        ["neotest-rust"] = {},
      },
    },
  },
}
