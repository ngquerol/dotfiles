return {
  {
    "catppuccin/nvim",
    version = false,
    name = "catpuccin",
    lazy = false,
    priority = 1000,
    opts = {
      background = { dark = "mocha" },
      term_colors = true,
      no_italic = true,
      integrations = {
        blink_cmp = true,
        lsp_trouble = true,
        notify = true,
        native_lsp = {
          inlay_hints = { background = false },
          underlines = {
            errors = { "undercurl" },
            hints = { "undercurl" },
            warnings = { "undercurl" },
            information = { "undercurl" },
          },
        },
        mason = true,
        neotest = true,
        snacks = true,
        which_key = true,
      },
    },
    config = function(_, opts)
      require("catppuccin").setup(opts)
      vim.cmd.colorscheme("catppuccin-mocha")
    end,
  },
}
