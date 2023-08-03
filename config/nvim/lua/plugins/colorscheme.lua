return {
  "catppuccin/nvim",
  name = "catpuccin",
  lazy = false,
  priority = 1000,
  opts = {
    flavour = "frappe",
    background = {
      light = "latte",
      dark = "frappe",
    },
    term_colors = true,
  },
  config = function(_, opts)
    require("catppuccin").setup(opts)
    vim.cmd.colorscheme "catppuccin"
  end
}

