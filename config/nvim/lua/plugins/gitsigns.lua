return {
  "lewis6991/gitsigns.nvim",
  enabled = function()
    return vim.fn.executable("git") == 1
  end,
  event = "VeryLazy",
  config = function(_, opts)
    require("gitsigns").setup(opts)
  end
}
