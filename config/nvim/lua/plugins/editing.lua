return {
  {
    "windwp/nvim-autopairs",
    event = "InsertEnter",
    opts = {}
  },
  {
    "numToStr/Comment.nvim",
    keys = {
      { "gc", mode = { "n", "v" }, desc = "Comment toggle linewise" },
      { "gb", mode = { "n", "v" }, desc = "Comment toggle blockwise" },
    },
    config = function()
      require("Comment").setup({})
    end
  },
  {
    "ethanholz/nvim-lastplace",
    config = true,
  }, {
  "Wansmer/treesj",
  keys = {
    { "gj", "<cmd>TSJJoin<cr>",   desc = "Join lines" },
    { "gs", "<cmd>TSJSplit<cr>",  desc = "Split lines" },
    { "gJ", "<cmd>TSJToggle<cr>", desc = "Join/Split lines" },
  },
  opts = {
    use_default_keymaps = false,
    max_join_length = 150,
  }
}
}
