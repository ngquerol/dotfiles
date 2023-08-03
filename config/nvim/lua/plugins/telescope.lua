return {
  "nvim-telescope/telescope.nvim",
  version = false, ---@diagnostic disable-line: assign-type-mismatch
  dependencies = {
    { "nvim-lua/plenary.nvim" },
    { "nvim-telescope/telescope-ui-select.nvim" },
    { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
  },
  keys = {
    { "<leader>fG", "<cmd>Telescope live_grep<cr>",   desc = "Live grep" },
    { "<leader>fw", "<cmd>Telescope grep_string<cr>", desc = "Grep word at cursor" },
    { "<leader>ff", "<cmd>Telescope find_files<cr>",  desc = "Files" },
    { "<leader>fg", "<cmd>Telescope git_files<cr>",   desc = "Files (git)" },
    { "<leader>fo", "<cmd>Telescope oldfiles<cr>",    desc = "Old Files" },
    { "<leader>fb", "<cmd>Telescope buffers<cr>",     desc = "Buffers" },
    { "<leader>fh", "<cmd>Telescope help_tags<cr>",   desc = "Help tags" },
    { "<leader>ft", "<cmd>Telescope<cr>",             desc = "Telescope" },
  },
  opts = {},
  config = function(_, opts)
    require("telescope").setup(opts)
    require("telescope").load_extension("fzf")
    require("telescope").load_extension("ui-select")
  end
}
