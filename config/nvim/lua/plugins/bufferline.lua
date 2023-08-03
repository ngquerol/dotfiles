return {
  "akinsho/bufferline.nvim",
  event = "VeryLazy",
  keys = {
    { "<leader>bp", "<Cmd>BufferLineTogglePin<CR>",            desc = "Toggle pin" },
    { "<leader>bP", "<Cmd>BufferLineGroupClose ungrouped<CR>", desc = "Delete non-pinned buffers" },
  },
  opts = {
    options = {
      always_show_bufferline = false,
      offsets = { { filetype = "NvimTree", separator = false } },
      diagnostics = "nvim_lsp",
      separator_style = "thick",
      show_buffer_icons = false,
      show_buffer_close_icons = false,
      show_close_icon = false,
    },
  },
}
