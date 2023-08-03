return {
  "nvim-tree/nvim-tree.lua",
  keys = {
    { "<leader>n", "<cmd>NvimTreeToggle<cr>", desc = "Toggle NeoTree" },
  },
  opts = {
    filters = { dotfiles = true },
    renderer = {
      group_empty = true,
      add_trailing = true,
      indent_markers = {
        enable = true,
        icons = { corner = "╰" }
      },
      icons = {
        show = {
          file = false,
          folder_arrow = false,
          git = true,
          modified = true,
        },
        glyphs = {
          modified = "●",
          folder = {
            default = "▶",
            open = "▼",
            arrow_closed = "▶",
            arrow_open = "▼",
            empty = "▷",
            empty_open = "▽",
            symlink = "↪",
            symlink_open = "⤥",
          },
          git = { untracked = "✲" }
        },
      },
    },
  },
}
