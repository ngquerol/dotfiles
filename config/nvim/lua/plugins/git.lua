return {
  {
    "folke/snacks.nvim",
    keys = {
      {
        "<leader>vb",
        function()
          Snacks.picker.git_branches()
        end,
        desc = "Git Branches",
      },
      {
        "<leader>vB",
        function()
          Snacks.gitbrowse()
        end,
        desc = "Git Browse",
      },
      {
        "<leader>vl",
        function()
          Snacks.picker.git_log()
        end,
        desc = "Git Log",
      },
      {
        "<leader>vL",
        function()
          Snacks.picker.git_log_line()
        end,
        desc = "Git Log Line",
      },
      {
        "<leader>vs",
        function()
          Snacks.picker.git_status()
        end,
        desc = "Git Status",
      },
      {
        "<leader>vS",
        function()
          Snacks.picker.git_stash()
        end,
        desc = "Git Stash",
      },
      {
        "<leader>vf",
        function()
          Snacks.picker.git_log_file()
        end,
        desc = "Git Log File",
      },
      {
        "<leader>Tf",
        function()
          Snacks.lazygit.log_file()
        end,
        desc = "Lazygit log (file)",
      },
      {
        "<leader>Tg",
        function()
          Snacks.lazygit.open()
        end,
        desc = "Lazygit",
      },
      {
        "<leader>Tl",
        function()
          Snacks.lazygit.log()
        end,
        desc = "Lazygit log (cwd)",
      },
    },
    opts = {
      gitbrowse = {},
      lazygit = {},
    },
  },
  {
    "lewis6991/gitsigns.nvim",
    main = "gitsigns",
    enabled = function()
      return vim.fn.executable("git") == 1
    end,
    event = { "BufReadPost", "BufWritePost", "BufNewFile" },
    opts = {
      signs = {
        add = { text = "│" },
        change = { text = "│" },
        changedelete = { text = "│" },
        untracked = { text = "│" },
      },
      preview_config = { border = "rounded" },
      on_attach = function(buffer)
        local gs = require("gitsigns")

        local function map(mode, l, r, desc)
          vim.keymap.set(mode, l, r, { buffer = buffer, desc = desc })
        end

        -- stylua: ignore start
        map("n", "]h", function()
          if vim.wo.diff then
            vim.cmd.normal({ "]c", bang = true })
          else
            gs.nav_hunk("next")
          end
        end, "Next Hunk")
        map("n", "[h", function()
          if vim.wo.diff then
            vim.cmd.normal({ "[c", bang = true })
          else
            gs.nav_hunk("prev")
          end
        end, "Prev Hunk")
        map("n", "]H", function() gs.nav_hunk("last") end, "Last Hunk")
        map("n", "[H", function() gs.nav_hunk("first") end, "First Hunk")
        map({ "n", "v" }, "<leader>vgs", ":Gitsigns stage_hunk<CR>", "Stage Hunk")
        map({ "n", "v" }, "<leader>vgr", ":Gitsigns reset_hunk<CR>", "Reset Hunk")
        map("n", "<leader>vgS", gs.stage_buffer, "Stage Buffer")
        map("n", "<leader>vgu", gs.stage_hunk, "Undo Stage Hunk")
        map("n", "<leader>vgR", gs.reset_buffer, "Reset Buffer")
        map("n", "<leader>vgp", gs.preview_hunk_inline, "Preview Hunk Inline")
        map("n", "<leader>vgb", function() gs.blame_line({ full = true }) end, "Blame Line")
        map("n", "<leader>vgB", function() gs.blame() end, "Blame Buffer")
        map("n", "<leader>vgd", gs.diffthis, "Diff This")
        map("n", "<leader>vgD", function() gs.diffthis("~") end, "Diff This ~")
      end,
    },
  },
}
