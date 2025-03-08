return {
  {
    "folke/snacks.nvim",
    opts = {
      bufdelete = {},
      dashboard = {
        preset = {
          keys = {
            { icon = " ", key = "f", desc = "Find File", action = ":lua Snacks.dashboard.pick('files')" },
            { icon = " ", key = "n", desc = "New File", action = ":ene | startinsert" },
            { icon = " ", key = "p", desc = "Projects", action = ":lua Snacks.dashboard.pick('projects')" },
            { icon = " ", key = "r", desc = "Recent Files", action = ":lua Snacks.dashboard.pick('oldfiles')" },
            {
              icon = " ",
              key = "c",
              desc = "Config",
              action = ":lua Snacks.dashboard.pick('files', {cwd = vim.fn.stdpath('config')})",
            },
            { icon = "󰒲 ", key = "L", desc = "Lazy", action = ":Lazy", enabled = package.loaded.lazy ~= nil },
            {
              icon = " ",
              key = "M",
              desc = "Mason",
              action = ":Mason",
            },
            { icon = " ", key = "q", desc = "Quit", action = ":qa" },
          },
        },
      },
      explorer = {},
      input = {},
      notifier = {},
      picker = {
        prompt = " ",
        icons = {
          files = { enabled = false },
          tree = { last = "╰╴" },
        },
        layout = {
          preset = function()
            return vim.o.columns >= 120 and "default" or "select"
          end,
        },
      },
      statuscolumn = {},
      styles = {},
      words = {},
    },
    keys = {
      {
        "<leader><space>",
        function()
          Snacks.picker.smart()
        end,
        desc = "Smart Find Files",
      },
      {
        "<leader>n",
        function()
          Snacks.picker.notifications()
        end,
        desc = "Notification History",
      },
      {
        "<leader>e",
        function()
          Snacks.explorer()
        end,
        desc = "File Explorer",
      },
      {
        "<leader>q",
        function()
          Snacks.bufdelete()
        end,
        desc = "Delete Buffer",
      },
      {
        "<leader>fb",
        function()
          Snacks.picker.buffers()
        end,
        desc = "Buffers",
      },
      {
        "<leader>fc",
        function()
          Snacks.picker.files({ cwd = vim.fn.stdpath("config")[0] })
        end,
        desc = "Find Config File",
      },
      {
        "<leader>ff",
        function()
          Snacks.picker.files()
        end,
        desc = "Find Files",
      },
      {
        "<leader>fg",
        function()
          Snacks.picker.git_files()
        end,
        desc = "Find Git Files",
      },
      {
        "<leader>fp",
        function()
          Snacks.picker.projects()
        end,
        desc = "Projects",
      },
      {
        "<leader>fr",
        function()
          Snacks.picker.recent()
        end,
        desc = "Recent",
      },
      {
        "<leader>sb",
        function()
          Snacks.picker.lines()
        end,
        desc = "Buffer Lines",
      },
      {
        "<leader>sB",
        function()
          Snacks.picker.grep_buffers()
        end,
        desc = "Grep Open Buffers",
      },
      {
        "<leader>sg",
        function()
          Snacks.picker.grep()
        end,
        desc = "Grep",
      },
      {
        "<leader>sw",
        function()
          Snacks.picker.grep_word()
        end,
        desc = "Visual selection or word",
        mode = { "n", "x" },
      },
      {
        '<leader>s"',
        function()
          Snacks.picker.registers()
        end,
        desc = "Registers",
      },
      {
        "<leader>s/",
        function()
          Snacks.picker.search_history()
        end,
        desc = "Search History",
      },
      {
        "<leader>sa",
        function()
          Snacks.picker.autocmds()
        end,
        desc = "Autocmds",
      },
      {
        "<leader>sc",
        function()
          Snacks.picker.command_history()
        end,
        desc = "Command History",
      },
      {
        "<leader>sC",
        function()
          Snacks.picker.commands()
        end,
        desc = "Commands",
      },
      {
        "<leader>sD",
        function()
          Snacks.picker.diagnostics()
        end,
        desc = "Diagnostics",
      },
      {
        "<leader>sd",
        function()
          Snacks.picker.diagnostics_buffer()
        end,
        desc = "Buffer Diagnostics",
      },
      {
        "<leader>sh",
        function()
          Snacks.picker.help()
        end,
        desc = "Help Pages",
      },
      {
        "<leader>sH",
        function()
          Snacks.picker.highlights()
        end,
        desc = "Highlights",
      },
      {
        "<leader>sj",
        function()
          Snacks.picker.jumps()
        end,
        desc = "Jumps",
      },
      {
        "<leader>sk",
        function()
          Snacks.picker.keymaps()
        end,
        desc = "Keymaps",
      },
      {
        "<leader>sl",
        function()
          Snacks.picker.loclist()
        end,
        desc = "Location List",
      },
      {
        "<leader>sm",
        function()
          Snacks.picker.marks()
        end,
        desc = "Marks",
      },
      {
        "<leader>sM",
        function()
          Snacks.picker.man()
        end,
        desc = "Man Pages",
      },
      {
        "<leader>sp",
        function()
          Snacks.picker.lazy()
        end,
        desc = "Search for Plugin Spec",
      },
      {
        "<leader>sq",
        function()
          Snacks.picker.qflist()
        end,
        desc = "Quickfix List",
      },
      {
        "<leader>sR",
        function()
          Snacks.picker.resume()
        end,
        desc = "Resume",
      },
      {
        "<leader>su",
        function()
          Snacks.picker.undo()
        end,
        desc = "Undo History",
      },
      {
        "]]",
        function()
          Snacks.words.jump(1, true)
        end,
        desc = "Next reference",
      },
      {
        "[[",
        function()
          Snacks.words.jump(-1, true)
        end,
        desc = "Previous reference",
      },
    },
  },
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    opts = {
      keys = {
        scroll_down = "<c-f>",
        scroll_up = "<c-b>",
      },
      icons = { mappings = false },
      preset = "helix",
      show_help = false,
      win = { border = "rounded" },
      layout = {
        height = { max = 10 },
      },
      spec = {
        {
          mode = { "n", "v" },
          { "<leader>T", group = "+terminal" },
          { "<leader>c", group = "+env" },
          { "<leader>d", group = "+diagnostics" },
          { "<leader>f", group = "+find" },
          { "<leader>g", group = "+act" },
          { "<leader>s", group = "+search" },
          { "<leader>t", group = "+test" },
          { "<leader>v", group = "+vcs" },
          { "<leader>vg", group = "+act" },
        },
      },
    },
  },
  {
    "folke/trouble.nvim",
    keys = {
      {
        "<leader>dd",
        "<cmd>Trouble diagnostics toggle filter.buf=0<cr>",
        desc = "Buffer Diagnostics (Trouble)",
      },
      {
        "<leader>dD",
        "<cmd>Trouble diagnostics toggle<cr>",
        desc = "Diagnostics (Trouble)",
      },
      {
        "<leader>dl",
        "<cmd>Trouble loclist toggle<cr>",
        desc = "Location List (Trouble)",
      },
      {
        "<leader>dq",
        "<cmd>Trouble qflist toggle<cr>",
        desc = "Quickfix List (Trouble)",
      },
    },
    opts = {
      icons = {
        folder_open = " 📂",
        folder_closed = " 📁",
        indent = {
          last = "╰╴",
          fold_closed = "+",
          fold_open = "-",
        },
      },
    },
  },
  {
    "akinsho/bufferline.nvim",
    event = "VeryLazy",
    opts = {
      options = {
        always_show_bufferline = false,
        separator_style = "thin",
        diagnostics = "nvim_lsp",
        show_buffer_close_icons = false,
        show_buffer_icons = false,
        show_close_icon = false,
        offsets = {
          {
            filetype = "snacks_layout_box",
            separator = "│",
          },
        },
      },
    },
  },
  {
    "nvim-lualine/lualine.nvim",
    event = "VeryLazy",
    opts = function()
      local function macro_recording()
        local recording_register = vim.fn.reg_recording()

        if recording_register == "" then
          return ""
        else
          return "Recording @" .. recording_register
        end
      end

      local function lsp_clients()
        local buf = vim.api.nvim_get_current_buf()
        local client_names = {}

        for _, client in pairs(vim.lsp.get_clients() or {}) do
          if vim.lsp.buf_is_attached(buf, client.id) then
            table.insert(client_names, client.name)
          end
        end

        if vim.tbl_isempty(client_names) then
          return ""
        end

        table.sort(client_names)

        return "⚡ " .. table.concat(client_names, ", ")
      end

      local function lsp_actions()
        local lightbulb = require("nvim-lightbulb")

        return lightbulb.get_status_text()
      end

      return {
        options = {
          component_separators = { left = "╱", right = "╱" },
          disabled_filetypes = {
            "lazy",
            "snacks_dashboard",
            "trouble",
          },
          globalstatus = vim.opt.laststatus == 3,
          icons_enabled = false,
          section_separators = { left = "", right = "" },
          theme = "auto",
        },
        sections = {
          lualine_b = {
            macro_recording,
            "branch",
            "diff",
            {
              "diagnostics",
              sources = {
                "nvim_lsp",
                "nvim_diagnostic",
              },
              symbols = {
                error = "✖ ",
                warn = "▲ ",
                info = "● ",
                hint = "🞷 ",
              },
            },
          },
          lualine_c = {
            { "filename", path = 4 },
          },
          lualine_x = {
            { lsp_actions },
            { lsp_clients },
          },
          lualine_y = {
            "searchcount",
            "selectioncount",
            "progress",
          },
        },
        extensions = { "trouble" },
      }
    end,
  },
}
