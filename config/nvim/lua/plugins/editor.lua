return {
  {
    "folke/snacks.nvim",
    opts = {
      bigfile = {},
      quickfile = {},
    },
  },
  {
    "nmac427/guess-indent.nvim",
    event = { "BufReadPost", "BufWritePost", "BufNewFile" },
    opts = {},
  },
  {
    "echasnovski/mini.surround",
    event = { "BufReadPost", "BufWritePost", "BufNewFile" },
    opts = {},
  },
  {
    "echasnovski/mini.ai",
    event = { "BufReadPost", "BufWritePost", "BufNewFile" },
    dependencies = { "nvim-treesitter/nvim-treesitter" },
    opts = function()
      local ai = require("mini.ai")

      return {
        custom_textobjects = {
          a = ai.gen_spec.treesitter({
            a = { "@assignment.outer", "@parameter.outer" },
            i = { "@assignment.rhs", "@parameter.inner" },
          }),
          c = ai.gen_spec.treesitter({ a = "@call.outer", i = "@call.inner" }),
          C = ai.gen_spec.treesitter({ a = "@comment.outer", i = "@comment.inner" }),
          f = ai.gen_spec.treesitter({ a = "@function.outer", i = "@function.inner" }),
          o = ai.gen_spec.treesitter({
            a = { "@conditional.outer", "@loop.outer", "@block.outer", "@class.outer" },
            i = { "@conditional.inner", "@loop.inner", "@block.inner", "@class.inner" },
          }),
          t = { "<([%p%w]-)%f[^<%w][^<>]->.-</%0>", "^<.->().*()</[^/]->$" },
        },
        search_method = "cover_or_nearest",
        silent = true,
      }
    end,
  },
  {
    "echasnovski/mini.operators",
    event = { "BufReadPost", "BufWritePost", "BufNewFile" },
    opts = {
      evaluate = { prefix = "" },
      multiply = { prefix = "" },
      replace = { prefix = "g/" },
    },
  },
  {
    "echasnovski/mini.pairs",
    event = { "BufReadPost", "BufWritePost", "BufNewFile" },
    opts = {
      modes = {
        insert = true,
        command = true,
        terminal = false,
      },
      skip_next = [=[[%w%%%'%[%"%.%`%$]]=],
      skip_ts = { "string" },
      skip_unbalanced = true,
      markdown = true,
    },
  },
  {
    "ethanholz/nvim-lastplace",
    event = "BufReadPre",
    opts = {
      lastplace_ignore_buftype = {
        "quickfix",
        "nofile",
        "help",
      },
      lastplace_ignore_filetype = {
        "gitcommit",
        "gitrebase",
        "svn",
        "hgcommit",
      },
      lastplace_open_folds = true,
    },
  },
  {
    "Wansmer/treesj",
    event = { "BufReadPost", "BufWritePost", "BufNewFile" },
    keys = {
      { "gS", "<cmd>TSJToggle<cr>", desc = "Split/Join lines" },
    },
    opts = {
      use_default_keymaps = false,
      max_join_length = 200,
    },
  },
  {
    "monaqa/dial.nvim",
    event = { "BufReadPost", "BufWritePost", "BufNewFile" },
    keys = {
      {
        "<C-a>",
        function()
          return require("dial.map").inc_normal()
        end,
        expr = true,
        desc = "Increment",
      },
      {
        "<C-x>",
        function()
          return require("dial.map").dec_normal()
        end,
        expr = true,
        desc = "Decrement",
      },
    },
    config = function()
      local augend = require("dial.augend")
      require("dial.config").augends:register_group({
        default = {
          augend.integer.alias.binary,
          augend.integer.alias.decimal_int,
          augend.integer.alias.hex,
          augend.integer.alias.octal,
          augend.date.alias["%Y/%m/%d"],
          augend.date.alias["%Y-%m-%d"],
          augend.date.alias["%d/%m/%Y"],
          augend.date.alias["%H:%M:%S"],
          augend.constant.alias.bool,
          augend.semver.alias.semver,
          augend.constant.new({
            elements = { "True", "False" },
          }),
        },
      })
    end,
  },
}
