return {
  "nvim-neotest/neotest",
  dependencies = {
    "nvim-neotest/nvim-nio",
    "nvim-treesitter/nvim-treesitter",
    "nvim-lua/plenary.nvim",
  },
  keys = {
    {
      "<leader>tt",
      function()
        require("neotest").run.run(vim.fn.expand("%"))
      end,
      desc = "Run File",
    },
    {
      "<leader>tT",
      function()
        require("neotest").run.run(vim.uv.cwd())
      end,
      desc = "Run All Test Files",
    },
    {
      "<leader>tr",
      function()
        require("neotest").run.run()
      end,
      desc = "Run Nearest",
    },
    {
      "<leader>tl",
      function()
        require("neotest").run.run_last()
      end,
      desc = "Run Last",
    },
    {
      "<leader>ts",
      function()
        require("neotest").summary.toggle()
      end,
      desc = "Toggle Summary",
    },
    {
      "<leader>to",
      function()
        require("neotest").output.open({ enter = true, auto_close = true })
      end,
      desc = "Show Output",
    },
    {
      "<leader>tO",
      function()
        require("neotest").output_panel.toggle()
      end,
      desc = "Toggle Output Panel",
    },
    {
      "<leader>tS",
      function()
        require("neotest").run.stop()
      end,
      desc = "Stop",
    },
  },
  opts = {
    output = { open_on_run = true },
    status = {
      enabled = true,
      signs = false,
      virtual_text = true,
    },
    icons = {
      child_indent = "│",
      child_prefix = "├",
      collapsed = "─",
      expanded = "╮",
      failed = "✗",
      final_child_indent = " ",
      final_child_prefix = "╰",
      non_collapsible = "─",
      notify = "",
      passed = "✔",
      running = "⏳",
      running_animated = { "/", "|", "\\", "-", "/", "|", "\\", "-" },
      skipped = "⋯",
      unknown = "?",
      watching = "👁",
    },
  },
  config = function(_, opts)
    local adapters = {}
    for name, config in pairs(opts.adapters or {}) do
      local adapter = require(name)
      if adapter.setup then
        adapter.setup(config)
      elseif type(adapter) == "table" then
        local meta = getmetatable(adapter)
        if meta and meta.__call then
          adapter(config)
        end
      end
      adapters[#adapters + 1] = adapter
    end
    opts.adapters = adapters

    require("neotest").setup(opts)
  end,
}
