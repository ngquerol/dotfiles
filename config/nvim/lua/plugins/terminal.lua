return {
  "folke/snacks.nvim",
  opts = {
    terminal = {},
  },
  keys = {
    {
      "<leader>Tt",
      function()
        Snacks.terminal()
      end,
      desc = "Toggle terminal",
    },
    {
      "<leader>Tr",
      function()
        Snacks.input({ prompt = "Run command" }, function(cmd)
          if cmd then
            Snacks.terminal.open(cmd, { interactive = false })
          end
        end)
      end,
      desc = "Run command",
    },
  },
}
