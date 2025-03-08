return {
  "stevearc/conform.nvim",
  dependencies = { "mason.nvim" },
  event = { "BufWritePre" },
  cmd = { "ConformInfo" },
  keys = {
    {
      "<leader>gf",
      function()
        require("conform").format()
      end,
      mode = { "n", "v" },
      desc = "format buffer",
    },
  },
  opts = {
    formatters_by_ft = {
      lua = { "stylua" },
      fish = { "fish_indent" },
      sh = { "shfmt" },
    },
    format = {
      lsp_format = "fallback",
      timeout_ms = 3000,
    },
    format_on_save = {
      lsp_format = "fallback",
      timeout_ms = 500,
    },
  },
  init = function()
    vim.o.formatexpr = "v:lua.require'conform'.formatexpr()"
  end,
}
