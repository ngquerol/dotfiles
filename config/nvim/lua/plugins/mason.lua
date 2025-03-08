return {
  "williamboman/mason.nvim",
  event = "VeryLazy",
  cmd = "Mason",
  keys = { { "<leader>M", "<cmd>Mason<cr>", desc = "Mason" } },
  build = ":MasonUpdate",
  opts_extend = { "ensure_installed" },
  opts = {
    ensure_installed = { "stylua", "shfmt" },
    ui = {
      border = "rounded",
      icons = {
        package_installed = "✓",
        package_pending = "➜",
        package_uninstalled = "✗",
      },
    },
  },
  config = function(_, opts)
    local mason = require("mason")
    local registry = require("mason-registry")

    mason.setup(opts)

    registry.refresh(function()
      for _, name in ipairs(opts.ensure_installed) do
        local package = registry.get_package(name)
        if not registry.is_installed(name) then
          package:install()
        end
      end
    end)
  end,
}
