return {
      "nvim-lualine/lualine.nvim",
      event = "VeryLazy",
      opts = function()
        return {
          options = {
            icons_enabled = false,
            theme = "auto",
            section_separators = { left = "", right = "" },
            component_separators = { left = "", right = "" },
            disabled_filetypes = { "lazy", "NvimTree" }
          },
          sections = {
            lualine_b = {
              "branch",
              "diff",
              {
                "diagnostics",
                symbols = {
                  error = "🚫 ",
                  warn = "⚠️  ",
                  info = "ℹ️  ",
                  hint = "💡 ",
                },
              },
            },
            lualine_c = {
              { "filename", path = 3, },
            },
            lualine_y = {
              "searchcount",
              "selectioncount",
              "progress",
            },
          },
        }
      end,
    }