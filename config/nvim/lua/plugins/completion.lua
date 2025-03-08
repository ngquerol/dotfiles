local window_opts = {
  border = "rounded",
  winhighlight = "Normal:CmpPmenu,FloatBorder:CmpBorder,CursorLine:PmenuSel,Search:None",
}

return {
  {
    "saghen/blink.cmp",
    event = "InsertEnter",
    dependencies = { "rafamadriz/friendly-snippets" },
    opts = {
      appearance = { nerd_font_variant = "normal" },
      completion = {
        documentation = {
          auto_show = false,
          auto_show_delay_ms = 500,
          window = window_opts,
        },
        ghost_text = { enabled = true },
        list = {
          max_items = 50,
          selection = { auto_insert = false },
        },
        menu = {
          draw = {
            columns = {
              { "kind_icon" },
              { "label", "label_description", gap = 1 },
              { "kind" },
            },
          },
          border = window_opts.border,
          winhighlight = window_opts.winhighlight,
        },
      },
      keymap = {
        preset = "enter",
        ["<Tab>"] = { "select_next", "snippet_forward", "fallback" },
        ["<S-Tab>"] = { "select_prev", "snippet_backward", "fallback" },
      },
      signature = {
        enabled = true,
        window = window_opts,
      },
      sources = {
        default = { "lsp", "path", "snippets", "buffer" },
      },
    },
    opts_extend = { "sources.default" },
  },
  {
    "saghen/blink.cmp",
    dependencies = {
      {
        "milanglacier/minuet-ai.nvim",
        enabled = vim.env.GEMINI_API_KEY,
        dependencies = { "nvim-lua/plenary.nvim" },
        opts = {
          blink = { enable_auto_complete = false },
          provider = "gemini",
          provider_options = {
            gemini = {
              system = {
                prompt = [[
                  You are the backend of an AI-powered code completion engine. Your task is to
                  provide code suggestions based on the user's input. The user's code will be
                  enclosed in markers:

                  - `<contextAfterCursor>`: Code context after the cursor
                  - `<cursorPosition>`: Current cursor location
                  - `<contextBeforeCursor>`: Code context before the cursor
                ]],
              },
              chat_input = {
                template = "{{{language}}}\n{{{tab}}}\n<contextBeforeCursor>\n{{{context_before_cursor}}}<cursorPosition>\n<contextAfterCursor>\n{{{context_after_cursor}}}",
              },
              optional = {
                generationConfig = {
                  maxOutputTokens = 256,
                  topP = 0.9,
                },
                safetySettings = {
                  {
                    category = "HARM_CATEGORY_DANGEROUS_CONTENT",
                    threshold = "BLOCK_NONE",
                  },
                  {
                    category = "HARM_CATEGORY_HATE_SPEECH",
                    threshold = "BLOCK_NONE",
                  },
                  {
                    category = "HARM_CATEGORY_HARASSMENT",
                    threshold = "BLOCK_NONE",
                  },
                  {
                    category = "HARM_CATEGORY_SEXUALLY_EXPLICIT",
                    threshold = "BLOCK_NONE",
                  },
                },
              },
            },
          },
        },
      },
    },
    opts = {
      keymap = {
        ["<C-y>"] = {
          function(cmp)
            cmp.show({
              providers = { "minuet" },
              -- force documentation popup to preview suggestions,
              -- even if auto show is false
              callback = function()
                vim.schedule(function()
                  cmp.show_documentation()
                end)
              end,
            })
          end,
        },
      },
      sources = {
        providers = {
          minuet = {
            name = "minuet",
            module = "minuet.blink",
            score_offset = 8,
          },
        },
      },
    },
  },
}
