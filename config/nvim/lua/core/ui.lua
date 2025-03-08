-- Make window backgrounds transparent
vim.api.nvim_set_hl(0, "FloatBorder", { bg = "NONE" })
vim.api.nvim_set_hl(0, "NormalFloat", { bg = "NONE" })

-- Make LSP windows use rounded borders
for _, handler in pairs({ "textDocument/hover", "textDocument/signatureHelp" }) do
  vim.lsp.handlers[handler] = vim.lsp.with(vim.lsp.handlers.hover, {
    border = "rounded",
  })
end

-- Customize diagnostics appearance
vim.diagnostic.config({
  float = { border = "rounded" },
  severity_sort = true,
  signs = true,
  underline = true,
  update_in_insert = false,
  virtual_text = true,
})

for severity, text in pairs({
  Error = "✖ ",
  Warn = "▲ ",
  Info = "● ",
  Hint = "🞷 ",
}) do
  -- set severity symbol
  local sign = "DiagnosticSign" .. severity
  vim.fn.sign_define(sign, { text = text, texthl = sign, numhl = sign })

  -- use undercurl instead of an underline
  local underline = "DiagnosticUnderline" .. severity
  local highlight = vim.api.nvim_get_hl(0, { name = underline })
  vim.api.nvim_set_hl(0, underline, vim.tbl_extend("force", highlight, { undercurl = true }))
end
