local capabilities = require("cmp_nvim_lsp").default_capabilities()

vim.lsp.config("djls", {
	capabilities = capabilities,
	cmd = { "djls", "serve" },
	filetypes = { "htmldjango", "html", "python" },
	root_markers = { "manage.py", "pyproject.toml", ".git" },
})
vim.lsp.enable("djls")
